#' Identify the polygons containing locations in grains of connectivity (GOC) tessellations
#'
#' @description
#' Identify the polygon containing a location at multiple scales.
#'
#' @param ...  Additional arguments.
#'
#' @export
#'
point <- function(x, coords) UseMethod("point")



#' @param x       A \code{goc} object produced by \code{\link{GOC}}.
#'
#' @param coords  A two column matrix or a \code{\link{SpatialPoints}} object giving
#'                the coordinates of points of interest.
#'
#' @return A list with elements:
#'
#' \describe{
#'   \item{\code{pointPolygon}}{a matrix with elements giving the id of the
#'   polygon from the \code{goc}, where rows give points of interest and
#'   columns give thresholds;}
#'
#'   \item{\code{pointTotalPatchArea}}{is a matrix with elements giving the area
#'   of patches in a polygon (in cell counts), where rows give points of and
#'   columns give thresholds;}
#'
#'   \item{\code{pointTotalCoreArea}}{the same for core area of patches;}
#'
#'   \item{\code{pointECS}}{gives the patch area (in cell counts) averaged for
#'   all points of interest (defined by O'Brien et al. 2006);}
#'
#'   \item{\code{pointECSCore}}{is the same for the core area of patches.}
#' }
#'
#' @note See \code{\link{MPG}} for warning related to areal measurements.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.
#'
#' O'Brien, D., M. Manseau, A. Fall, and M.-J. Fortin. (2006) Testing the importance of spatial configuration of winter habitat for woodland caribou: An application of graph theory. Biological Conservation 130:70-83.
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom igraph is_igraph V
#' @importFrom raster cellFromXY
#' @rdname point
#' @seealso \code{\link{GOC}}, \code{\link{distance}}
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Three sets of coordinates in the study area
#' loc <- cbind(c(30, 60, 90), c(30, 60, 90))
#'
#' ## Find the GOC polygon containing these three locations
#' ## for each of the 5 grains of connectivity
#' tinyPts <- point(tinyPatchGOC, loc)
#' }
#'
point.goc <- function(x, coords) {
  if (is.null(dim(coords)) & !inherits(coords, "SpatialPoints")) {
    coords <- t(as.matrix(coords))
  }

  if (!inherits(coords, "SpatialPoints") && (dim(coords)[2] != 2)) {
    stop("grainscape:  coords must be a SpatialPoints object or a matrix of two columns giving X and Y coordinates", call. = FALSE)
  }

  if (!inherits(coords, "SpatialPoints")) {
    coords <- SpatialPoints(coords)
  }

  ## Remove points that fall in NA locations
  cellPoints <- cellFromXY(x$voronoi, coords)
  if (suppressWarnings(sum(is.na(x$voronoi[cellPoints]))) > 0) {
    cellPoints <- suppressWarnings(cellPoints[!is.na(x$voronoi[cellPoints])])
    stop("grainscape:  there are coords that are not defined on the raster.\n", call. = FALSE)
  }

  grainPoints <- matrix(NA, nrow = length(cellPoints), ncol = length(x$th))
  totalPatchAreaPoints <- grainPoints
  totalCoreAreaPoints <- grainPoints

  for (iThresh in 1:length(x$th)) {
    if (is_igraph(x$th[[iThresh]]$goc)) {
      threshGraph <- x$th[[iThresh]]$goc

      ## Produce patchId and patchArea lookup tables with polygonId as the index
      patchIdLookup <-  matrix(0, 1, 2)
      for (i in 1:length(V(threshGraph)$polygonId)) {
        patchIdLookup <- rbind(patchIdLookup,
                               cbind(as.integer(V(threshGraph)$polygonId[i]),
                                     as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", ")))))
      }
      patchIdLookup <- patchIdLookup[2:nrow(patchIdLookup), ]
      patchAreaLookup <- cbind(V(threshGraph)$polygonId,
                               V(threshGraph)$totalPatchArea,
                               V(threshGraph)$totalPatchEdgeArea,
                               V(threshGraph)$totalCoreArea)

      ## Faster method which references the cells from the stored voronoi raster
      ## and uses the graph vertex record to determine the polygonId

      grainPoints[, iThresh] <- as.numeric(sapply(x$voronoi[cellPoints], function(z) {
        patchIdLookup[patchIdLookup[, 2] == z, 1]
      }))

      totalPatchAreaPoints[, iThresh] <- as.numeric(sapply(grainPoints[, iThresh], function(z) {
        patchAreaLookup[patchAreaLookup[, 1] == z, 2]
      }))
      totalCoreAreaPoints[, iThresh] <- as.numeric(sapply(grainPoints[, iThresh], function(z) {
        patchAreaLookup[patchAreaLookup[, 1] == z, 4]
      }))
    }
  }

  results <- list()
  results$pointPolygon <- grainPoints
  results$pointTotalPatchArea <- totalPatchAreaPoints
  results$pointTotalCoreArea <- totalCoreAreaPoints
  results$pointECS <- apply(totalPatchAreaPoints, 2, mean)
  results$pointECSCore <- apply(totalCoreAreaPoints, 2, mean)
  return(results)
}