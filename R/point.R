#' Identify the polygons containing locations in grains of connectivity (GOC) tessellations
#'
#' @description
#' Identify the polygon containing a location at multiple scales.
#'
#' @param x       A \code{goc} object produced by \code{\link{GOC}}.
#'
#' @param coords  A two column matrix or a \code{\link{SpatialPoints}} object giving
#'                the coordinates of points of interest.
#'
#' @param ...     Additional arguments (not used).
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
#'   all points of interest (defined by O'Brien \emph{et al.}, 2006);}
#'
#'   \item{\code{pointECSCore}}{is the same for the core area of patches.}
#' }
#'
#' @note See \code{\link{MPG}} for warning related to areal measurements.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs:
#' Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing methods
#' for scaling resistance surfaces. Landscape Ecology 28:1269-1291.
#'
#' Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape connectivity
#' on animal distribution: a functional grain approach. Ecography 36:1004-1016.
#'
#' Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape connectivity:
#' a guide to construction, analysis, and application for conservation.
#' Biological Conservation 144:44-55.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis
#' at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#' O'Brien, D., M. Manseau, A. Fall, and M.-J. Fortin. (2006) Testing the importance of
#' spatial configuration of winter habitat for woodland caribou: An application of graph theory.
#' Biological Conservation 130:70-83.
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom raster cellFromXY
#' @include classes.R
#' @rdname point
#' @seealso \code{\link{GOC}}, \code{\link{distance}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_point.R
#'
setGeneric("point", function(x, ...) {
  standardGeneric("point")
})

#' @export
#' @rdname point
setMethod(
  "point",
  signature = "goc",
  definition = function(x, coords, ...) {
    if (is.null(dim(coords)) & !inherits(coords, "SpatialPoints")) {
      coords <- t(as.matrix(coords))
    }

    if (!inherits(coords, "SpatialPoints") && (ncol(coords) != 2)) {
      stop("coords must be a SpatialPoints object or a matrix of two columns",
           " giving X and Y coordinates")
    }

    if (!inherits(coords, "SpatialPoints")) {
      coords <- SpatialPoints(coords)
    }

    ## Remove points that fall in NA locations
    cellPoints <- cellFromXY(x@voronoi, coords)
    if (suppressWarnings(sum(is.na(x@voronoi[cellPoints]))) > 0) {
      cellPoints <- suppressWarnings(cellPoints[!is.na(x@voronoi[cellPoints])])
      stop("there are coords that are not defined on the raster.")
    }

    grainPoints <- matrix(NA, nrow = length(cellPoints), ncol = length(x@th))
    totalPatchAreaPoints <- grainPoints
    totalCoreAreaPoints <- grainPoints

    for (iThresh in 1:length(x@th)) {
      if (is_igraph(x@th[[iThresh]]$goc)) {
        threshGraph <- x@th[[iThresh]]$goc

        ## Produce patchId and patchArea lookup tables with polygonId as the index
        patchIdLookup <-  matrix(0, 1, 2)
        for (i in 1:length(V(threshGraph)$polygonId)) {
          patchIdLookup <- rbind(
            patchIdLookup,
            cbind(as.integer(V(threshGraph)$polygonId[i]),
                  as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", "))))
          )
        }
        patchIdLookup <- patchIdLookup[2:nrow(patchIdLookup), ]
        patchAreaLookup <- cbind(V(threshGraph)$polygonId,
                                 V(threshGraph)$totalPatchArea,
                                 V(threshGraph)$totalPatchEdgeArea,
                                 V(threshGraph)$totalCoreArea)

        ## Faster method which references the cells from the stored voronoi raster
        ## and uses the graph vertex record to determine the polygonId
        grainPoints[, iThresh] <- as.numeric(sapply(x@voronoi[cellPoints], function(z) {
          patchIdLookup[patchIdLookup[, 2] == na.omit(z), 1]
        }))

        totalPatchAreaPoints[, iThresh] <- as.numeric(sapply(grainPoints[, iThresh], function(z) {
          if (is.na(z)) warning("values of 'coords' correspond to cells with value 'NA'.")
          patchAreaLookup[patchAreaLookup[, 1] == na.omit(z), 2]
        }))

        totalCoreAreaPoints[, iThresh] <- as.numeric(sapply(grainPoints[, iThresh], function(z) {
          ## warning provided above
          patchAreaLookup[patchAreaLookup[, 1] == na.omit(z), 4]
        }))
      }
    }

    results <- list()
    results$pointPolygon <- grainPoints
    results$pointTotalPatchArea <- totalPatchAreaPoints
    results$pointTotalCoreArea <- totalCoreAreaPoints
    results$pointECS <- apply(totalPatchAreaPoints, 2, mean, na.rm = TRUE)
    results$pointECSCore <- apply(totalCoreAreaPoints, 2, mean, na.rm = TRUE)
    return(results)
})
