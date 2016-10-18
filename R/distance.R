#' Find the grains of connectivity network distance
#'
#' Find the shortest network distance between pairs of points using the GOC graph.
#' This can be used as an effective distance for landscape connectivity assessments.
#'
#' @param ...  Additional arguments.
#'
#' @export
#'
distance <- function(x, ...) UseMethod("distance")



#' @param x        A \code{GOC} object produced by \code{\link{GOC}}.
#'
#' @param  coords  A two column matrix or a \code{\link{SpatialPoints}} object
#'                 giving the coordinates of points of interest.
#'
#' @param  weight  The GOC graph link weight to use in calculating the distance.
#'                 Please see Details for explanation.
#'
#' @return  A list object giving a distance matrix for each threshold in the \code{GOC} object.
#' Distance matrices give the pairwise grains of connectivity network distances between sampling locations.
#' Matrix indices correspond to rows in the \code{coords} matrix.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom igraph distances 'E<-' edge_attr is_igraph
#' @rdname distance
#' @seealso  \code{\link{GOC}}, \code{\link{point}}
#'
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassifification
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
#' ## Find the GOC network distance matrices between these points
#' ## for each of the 5 grains of connectivity
#' tinyDist <- distance(tinyPatchGOC, loc)
#' }
#'
distance.goc <- function(x, ..., coords, weight = "meanWeight") {
  if ((is.null(dim(coords))) & !inherits(coords, "SpatialPoints")) {
    coords <- t(as.matrix(coords))
  }

  if (!inherits(coords, "SpatialPoints") && (dim(coords)[2] != 2)) {
    stop("grainscape:  coords must be a SpatialPoints object or a matrix of two columns giving X and Y coordinates", call. = FALSE)
  }

  if (!(weight %in% names(edge_attr(x$th[[1]]$goc)))) {
    stop("grainscape:  link weight attribute with this name doesn't exist in GOC object", call. = FALSE)
  }

  whichGrain <- point(x, coords)$pointPolygon

  results <- list()
  results$metaData <- x$metaData
  results$th <- vector("list", ncol(whichGrain))

  for (iThresh in 1:ncol(whichGrain)) {
    threshGraph <- x$th[[iThresh]]$goc

    if (is_igraph(threshGraph)) {
      E(threshGraph)$weight <- edge_attr(threshGraph, weight)
      vertices <- sapply(whichGrain[, iThresh], function(z) which(V(threshGraph)$polygonId == z))
      results$th[[iThresh]]$grainD <- distances(threshGraph, v = vertices)[, vertices]
    } else {
      results$th[[iThresh]] <- NA
    }
  }
  return(results)
}
