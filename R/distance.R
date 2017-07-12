#' Find the grains of connectivity network distance
#'
#' Find the shortest network distance between pairs of points using the GOC graph.
#' This can be used as an effective distance for landscape connectivity assessments.
#'
#' @param x        A \code{goc} object produced by \code{\link{GOC}}.
#'
#' @param  y       A two column matrix or a \code{\link{SpatialPoints}} object
#'                 giving the coordinates of points of interest.
#'
#' @param ...      Additional arguments (not used).
#'
#' @param  weight  The GOC graph link weight to use in calculating the distance.
#'                 Please see Details for explanation.
#'
#' @return  A list object giving a distance matrix for each threshold in the \code{GOC} object.
#' Distance matrices give the pairwise grains of connectivity network distances between sampling locations.
#' Matrix indices correspond to rows in the coords matrix (\code{y}).
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing methods for scaling resistance surfaces. Landscape Ecology 28:1269-1291.
#'
#' Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape connectivity on animal distribution: a functional grain approach. Ecography 36:1004-1016.
#'
#' Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape connectivity: a guide to construction, analysis, and application for conservation. Biological Conservation 144:44-55.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern and Alex Chubaty
#' @docType methods
#' @export
#' @include classes.R
#' @rdname distance
#' @seealso  \code{\link{GOC}}, \code{\link{point}}
#'
#' @examples
#' \dontrun{
#' library(raster)
#'
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
setGeneric("distance", function(x, y, ...) {
    raster::distance(x, y, ...)
})

#' @export
#' @rdname distance
setMethod(
  "distance",
  signature = c(x = "goc", y = "SpatialPoints"),
  definition = function(x, y, weight = "meanWeight", ...) {
    if (!(weight %in% names(edge_attr(x@th[[1]]$goc)))) {
      stop("grainscape:  link weight attribute with this name doesn't exist in GOC object", call. = FALSE)
    }

    whichGrain <- point(x, y)$pointPolygon

    results <- list()
    results$th <- vector("list", ncol(whichGrain))

    for (iThresh in 1:ncol(whichGrain)) {
      threshGraph <- x@th[[iThresh]]$goc

      if (is_igraph(threshGraph)) {
        E(threshGraph)$weight <- edge_attr(threshGraph, weight)
        vertices <- sapply(whichGrain[, iThresh], function(z) which(V(threshGraph)$polygonId == z))
        results$th[[iThresh]]$grainD <- distances(threshGraph, v = vertices)[, vertices]
      } else {
        results$th[[iThresh]] <- NA
      }
    }
    return(results)
})

#' @importFrom sp SpatialPoints
#' @export
#' @rdname distance
setMethod(
  "distance",
  signature = c(x = "goc", y = "matrix"),
  definition = function(x, y, weight = "meanWeight", ...) {
    if (ncol(y) != 2) {
      stop("grainscape:  y must be a matrix of two columns giving X and Y coordinates", call. = FALSE)
    }

    distance(x, SpatialPoints(y), weight, ...)
})

#' @export
#' @rdname distance
setMethod(
  "distance",
  signature = c(x = "goc", y = "numeric"),
  definition = function(x, y, weight = "meanWeight", ...) {
    distance(x, t(as.matrix(y)), weight, ...)
})

