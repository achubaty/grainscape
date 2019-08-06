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
#' Distance matrices give the pairwise grains of connectivity network distances
#' between sampling locations.
#' Matrix indices correspond to rows in the coordinates matrix (\code{y}).
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
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @include classes.R
#' @rdname distance
#' @seealso  \code{\link{GOC}}, \code{\link{point}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_distance.R
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
      stop("link weight attribute with this name doesn't exist in GOC object")
    }

    whichGrain <- point(x, y)$pointPolygon

    results <- list()
    results$th <- vector("list", ncol(whichGrain))

    for (iThresh in 1:ncol(whichGrain)) {
      threshGraph <- x@th[[iThresh]]$goc

      if (is_igraph(threshGraph)) {
        E(threshGraph)$weight <- edge_attr(threshGraph, weight)
        vertices <- sapply(whichGrain[, iThresh], function(z) {
          if (is.na(z)) NA_integer_ else which(V(threshGraph)$polygonId == z)
        })
        results$th[[iThresh]]$grainD <- distances(threshGraph,
                                                  v = na.omit(vertices))[, na.omit(vertices)]
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
      stop("y must be a matrix of two columns giving X and Y coordinates")
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
