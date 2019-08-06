#' Extract a grain of connectivity (GOC) tessellation at a given scale
#'
#' @description
#' Extract a grain (i.e. a scaled version of a Voronoi tessellation) from a GOC model.
#'
#' @param x   A \code{goc} object created by \code{\link{GOC}}.
#'
#' @param whichThresh  Integer giving the grain threshold to extract.
#'                     This is the index of the threshold extracted by \code{\link{GOC}}.
#'
#' @param ...     Additional arguments (not used).
#'
#' @return  A list object containing the following elements:
#'
#' \describe{
#'   \item{\code{summary}}{gives the properties of the specified scale/grain \code{whichThresh}
#'   of the GOC model;}
#'
#'   \item{\code{voronoi}}{a \code{RasterLayer} giving the Voronoi tessellation the
#'   specified scale/grain \code{whichThresh} of the GOC model;}
#'
#'   \item{\code{centroids}}{a \code{SpatialPoints} objects giving the centroids
#'   of the polygons in the Voronoi tessellation at the specified scale/grain \code{whichThresh};}
#'
#'   \item{\code{th}}{a \code{igraph} object giving the graph describing the relationship
#'   among the polygons at the specified scale/grain \code{whichThresh}}
#' }
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
#' @importFrom graphics plot
#' @importFrom raster as.data.frame plot reclassify
#' @importFrom sp geometry plot SpatialPoints SpatialPolygonsDataFrame
#' @include classes.R
#' @rdname grain
#' @seealso \code{\link{GOC}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_grain.R
#'
setGeneric("grain", function(x, ...) {
  standardGeneric("grain")
})

#' @export
#' @rdname grain
setMethod(
  "grain",
  signature = "goc",
  definition = function(x, whichThresh, ...) {
    dots <- list(...)
    if (!is.null(dots$sp)) warning("Argument 'sp' is deprecated and will be ignored.")

    ## Check whichThresh
    thresholds <- x@summary$id
    if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(thresholds)))) { # nolint
      stop("whichThresh must index a single threshold existing in the GOC object")
    }

    results <- list()

    results$summary <- x@summary[whichThresh, ]

    if (is_igraph(x@th[[whichThresh]]$goc)) {
      threshGraph <- x@th[[whichThresh]]$goc

      ## Produce is-becomes reclassification table for voronoi raster
      rclTable <-  matrix(0, 1, 2)
      for (i in 1:length(V(threshGraph)$polygonId)) {
        rclTable <- rbind(rclTable,
                          cbind(as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", "))),
                                as.integer(V(threshGraph)$polygonId[i])))
      }
      rclTable <- rclTable[2:nrow(rclTable), ]

      results$voronoi <- reclassify(x@voronoi, rcl = rclTable)

      results$centroids <- SpatialPoints(cbind(V(threshGraph)$centroidX,
                                               V(threshGraph)$centroidY))

    }

    out <- new("grain", voronoi = results$voronoi, summary = results$summary,
               centroids = results$centroids, th = threshGraph)

    return(out)
})
