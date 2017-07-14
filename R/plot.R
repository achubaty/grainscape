#' Plotting \code{grainscape} objects
#'
#' @param x    A \code{grainscape} object (\code{corridor}, \code{grain}, or \code{mpg}).
#'
#' @param y    Ignored.
#'
#' @param ...  Additional arguments passed to \code{plot}.
#'
#' @author Alex Chubaty and Paul Galpern
#' @docType methods
#' @export
#' @importFrom raster plot
#' @include classes.R
#' @rdname plot
#' @seealso \code{\linkS4class{corridor}},
#'          \code{\linkS4class{grain}},
#'          \code{\linkS4class{mpg}}
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' library(raster)
#'
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassifyification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))
#'
#' ## Quick visualization of the MPG
#' plot(tinyPatchMPG, col = c("grey", "black"), legend = FALSE)
#' }
#'
setMethod(
  "plot",
  signature = "corridor",
  definition = function(x, y, ...) {
    plot(x@voronoi, col = c("white", "black"))
    plot(x@linksSP, add = TRUE, col = "darkgray", lwd = 1.5)
    plot(x@nodesSP, add = TRUE, pch = 21, col = "darkgrey", bg = "white", cex = 0.75)
    plot(x@shortestLinksSP, add = TRUE, col = "black", lwd = 2)
    plot(x@shortestNodesSP, add = TRUE,  pch = 21, col = "black", bg = "white", cex = 0.75)
})

#' @export
#' @rdname plot
setMethod(
  "plot",
  signature = "grain",
  definition = function(x, y, ...) {
    dots <- list(...)
    dots$main <- if (any(grepl(pattern = "main", names(dots)))) {
      as.character(dots$main)
    } else {
      paste(c("whichThresh = ", x@summary$id), collapse = "")
    }

    raster::plot(x@voronoi, main = dots$main, ...)
})

#' @export
#' @rdname plot
setMethod(
  "plot",
  signature = "mpg",
  definition = function(x, y, ...) {
    plot(x@mpgPlot, ...)
})
