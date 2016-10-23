#' Plot a \code{mpg} object
#'
#' @param x    A \code{mpg} object produced by \code{\link{MPG}}.
#'
#' @param y    Ignored.
#'
#' @param ...  Additional arguments passed to \code{raster::plot}.
#'
#' @importFrom raster plot
#' @include classes.R
#' @export
#' @rdname plot
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
  signature = "mpg",
  definition = function(x, y, ...) {
    plot(x@mpgPlot, ...)
})
