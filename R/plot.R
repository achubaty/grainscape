#' Plotting \code{grainscape} objects
#'
#' @param x    A \code{mpg} object produced by \code{\link{MPG}} or \code{grain}
#'             object produced by \code{\link{grain}}.
#'
#' @param y    Ignored.
#'
#' @param ...  Additional arguments passed to \code{plot}.
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
