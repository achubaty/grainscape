#' \code{grainscape}: Deprecated
#'
#' These have been deprecated and will be removed in a future release.
#'
#' @note the \code{sp} argument has also been deprecated from all functions.
#'
#' @inheritParams GOC
#'
#' @param mpg  A \code{mpg} object.
#'
#' @param sp   Logical. If \code{TRUE} the \code{rgeos} package is used to create
#'            a vector of class \code{\link{SpatialPolygonsDataFrame}} describing
#'            the finest grain of connectivity.
#'
#' @param doPlot  Logical. If \code{TRUE} plots a vector visualization of the
#'                corridor at the given scale
#'
#' @export gsGOC
#' @rdname grainscape-deprecated
#'
gsGOC <- function(mpg, nThresh = NULL, doThresh = NULL,
                  weight = "lcpPerimWeight", sp = FALSE, verbose = 3) {
  .Deprecated("GOC", old = "gsGOC")
  if (!is.null(sp)) warning("gsGOC(): argument 'sp' was supplied but will be ignored.")
  GOC(mpg, nThresh, doThresh, weight, sp, verbose)
}

#' @export gsGOCCorridor
#' @inheritParams corridor
#' @param GOC  A \code{goc} object.
#' @rdname grainscape-deprecated
#'
gsGOCCorridor <- function(GOC, whichThresh, coords, doPlot = FALSE, # nolint
                          weight = "meanWeight") {
  .Deprecated("corridor", old = "gsGOCCorridor")
  corridor(GOC, whichThresh, coords, doPlot, weight)
}

#' @export gsGOCDistance
#' @inheritParams distance
#' @rdname grainscape-deprecated
#'
gsGOCDistance <- function(GOC, coords, weight = "meanWeight") { # nolint
  .Deprecated("distance", old = "gsGOCDistance")
  corridor(GOC, coords, weight)
}

#' @export gsGOCPoint
#' @inheritParams distance
#' @rdname grainscape-deprecated
#'
gsGOCPoint <- function(GOC, coords) { # nolint
  .Deprecated("point", old = "gsGOCPoint")
  point(GOC, coords)
}

#' @export gsGOCVisualize
#' @inheritParams grain
#' @rdname grainscape-deprecated
#'
gsGOCVisualize <- function(GOC, whichThresh, sp = FALSE, doPlot = FALSE) { # nolint
  .Deprecated("grain", old = "gsGOCVisualize")
  grain(GOC, whichThresh, sp, doPlot)
}

#' @export gsGOCVisualize
#' @inheritParams grain
#' @rdname grainscape-deprecated
#'
visualize <- function(GOC, whichThresh, sp = FALSE, doPlot = FALSE) { # nolint
  .Deprecated("grain", old = "visualize")
  if (!is.null(sp)) warning("visualize(): argument 'sp' was supplied but will be ignored.")
  grain(GOC, whichThresh, sp, doPlot)
}

#' @export gsGraphDataFrame
#' @inheritParams graphdf
#' @rdname grainscape-deprecated
#'
gsGraphDataFrame <- function(x) {
  .Deprecated("graphdf", old = "gsGraphDataFrame")
  graphdf(x)
}

#' @export gsMPG
#' @inheritParams MPG
#' @param filterPatch,sa,spreadFactor  No longer used.
#' @rdname grainscape-deprecated
#'
gsMPG <- function(cost, patch, sa = NULL, filterPatch = NULL, spreadFactor = 0) {
  .Deprecated("MPG", old = "gsMPG")
  if (!is.null(filterPatch))
    warning("gsMPG(): argument 'filterPatch' was supplied but will be ignored.")

  if (!is.null(sa))
    warning("gsMPG(): argument 'sa' was supplied but will be ignored.")

  if (spreadFactor != 0)
    warning("gsMPG(): argument 'spreadFactor' was supplied but will be ignored.")
  MPG(cost, patch)
}



#' \code{grainscape}: Defunct
#'
#' These functions have removed from \code{grainscape}.
#'
#' @export gsMPGstitch
#' @param ... Any arguments passed to defunct functions.
#' @rdname grainscape-defunct
#'
gsMPGstitch <- function(...) {
  .Defunct("MPG", msg = paste("'gsMPGstitch' is defunct.\n",
                              "'MPG' is now capable of handling larger rasters. Try it instead."))
}
