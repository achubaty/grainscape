#' Deprecated functions
#'
#' These functions have been deprecated and will be removed in a future release.
#'
#' @export gsGOC
#' @inheritParams GOC
#' @param mpg  A \code{mpg} object.
#' @rdname grainscape-deprecated
#'
gsGOC <- function(mpg, nThresh = NULL, doThresh = NULL,
                  weight = "lcpPerimWeight", sp = FALSE, verbose = 3) {
  .Deprecated("GOC", old = "gsGOC")
  GOC(mpg, nThresh, doThresh, weight, sp, verbose)
}

#' @export gsGOCCorridor
#' @inheritParams corridor
#' @param GOC  A \code{goc} object.
#' @rdname grainscape-deprecated
#'
gsGOCCorridor <- function(GOC, whichThresh, coords, doPlot = FALSE, weight = "meanWeight") {
  .Deprecated("corridor", old = "gsGOCCorridor")
  corridor(GOC, whichThresh, coords, doPlot, weight)
}

#' @export gsGOCDistance
#' @inheritParams distance
#' @rdname grainscape-deprecated
#'
gsGOCDistance <- function(GOC, coords, weight = "meanWeight") {
  .Deprecated("distance", old = "gsGOCDistance")
  corridor(GOC, coords, weight)
}

#' @export gsGOCPoint
#' @inheritParams distance
#' @rdname grainscape-deprecated
#'
gsGOCPoint <- function(GOC, coords) {
  .Deprecated("point", old = "gsGOCPoint")
  point(GOC, coords)
}

#' @export gsGOCVisualize
#' @inheritParams visualize
#' @rdname grainscape-deprecated
#'
gsGOCVisualize <- function(GOC, whichThresh, sp = FALSE, doPlot = FALSE) {
  .Deprecated("visualize", old = "gsGOCVisualize")
  visualize(GOC, whichThresh, sp, doPlot)
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
  if (!is.null(filterPatch)) warning("gsMPG(): argument 'filterPatch' was supplied but will be ignored.")
  if (!is.null(sa)) warning("gsMPG(): argument 'sa' was supplied but will be ignored.")
  if (spreadFactor != 0) warning("gsMPG(): argument 'spreadFactor' was supplied but will be ignored.")
  MPG(cost, patch)
}



#' Defunct functions
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
