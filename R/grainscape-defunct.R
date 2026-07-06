#' Defunct functions in grainscape
#'
#' These functions are defunct and have been removed from \pkg{grainscape}.
#' They were deprecated in version 0.4.0, when the functions were renamed to
#' drop their `gs`/`gsGOC` prefixes (#10), and are removed in version 1.0.0.
#' Use the replacement function named in each function's error message.
#'
#' @param ... Arguments formerly passed to these functions; now ignored.
#'
#' @name grainscape-defunct
#' @keywords internal
NULL

#' @export
#' @rdname grainscape-defunct
gsMPG <- function(...) {
  .Defunct("MPG", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsGOC <- function(...) {
  .Defunct("GOC", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsGOCCorridor <- function(...) {
  .Defunct("corridor", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsGOCDistance <- function(...) {
  .Defunct("distance", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsGOCPoint <- function(...) {
  .Defunct("point", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsGOCVisualize <- function(...) {
  .Defunct("grain", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsGraphDataFrame <- function(...) {
  .Defunct("graphdf", package = "grainscape")
}

#' @export
#' @rdname grainscape-defunct
gsMPGstitch <- function(...) {
  .Defunct(
    "MPG",
    package = "grainscape",
    msg = paste(
      "'gsMPGstitch' is defunct.",
      "'MPG' is now capable of handling larger rasters. Try it instead.",
      'See help("grainscape-defunct").',
      sep = "\n"
    )
  )
}
