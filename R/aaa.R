#' Create emtpy/null SpatialPolygons* objects
#'
#' Internal use only.
#'
#' @section Warning:
#' Do not attempt to \code{print} or \code{show} these objects, as you'll get caught
#' in an inifinite loop that requires termitation of the R process!
#'
#' @importFrom sp SpatialPolygons
#' @keywords internal
#' @rdname emptySP
.emptySP <- function() {
  SpatialPolygons(list()) ## is this the best way to do this?
}


#' @importFrom sp SpatialPolygons SpatialPolygonsDataFrame
#' @keywords internal
#' @rdname emptySP
.emptySPDF <- function() {
  SpatialPolygonsDataFrame(.emptySP(), data.frame()) ## is this the best way to do this?
}
