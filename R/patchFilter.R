#' Filter out patches smaller than a specified area
#'
#' Pre-process patch rasters prior to their use with \code{\link{MPG}}.
#'
#' It examines a binary raster to identify all patches or clumps of cells with
#' values \code{=1}, determines their area, and returns a binary raster where
#' only patches of area greater than or equal to the specified amount are represented.
#'
#' This is helpful when analyzing habitat connectivity models where patches
#' represent a land cover or habitat type.
#' For example, a raster may have patches of a certain habitat type of insufficient
#' area to support the ecological process of interest.
#' Another use case is remote sensing classification errors that have introduced artifacts.
#' Filtering can help in both cases.
#'
#' @param x             A binary raster (i.e. consisting of \code{0}, \code{1},
#'                      or \code{NA} cells), where cells \code{=1} represent
#'                      patches
#'
#' @param cells         The minimum number of cells that constitute a patch.
#'                      Default \code{NULL}. Only one of \code{cells} or
#'                      \code{area} may be specified.
#'
#' @param area          The minimum area that constitutes a patch (where area is
#'                      calculated in the coordinate reference system of the
#'                      raster by multiplying the count of cells in patch by the x
#'                      and y resolution of a raster cell). Default \code{NULL}.
#'                      Only one of \code{cells} or \code{area} may be specified.
#'
#' @param ...           Additional arguments passed to the \code{\link{clump}}
#'                      function in the \code{raster} package. For example
#'                      \code{directions = 4} may be used to be more conservative
#'                      about which cells constitute a patch.
#'
#' @return   A binary raster where all patches (i.e. clumped areas \code{=1})
#'           are greater than the specified area.
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom raster clump freq res
#' @include classes.R
#' @rdname patchFilter
#' @seealso \code{\link{MPG}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_filter.R
#'
setGeneric(
  "patchFilter",
  function(x, cells = NULL, area = NULL, ...) {
    standardGeneric("patchFilter")
})

#' @export
#' @rdname patchFilter
setMethod(
  "patchFilter",
  signature = "RasterLayer",
  definition = function(x, cells, area, ...) {

  if (!xor(is.null(cells), is.null(area))) {
    stop("either the cells or area parameter must be specified, not both.")
  }

  if (any(!(unique(x[]) %in% c(NA, 0, 1)))) {
    stop("x must be a binary raster containing 0, 1 or NA values")
  }

  cellThresh <- if (!is.null(area)) {
    ceiling(area / (res(x)[1] * res(x)[2]))
  } else {
    cells
  }

  patch <- clump(x, ...)
  fp <- freq(patch)
  rmpatch <- fp[fp[, 2] < cellThresh, 1]

  out <- patch
  out[patch[] %in% rmpatch] <- NA
  out <- !is.na(out)

  return(out)
})
