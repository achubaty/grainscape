#' Filter out patches smaller than a specified area
#'
#' Pre-process patch rasters prior to their use with [MPG()].
#'
#' It examines a binary raster to identify all patches or clumps of cells with
#' values `=1`, determines their area, and returns a binary raster where
#' only patches of area greater than or equal to the specified amount are represented.
#'
#' This is helpful when analyzing habitat connectivity models where patches
#' represent a land cover or habitat type.
#' For example, a raster may have patches of a certain habitat type of insufficient
#' area to support the ecological process of interest.
#' Another use case is remote sensing classification errors that have introduced artifacts.
#' Filtering can help in both cases.
#'
#' @param x             A binary raster (i.e. consisting of `0`, `1`,
#'                      or `NA` cells), where cells `=1` represent
#'                      patches
#'
#' @param cells         The minimum number of cells that constitute a patch.
#'                      Default `NULL`. Only one of `cells` or
#'                      `area` may be specified.
#'
#' @param area          The minimum area that constitutes a patch (where area is
#'                      calculated in the coordinate reference system of the
#'                      raster by multiplying the count of cells in patch by the x
#'                      and y resolution of a raster cell). Default `NULL`.
#'                      Only one of `cells` or `area` may be specified.
#'
#' @param ...           Additional arguments passed to [terra::patches()].
#'                      For example, `directions = 4` may be used to be more
#'                      conservative about which cells constitute a patch.
#'
#' @return   A binary raster where all patches (i.e. clumped areas `=1`)
#'           are greater than the specified area.
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @include classes.R
#' @rdname patchFilter
#' @seealso [MPG()]
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_filter.R
#'
setGeneric(
  "patchFilter",
  function(x, cells = NULL, area = NULL, ...) {
    standardGeneric("patchFilter")
  }
)

#' @export
#' @rdname patchFilter
setMethod(
  "patchFilter",
  signature = "SpatRaster",
  definition = function(x, cells, area, ...) {
    if (!xor(is.null(cells), is.null(area))) {
      stop("either the cells or area parameter must be specified, not both.")
    }

    xvals <- terra::values(x)[, 1]
    if (any(!(unique(xvals) %in% c(NA, 0, 1)))) {
      stop("x must be a binary raster containing 0, 1 or NA values")
    }

    cellThresh <- if (!is.null(area)) {
      ceiling(area / (terra::res(x)[1] * terra::res(x)[2]))
    } else {
      cells
    }

    ## terra::patches() treats 0 cells as patches; mask non-patch cells to NA first
    xMasked <- x
    xMasked[xMasked == 0] <- NA
    patch <- terra::patches(xMasked, ...)
    fp <- terra::freq(patch, usenames = FALSE)
    rmpatch <- fp[fp[, "count"] < cellThresh, "value"]

    out <- patch
    if (length(rmpatch) > 0) {
      ## use terra::subst() rather than `out[out %in% rmpatch] <- NA`: the `%in%` form relies on
      ## S4 dispatch for a SpatRaster that errors ("'match' requires vector arguments") on
      ## R < 4.6 with recent terra (#76)
      out <- terra::subst(out, rmpatch, NA)
    }
    out <- !is.na(out)

    return(out)
  }
)
