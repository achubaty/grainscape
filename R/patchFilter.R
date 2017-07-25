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
#'                      and y resolution of a raster cell).  Default \code{NULL}.
#'                      Only one of \code{cells} or \code{area} may be specified.
#'
#' @param ...           Additional arguments passed to the \code{\link{clump}}
#'                      function in the \code{raster} package.  For example
#'                      \code{directions = 4} may be used to be more conservative
#'                      about which cells constitute a patch.
#'
#' @return   A binary raster where all patches (i.e. clumped areas \code{=1})
#'           are greater than the specified area.
#'
#' @author Paul Galpern and Alex Chubaty
#' @docType methods
#' @export
#' @importFrom raster clump freq res
#' @include classes.R
#' @rdname patchFilter
#' @seealso \code{\link{MPG}}
#' @examples
#' ## Load raster landscape
#' tiny <- raster::raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- raster::reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features = 10
#' ## and all patches are greater than or equal to 2 cells in size
#' filteredPatch <- patchFilter(tinyCost == 10, cells = 2)
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
#' plot(tinyPatchMPG)
#'
#' ## Compare to removal of patches greater than or equal to 40 cells in size!
#' filteredPatch <- patchFilter(tinyCost == 10, cells = 40)
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
#' plot(tinyPatchMPG)
#'
#' ## Use a rook/castle 4-direction case rather than the queen 8-direction case
#' ## to identify neighbouring cells in a patch
#' filteredPatch <- patchFilter(tinyCost == 10, cells = 40, directions = 4)
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
#' plot(tinyPatchMPG)
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
