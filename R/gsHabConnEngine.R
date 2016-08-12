#' Habitat connectivity engine
#'
#' DESCRIPTION NEEDED
#'
#' @param cost      Raster cost (resistance) map.
#' @param hab       Numeric value corresponding to habitat cells.
#' @param nodata    Numeric value corresponding to 'no data' or \code{NA}.
#' @param fpthresh  Threshold value for making comparisons of floating point numbers.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @importFrom raster getValues ncol nrow unique
#' @rdname habConnEngine
#' @seealso \code{link{habConnRcpp}}
#'
#' @examples
#' cost <- raster(system.file("extdata/fragmented.asc", package = "grainscape2"))
#' if (interactive()) plot(cost)
#'
#' # cells in raster `cost` with value of 1 are habitat (patch) cells
#' # cell in raster `cost` with value -9999 should be interpreted as NA (no data)
#' habConnEngine(cost, hab = 1, nodata = -9999)
#'
habConnEngine <- function(cost, hab, nodata = as.integer(NA), fpthresh = 1e-4) {
  stopifnot(class(cost) == "RasterLayer",
            length(hab) == 1)
  .habConnRcpp(cost = getValues(cost), nrow = nrow(cost), ncol = ncol(cost),
               hab = hab, no_data = as.numeric(nodata), distinctValues = sort(unique(cost)), threshold = fpthresh)
}
