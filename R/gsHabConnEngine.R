#' Habitat connectivity engine
#'
#' DESCRIPTION NEEDED
#'
#' @param cost      Raster cost (resistance) map.
#' @param hab       Numeric value corresponding to habitat cells.
#'
#' @return An object of class \code{hce} containing the following components:\cr
#'         1. \code{voronoi}: a raster whose values indicate the voronoi tesselation;\cr
#'         2. \code{patchLinks}: a raster whose values indicate patch ids (positive integers)
#'            and link ids (negative integers);\cr
#'         3. \code{linkData}: data.frame of link attributes.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @importFrom raster getValues ncol nrow raster unique
#' @rdname habConnEngine
#' @seealso \code{link{habConnRcpp}}
#'
#' @examples
#' cost <- raster(system.file("extdata/fragmented.asc", package = "grainscape2"))
#' if (interactive()) plot(cost)
#'
#' # cells in raster `cost` with value of 1 are habitat (patch) cells
#' links <- habConnEngine(cost, hab = 1)
#'
#'
#' if (interactive()) {
#'   links                  # examine the object
#'   plot(links$voronoi)    # plot the voronoi tesselation
#'   plot(links$patchLinks) # plot the patches and links
#' }
habConnEngine <- function(cost, hab) {
  stopifnot(class(cost) == "RasterLayer",
            length(hab) == 1)
  hce <- .habConnRcpp(cost = getValues(cost), nrow = nrow(cost), ncol = ncol(cost),
                      hab = hab, threshold = getOption("gs.fpthresh"))

  # convert `VoronoiVector` to a raster of identical dimensions etc. as `cost`
  voronoi <- cost
  voronoi[] <- hce$VoronoiVector

  # convert `PatchLinkIDsVector` to a raster of identical dimensions etc. as `cost`
  patchLinks <- cost
  patchLinks[] <- hce$PatchLinkIDsVector

  # convert `LinkData` to a data.frame
  linkData <- do.call(rbind, hce$LinkData) %>% as.data.frame()

  out <- list(voronoi = voronoi, patchLinks = patchLinks, linkData = linkData)
  class(out) <- "hce"
  return(out)
}
