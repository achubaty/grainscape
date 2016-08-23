if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

#' Habitat connectivity engine
#'
#' DESCRIPTION NEEDED
#'
#' @param cost      Numeric raster cost (resistance) map.
#' @param patches   Logical raster indicating presence of habitat patches.
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
#' @importFrom raster extent getValues ncol nrow raster unique
#' @rdname habConnEngine
#' @seealso \code{link{habConnRcpp}}
#'
#' @examples
#' cost <- raster::raster(system.file("extdata/fragmented.asc", package = "grainscape2"))
#' if (interactive()) plot(cost)
#'
#' # cells in raster `cost` with value of 1 are habitat (patch) cells
#' links <- habConnEngine(cost, patches = (cost == 1))
#'
#'
#' if (interactive()) {
#'   links                  # examine the object
#'   plot(links$voronoi)    # plot the voronoi tesselation
#'   plot(links$patchLinks) # plot the patches and links
#' }
habConnEngine <- function(cost, patches) {
  stopifnot(class(cost) == "RasterLayer", class(patches) == "RasterLayer",
            extent(cost) == extent(patches),
            ncol(cost) == ncol(patches), nrow(cost) == nrow(patches))
  hce <- .habConnRcpp(cost = getValues(cost), patches = getValues(patches),
                      nrow = nrow(cost), ncol = ncol(cost))

  # convert `VoronoiVector` to a raster of identical dimensions etc. as `cost`
  voronoi <- cost
  voronoi[] <- hce$VoronoiVector

  # convert `PatchLinkIDsVector` to a raster of identical dimensions etc. as `cost`
  patchLinks <- cost
  patchLinks[] <- hce$PatchLinkIDsVector
  patchLinks[patchLinks == 0] <- NA

  # convert `LinkData` to a data.frame
  linkData <- lapply(hce$LinkData, data.frame) %>% do.call(rbind, .)

  out <- list(voronoi = voronoi, patchLinks = patchLinks, linkData = linkData)
  class(out) <- "hce"
  return(out)
}
