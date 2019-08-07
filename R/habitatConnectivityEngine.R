if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

#' Habitat connectivity engine
#'
#' Internal function. Serves as a wrapper around the habitat connectivity engine
#' developed in C++.
#'
#' @param cost      Numeric raster cost (resistance) map.
#' @param patches   Logical raster indicating presence of habitat patches.
#'
#' @return An object of class \code{\linkS4class{hce}}.
#'
#' @author Alex Chubaty
#' @importFrom raster extent getValues ncol nrow raster unique
#' @include classes.R
#' @keywords internal
#' @rdname habConnEngine
#' @seealso \code{link{habConnRcpp}}
#'
.habConnEngine <- function(cost, patches) {
  stopifnot(inherits(cost, "RasterLayer"),
            inherits(patches, "RasterLayer"),
            extent(cost) == extent(patches),
            ncol(cost) == ncol(patches),
            nrow(cost) == nrow(patches))

  hce <- .habConnRcpp(cost = getValues(cost), patches = getValues(patches),
                      nrow = nrow(cost), ncol = ncol(cost))

  # convert `VoronoiVector` to a raster of identical dimensions etc. as `cost`
  voronoi <- patches
  voronoi[] <- hce$VoronoiVector

  # convert `PatchLinkIDsVector` to a raster of identical dimensions etc. as `cost`
  patchLinks <- cost
  patchLinks[] <- hce$PatchLinkIDsVector
  patchLinks[patchLinks == 0] <- NA

  # convert `LinkData` to a data.frame
  linkData <- lapply(hce$LinkData, data.frame) %>% do.call(rbind, .)

  out <- new("hce", voronoi = voronoi, patchLinks = patchLinks, linkData = linkData)
  return(out)
}
