utils::globalVariables(".")

#' Habitat connectivity engine
#'
#' Internal function.
#' Serves as a wrapper around the habitat connectivity engine developed in C++.
#'
#' @param cost      Numeric raster cost (resistance) map.
#' @param patches   Logical raster indicating presence of habitat patches.
#'
#' @return An object of class [hce-class].
#'
#' @author Alex Chubaty
#' @include classes.R
#' @keywords internal
#' @rdname habConnEngine
#' @seealso \code{link{habConnRcpp}}
#'
.habConnEngine <- function(cost, patches) {
  stopifnot(
    inherits(cost, "SpatRaster"),
    inherits(patches, "SpatRaster"),
    terra::ext(cost) == terra::ext(patches),
    terra::ncol(cost) == terra::ncol(patches),
    terra::nrow(cost) == terra::nrow(patches)
  )

  hce <- .habConnRcpp(
    cost = as.numeric(terra::values(cost)[, 1]),
    patches = as.numeric(terra::values(patches)[, 1]),
    nrow = terra::nrow(cost),
    ncol = terra::ncol(cost)
  )

  ## convert `VoronoiVector` to a raster of identical dimensions etc. as `patches`
  voronoi <- patches
  terra::values(voronoi) <- hce$VoronoiVector

  ## convert `PatchLinkIDsVector` to a raster of identical dimensions etc. as `cost`
  patchLinks <- cost
  terra::values(patchLinks) <- hce$PatchLinkIDsVector
  patchLinks[patchLinks == 0] <- NA

  ## convert `LinkData` to a data.frame
  linkData <- lapply(hce$LinkData, data.frame) |> do.call(rbind, args = _)

  out <- new("hce", voronoi = voronoi, patchLinks = patchLinks, linkData = linkData)
  return(out)
}
