#' A \code{ggplot2} theme for \code{grainscape}
#'
#' A \code{\link{ggplot2}} theme designed for \code{grainscape} based on the
#' \code{\link[ggthemes]{theme_map}} theme, with several modifications.
#'
#' @param base_size     Base font size
#'
#' @param base_family   Base font family
#'
#' @return A theme suitable for use with \code{\link{ggplot}}
#'
#' @author Paul Galpern and Alex Chubaty
#' @docType methods
#' @export
#' @importFrom ggplot2 %+replace% element_blank theme theme_bw unit
#' @rdname theme_grainscape
#' @seealso \code{\link{ggGS}}, \code{\link{plot}}, \code{\link[ggthemes]{theme_map}}
#'
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster::raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- raster::reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Plot the patches in a minimum planar graph
#' ggplot() +
#'   geom_raster(ggGSPrep(tinyPatchMPG, "patchId"), aes(x = x, y = y, fill = value)) +
#'   theme_grainscape()
#' }
#'
theme_grainscape <- function(base_size = 9, base_family = "") { # nolint
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"), plot.background = element_blank(),
          legend.justification = c(0, 0), legend.position = "none",
          aspect.ratio = 1)
}
