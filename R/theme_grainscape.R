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
#' @export
#' @importFrom ggplot2 %+replace% element_blank theme theme_bw unit
#' @rdname theme_grainscape
#' @seealso \code{\link{ggGS}}, \code{\link{plot}}, \code{\link[ggthemes]{theme_map}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_theme.R
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
