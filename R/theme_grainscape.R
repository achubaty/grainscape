#' A `ggplot2` theme for `grainscape`
#'
#' A \pkg{ggplot2} theme designed for `grainscape` based on the
#' [ggthemes::theme_map()] theme, with several modifications.
#'
#' @param base_size     Base font size
#'
#' @param base_family   Base font family
#'
#' @return A theme suitable for use with [ggplot2::ggplot()]
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom ggplot2 %+replace% element_blank theme theme_bw unit
#' @rdname theme_grainscape
#' @seealso [ggGS()], [plot()], [ggthemes::theme_map()]
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_theme.R
theme_grainscape <- function(base_size = 9, base_family = "") { # nolint
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(), axis.text = element_blank(),
      axis.ticks = element_blank(), axis.title = element_blank(),
      panel.background = element_blank(), panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"), plot.background = element_blank(),
      legend.justification = c(0, 0), legend.position = "none",
      aspect.ratio = 1
    )
}
