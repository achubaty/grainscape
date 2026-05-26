utils::globalVariables(c("x", "y", "value"))

#' Plot a resistance surface with a consistent colour scheme
#'
#' @description
#' Draws a resistance `SpatRaster` using a shared colour scheme (green for low
#' resistance through red for high resistance) with a discrete-swatch legend placed to
#' the left of the panel. Each resistance value is mapped to a fixed colour so that the
#' same value appears identical across figures, which is useful when comparing several
#' landscapes or when pairing a resistance surface with a network plot (see
#' [plotWithResistance()]).
#'
#' @param x  A resistance `SpatRaster`, e.g. the `cost` surface passed to [MPG()].
#'
#' @param maxResistance  Integer. The resistance value mapped to the top (red) end of
#'   the colour ramp; values from `1` to `maxResistance` receive fixed colours so the
#'   scheme stays consistent across surfaces. Defaults to `12`, and is automatically
#'   raised to the largest value present in `x` if that exceeds it.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @author Alex M Chubaty
#' @seealso [plotWithResistance()], [ggGS()]
#' @export
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_manual coord_equal theme
#' @importFrom grDevices colorRampPalette
#' @importFrom stats setNames
#' @examples
#' tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
#' tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#' plotResistance(tinyCost)
#'
plotResistance <- function(x, maxResistance = 12L) {
  df <- ggGS(x)
  vals <- round(df$value)
  n <- max(as.integer(maxResistance), max(vals, na.rm = TRUE))
  df$value <- factor(vals, levels = seq_len(n))
  pal <- setNames(
    colorRampPalette(c("#1a9850", "#ffffbf", "#d73027"))(n),
    as.character(seq_len(n))
  )
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_manual(
      values = pal, name = "Resistance",
      drop = TRUE, na.translate = FALSE
    ) +
    coord_equal() +
    theme_grainscape() +
    theme(legend.position = "left")
}

#' Show a resistance surface beside a network plot
#'
#' @description
#' Places a resistance-surface reference panel (see [plotResistance()]) to the left of a
#' network plot -- for example one produced by [plot()] on an `mpg` or `grain` object, or
#' any \pkg{ggplot2} plot -- so the two can be compared at the same size as a visual
#' sanity check. Both panels are drawn at equal size (using `cowplot::plot_grid()` with
#' `align = "h"` and equal coordinates) and the resistance legend is placed at the far
#' left.
#'
#' Requires the \pkg{cowplot} package.
#'
#' @param x   A resistance `SpatRaster` shown as the reference panel on the left
#'   (passed to [plotResistance()]).
#'
#' @param gg  A plot shown on the right, e.g. the result of [plot()] on an `mpg` or
#'   `grain` object (typically with `theme = FALSE`), or any [ggplot2::ggplot()] object.
#'
#' @param maxResistance  Passed to [plotResistance()].
#'
#' @return A two-panel plot grid produced by `cowplot::plot_grid()`.
#'
#' @author Alex M Chubaty
#' @seealso [plotResistance()]
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("cowplot", quietly = TRUE)) {
#'   tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
#'   tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'   tinyMPG <- MPG(tinyCost, patch = tinyCost == 1)
#'   plotWithResistance(tinyCost, plot(tinyMPG, quick = "mpgPlot", theme = FALSE))
#' }
#' }
#'
plotWithResistance <- function(x, gg, maxResistance = 12L) {
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package 'cowplot' is required for plotWithResistance(); please install it.",
      call. = FALSE
    )
  }
  cowplot::plot_grid(
    plotResistance(x, maxResistance), gg,
    nrow = 1, align = "h", axis = "tb", rel_widths = c(1.25, 1)
  )
}
