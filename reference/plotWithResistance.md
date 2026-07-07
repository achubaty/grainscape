# Show a resistance surface beside a network plot

Places a resistance-surface reference panel (see
[`plotResistance()`](https://www.alexchubaty.com/grainscape/reference/plotResistance.md))
to the left of a network plot – for example one produced by
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on an `mpg` or
`grain` object, or any ggplot2 plot – so the two can be compared at the
same size as a visual sanity check. Both panels are drawn at equal size
(using
[`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)
with `align = "h"` and equal coordinates) and the resistance legend is
placed at the far left.

Requires the cowplot package.

## Usage

``` r
plotWithResistance(x, gg, maxResistance = 12L)
```

## Arguments

- x:

  A resistance `SpatRaster` shown as the reference panel on the left
  (passed to
  [`plotResistance()`](https://www.alexchubaty.com/grainscape/reference/plotResistance.md)).

- gg:

  A plot shown on the right, e.g. the result of
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on an `mpg`
  or `grain` object (typically with `theme = FALSE`), or any
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object.

- maxResistance:

  Passed to
  [`plotResistance()`](https://www.alexchubaty.com/grainscape/reference/plotResistance.md).

## Value

A two-panel plot grid produced by
[`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html).

## See also

[`plotResistance()`](https://www.alexchubaty.com/grainscape/reference/plotResistance.md)

## Author

Alex M Chubaty

## Examples

``` r
# \donttest{
if (requireNamespace("cowplot", quietly = TRUE)) {
  tiny <- terra::rast(
    system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
  )
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyMPG <- MPG(tinyCost, patch = tinyCost == 1)
  plotWithResistance(tinyCost, plot(tinyMPG, quick = "mpgPlot", theme = FALSE))
}

# }
```
