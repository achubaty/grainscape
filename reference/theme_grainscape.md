# A `ggplot2` theme for `grainscape`

A ggplot2 theme designed for `grainscape` based on the
[`ggthemes::theme_map()`](http://jrnold.github.io/ggthemes/reference/theme_map.md)
theme, with several modifications.

## Usage

``` r
theme_grainscape(base_size = 9, base_family = "")
```

## Arguments

- base_size:

  Base font size

- base_family:

  Base font family

## Value

A theme suitable for use with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## See also

[`ggGS()`](https://www.alexchubaty.com/grainscape/reference/ggGS.md),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`ggthemes::theme_map()`](http://jrnold.github.io/ggthemes/reference/theme_map.md)

## Author

Paul Galpern and Alex Chubaty

## Examples

``` r
## Load raster landscape
tiny <- terra::rast(
  system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
)

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
if (interactive()) {
  library(ggplot2)

  ## Plot the patches in a minimum planar graph
  theme_set(theme_grainscape())
  plot(tinyPatchMPG, quick = "mpgPlot")
}
```
