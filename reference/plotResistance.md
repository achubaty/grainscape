# Plot a resistance surface with a consistent colour scheme

Draws a resistance `SpatRaster` using a shared colour scheme (green for
low resistance through red for high resistance) with a discrete-swatch
legend placed to the left of the panel. Each resistance value is mapped
to a fixed colour so that the same value appears identical across
figures, which is useful when comparing several landscapes or when
pairing a resistance surface with a network plot (see
[`plotWithResistance()`](https://www.alexchubaty.com/grainscape/reference/plotWithResistance.md)).

## Usage

``` r
plotResistance(x, maxResistance = 12L)
```

## Arguments

- x:

  A resistance `SpatRaster`, e.g. the `cost` surface passed to
  [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md).

- maxResistance:

  Integer. The resistance value mapped to the top (red) end of the
  colour ramp; values from `1` to `maxResistance` receive fixed colours
  so the scheme stays consistent across surfaces. Defaults to `12`, and
  is automatically raised to the largest value present in `x` if that
  exceeds it.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`plotWithResistance()`](https://www.alexchubaty.com/grainscape/reference/plotWithResistance.md),
[`ggGS()`](https://www.alexchubaty.com/grainscape/reference/ggGS.md)

## Author

Alex M Chubaty

## Examples

``` r
tiny <- terra::rast(
  system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
)
tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
plotResistance(tinyCost)

```
