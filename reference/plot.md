# Plot quick visualizations of `grainscape` objects

Plot quick visualizations of `mpg`, `grain`, and `corridor` objects.

This function is intended to get a quick look at the state of a
`grainscape` object by rendering what are likely to be the most
universally useful visualizations of the spatial data within these
objects.

Much more control is available using
[`ggGS()`](https://www.alexchubaty.com/grainscape/reference/ggGS.md)
with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
enabling the layering of different different analytical products, and
the visualization of node and link attributes.

For high-resolution visualization and the greatest level of control use
[`export()`](https://www.alexchubaty.com/grainscape/reference/export.md)
to export spatial objects for cartographic representation in a
geographic information system (GIS).

## Usage

``` r
# S4 method for class 'corridor,ANY'
plot(x, y, quick = NULL, print = TRUE, theme = TRUE, ...)

# S4 method for class 'grain,ANY'
plot(x, y, quick = NULL, print = TRUE, theme = TRUE, ...)

# S4 method for class 'mpg,ANY'
plot(x, y, quick = NULL, print = TRUE, theme = TRUE, ...)
```

## Arguments

- x:

  A `grainscape` object (`corridor`, `grain`, or `mpg`).

- y:

  Ignored.

- quick:

  If `NULL` (the default) it will plot the most useful quick
  visualization for the supplied object type. See below for a
  description of the available quick plots, and the defaults.

- print:

  Render the `ggplot` on the default graphics device. Default is `TRUE`.

- theme:

  Apply grainscape theme and scale aesthetics. Default is `TRUE`.

- ...:

  Additional arguments (not used).

## Value

       Invisibly, a `ggplot2` object to which additional `ggplot`
               geoms and adjustments can be applied. Has the side effect of
               rendering the plot, unless `print = FALSE`.

## Types of visualization available with the `quick` parameter

`"mpgPerimPlot"` gives a a vector rendering of the minimum planar graph
with vector links connecting the perimeters of the patches. This doesn't
accurately represent the sinuosity of paths of the links between patches
but offers a good approximation that renders better at large extents.
Default for `mpg` objects. Not available for other objects.

`"mpgPlot"` gives a raster-only rendering of the minimum planar graph
where `patchId` are positive integers, and `linkId` are negative
integers showing the shortest paths between patches Only available for
`mpg` objects.

`"network"` gives a vector rendering of the minimum planar graph or the
grains of connectivity network with nodes and links plotted at the patch
or polygon centroid locations. Available for `mpg` and `grain` objects.
Default for `grain` objects.

`"grainPlot"` gives a raster and vector rendering of the grains of
connectivity network with nodes and links plotted at polygon centroid
locations, superimposed over the boundaries of the Voronoi polygons. Can
be time consuming on large rasters due to the Voronoi boundary
extraction. Only available for `grain` objects.

`"corridorPlot"` renders the output of a
[`corridor()`](https://www.alexchubaty.com/grainscape/reference/corridor.md)
analysis. It is the only option available with `corridor` objects and
the default.

## See also

[`ggGS()`](https://www.alexchubaty.com/grainscape/reference/ggGS.md),
[`export()`](https://www.alexchubaty.com/grainscape/reference/export.md),
[corridor](https://www.alexchubaty.com/grainscape/reference/corridor-class.md),
[grain](https://www.alexchubaty.com/grainscape/reference/grain-class.md),
[mpg](https://www.alexchubaty.com/grainscape/reference/mpg-class.md)

## Author

Alex Chubaty and Paul Galpern

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
## Extract a representative subset of 5 grains of connectivity
tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
if (interactive()) {
  library(ggplot2)

  ## MPG and showing simplified links among the perimeters of patches
  plot(tinyPatchMPG)

  ## MPG showing links among the nodes of connected patches
  plot(tinyPatchMPG, quick = "network")

  ## MPG showing the shortest paths between patches actually used to
  ## to calculate link weight values
  plot(tinyPatchMPG, quick = "mpgPlot")

  ## A grain of connectivity network plot with Voronoi boundaries
  plot(grain(tinyPatchGOC, 3), quick = "grainPlot")

  ## Capture plot output for further processing with ggplot
  g <- plot(tinyPatchMPG, print = FALSE, theme = FALSE)
  g <- g +
    theme_minimal() +
    ggtitle("Minimum planar graph") +
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    theme(legend.position = "none") +
    xlab("Easting") +
    ylab("Northing")
  g

  ## To change aesthetics it is best to build the plot from scratch
  ## using grainscape::ggGS(). See examples therein.
}
```
