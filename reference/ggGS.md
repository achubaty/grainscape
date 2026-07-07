# Prepare data in `MPG` and `grain` objects for use with ggplot2

This is an informal `fortify`-type method that prepares either
`SpatRaster` or `igraph` objects contained as slots within `MPG` or
`grain` objects for easy plotting with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Usage

``` r
ggGS(x, type = NULL, ...)

# S4 method for class 'SpatRaster'
ggGS(x, type = NULL, ...)

# S4 method for class 'list'
ggGS(x, type = NULL, ...)

# S4 method for class 'mpg'
ggGS(x, type = NULL, ...)

# S4 method for class 'grain'
ggGS(x, type = NULL, ...)

# S4 method for class 'goc'
ggGS(x, type = NULL, ...)
```

## Arguments

- x:

  A `mpg`, `grain`, or `SpatRaster` object.

- type:

  If a `mpg` or `grain` object is supplied, this gives the name of the
  slot to prepare for plotting. Options are discussed below. Not
  required if a `SpatRaster` is supplied.

- ...:

  Additional arguments (not used).

## Value

A `data.frame` suitable for plotting with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

Where `type` is a raster the `data.frame` will have the following
columns:

- `value`:

  the value of the raster cell

- `x`:

  the x coordinate of the centre of the raster cell

- `y`:

  the y coordinate of the centre of the raster cell

Where `type = 'nodes'` the `data.frame` will have the following columns:

- `x`:

  the x coordinate of the node

- `y`:

  the y coordinate of the node

- `...`:

  other attributes associated with the network nodes

Where `type = 'links'` the `data.frame` will have the following columns:

- `x1`:

  the x coordinate of the first node

- `y1`:

  the y coordinate of the first node

- `x2`:

  the x coordinate of the second node

- `y2`:

  the y coordinate of the second node

- `x1p`:

  the x coordinate at the perimeter of the first node

- `y1p`:

  the y coordinate at the perimeter of the first node

- `x2p`:

  the x coordinate at the perimeter of the second node

- `y2p`:

  the y coordinate at the perimeter of the second node

- `...`:

  other attributes associated with the network links

## Note

**Options for `type` parameter**

If a `SpatRaster` is supplied `type` is optional.

For `mpg` `type` options are `"node"` or `"links"`. This prepares the
nodes and links of the minimum planar graph network for plotting, Also
`"patchId"`, `"voronoi"`, `"lcpPerimWeight"`, `"lcpLinkId"`, `"mpgPlot"`
will prepare rasters for plotting.

For `grain` objects `type` options are `"nodes"` or`"links"` to prepare
the nodes and links of the grains of connectivity network for plotting.
Also `"voronoi"` will prepare the grains of connectivity Voronoi
polygons raster for plotting.

For either `mpg` or `grain` objects `type = "vorBound"` will identify
the boundaries of the Voronoi polygons for plotting. This is potentially
time consuming for large rasters.

## See also

[`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md),
[`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md)

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
## Extract a representative subset of 5 grains of connectivity
tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
if (interactive()) {
  library(ggplot2)

  ## Plot the patches in a minimum planar graph
  theme_set(theme_grainscape())
  ggplot() +
    geom_tile(data = ggGS(tinyPatchMPG, "patchId"), aes(x = x, y = y, fill = value))

  ## Plot the grain polygons in a grain of connectivity
  ggplot() +
    geom_tile(data = ggGS(grain(tinyPatchGOC, 3), "voronoi"), aes(x = x, y = y, fill = value))

  ## Plot the grain polygon boundaries
  ggplot() +
    geom_tile(data = ggGS(grain(tinyPatchGOC, 3), "vorBound"), aes(x = x, y = y, fill = value))

  ## Plot the patches and perimeter links of a minimum planar graph
  ggplot() +
    geom_tile(data = ggGS(tinyPatchMPG, "patchId"), aes(x = x, y = y, fill = value)) +
    geom_segment(data = ggGS(tinyPatchMPG, "links"), aes(x = x1p, y = y1p, xend = x2p, yend = y2p))

  ## Plot the patches and linear representations of the perimeter links
  ## of a minimum planar graph
  ggplot() +
    geom_tile(data = ggGS(tinyPatchMPG, "patchId"), aes(x = x, y = y, fill = value)) +
    geom_segment(data = ggGS(tinyPatchMPG, "links"), aes(x = x1p, y = y1p, xend = x2p, yend = y2p))

  ## Plot the nodes and links of a grains of connectivity network
  ## superimposed over the grain polygons
  focalGrain <- grain(tinyPatchGOC, 3)
  ggplot() +
    geom_tile(data = ggGS(focalGrain, "vorBound"), aes(x = x, y = y, fill = value)) +
    geom_point(data = ggGS(focalGrain, "nodes"), aes(x = x, y = y)) +
    geom_segment(data = ggGS(focalGrain, "links"), aes(x = x1, y = y1, xend = x2, yend = y2))
}
```
