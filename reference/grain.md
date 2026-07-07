# Extract a grain of connectivity (GOC) tessellation at a given scale

Extract a grain (i.e. a scaled version of a Voronoi tessellation) from a
GOC model.

## Usage

``` r
grain(x, ...)

# S4 method for class 'goc'
grain(x, whichThresh, ...)
```

## Arguments

- x:

  A `goc` object created by
  [`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md).

- ...:

  Additional arguments (not used).

- whichThresh:

  Integer giving the grain threshold to extract. This is the index of
  the threshold extracted by
  [`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md).

## Value

A list object containing the following elements:

- `summary`:

  gives the properties of the specified scale/grain `whichThresh` of the
  GOC model;

- `voronoi`:

  a `SpatRaster` giving the Voronoi tessellation the specified
  scale/grain `whichThresh` of the GOC model;

- `centroids`:

  an `sf` object giving the centroids of the polygons in the Voronoi
  tessellation at the specified scale/grain `whichThresh`;

- `th`:

  a `igraph` object giving the graph describing the relationship among
  the polygons at the specified scale/grain `whichThresh`

## References

Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs:
Principles and applications for habitat connectivity. Ecosystems
10:448:461.

Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing
methods for scaling resistance surfaces. Landscape Ecology 28:1269-1291.

Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape
connectivity on animal distribution: a functional grain approach.
Ecography 36:1004-1016.

Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape
connectivity: a guide to construction, analysis, and application for
conservation. Biological Conservation 144:44-55.

Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity:
analysis at multiple spatial scales in landscape genetics. Molecular
Ecology 21:3996-4009.

## See also

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
## Very quick visualization at the finest scale/grain/threshold
tinyPatchGOCgrain <- grain(tinyPatchGOC, whichThresh = 1)
if (interactive()) {
  plot(tinyPatchGOCgrain, col = topo.colors(10))
}

## Visualize the model at the finest scale/grain/threshold
## Manual control of plotting
if (interactive()) {
  plot(
    grain(tinyPatchGOC, whichThresh = 1)@voronoi,
    col = sample(rainbow(100)),
    legend = FALSE,
    main = "Threshold 1"
  )
}
```
