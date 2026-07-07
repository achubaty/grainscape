# Find the grains of connectivity network distance

Find the shortest network distance between pairs of points using the GOC
graph. This can be used as an effective distance for landscape
connectivity assessments.

## Usage

``` r
distance(x, y, ...)

# S4 method for class 'goc,sf'
distance(x, y, weight = "meanWeight", ...)

# S4 method for class 'goc,matrix'
distance(x, y, weight = "meanWeight", ...)

# S4 method for class 'goc,numeric'
distance(x, y, weight = "meanWeight", ...)
```

## Arguments

- x:

  A `goc` object produced by
  [`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md).

- y:

  A two-column matrix or an `sf` (POINT) object giving the coordinates
  of points of interest.

- ...:

  Additional arguments (not used).

- weight:

  The GOC graph link weight to use in calculating the distance. Please
  see Details for explanation.

## Value

A list object giving a distance matrix for each threshold in the `GOC`
object. Distance matrices give the pairwise grains of connectivity
network distances between sampling locations. Matrix indices correspond
to rows in the coordinates matrix (`y`).

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

[`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md),
[`point()`](https://www.alexchubaty.com/grainscape/reference/point.md)

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
## Three sets of coordinates in the study area
loc <- cbind(c(30, 60, 90), c(30, 60, 90))

## Find the GOC network distance matrices between these points
## for each of the 5 grains of connectivity
tinyDist <- grainscape::distance(tinyPatchGOC, loc)
```
