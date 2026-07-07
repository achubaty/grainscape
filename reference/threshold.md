# Produce a minimum planar graph (MPG) at multiple scales

Perform a scalar analysis of a minimum planar graph (MPG) by building
the graph at a series of link thresholds. As the threshold value
increases more nodes in the graph become connected, forming increasingly
fewer components, until the graph becomes connected (e.g., Brooks,
2003). N.B. Grains of connectivity (GOC) done by
[`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md) is
also a scalar analysis using Voronoi tessellations rather than patches
(see Galpern *et al.*, 2012).

## Usage

``` r
threshold(x, ...)

# S4 method for class 'mpg'
threshold(x, weight = "lcpPerimWeight", nThresh = NULL, doThresh = NULL, ...)
```

## Arguments

- x:

  A `mpg` object produced by
  [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md).

- ...:

  Additional arguments (not used).

- weight:

  A string giving the link weight or attribute to use for threshold.
  `"lcpPerimWeight"` uses the accumulated resistance or least-cost path
  distance from the perimeters of patches as the link weight.

- nThresh:

  Optional. An integer giving the number of thresholds (or scales) at
  which to create GOC models. Thresholds are selected to produce a
  maximum number of unique grains (i.e., models). `nThresh` thresholds
  are also approximately evenly spread between 0 and the threshold at
  which all patches or focal points on the landscape are connected. This
  is a simple way to get a representative subset of all possible GOC
  models. Provide either `nThresh` or `doThresh` not both.

- doThresh:

  Optional. A vector giving the link thresholds at which to create GOC
  models. Use `threshold()` to identify thresholds of interest. Provide
  either `nThresh` or `doThresh` not both.

## Value

A list object with the following elements:

- `summary`:

  summarizes the thresholded graphs generated and their properties;

- `th`:

  a list of length `nThresh` or `length(doThresh)` giving the
  thresholded graph (class `igraph`) at each threshold.

## Note

See [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md)
for warning related to areal measurements.

## References

Brooks, C.P. (2003) A scalar analysis of landscape connectivity. Oikos
102:433-439.

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

[`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md)

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
## Threshold this graph at a representative subset of 10 thresholds
tinyThresh <- threshold(tinyPatchMPG, nThresh = 10)

## Examine the properties of one of these threshold graphs
print(tinyThresh$th[[7]], vertex.attributes = TRUE, edge.attributes = TRUE)
#> IGRAPH 0f2939b UN-- 43 79 -- 
#> + attr: name (v/c), patchId (v/n), patchArea (v/n), patchEdgeArea
#> | (v/n), coreArea (v/n), centroidX (v/n), centroidY (v/n), linkId
#> | (e/n), lcpPerimWeight (e/n), startPerimX (e/n), startPerimY (e/n),
#> | endPerimX (e/n), endPerimY (e/n)
#> + edges from 0f2939b (vertex names):
#>  [1] 80 --86  30 --40  29 --31  19 --29  7  --22  5  --7   73 --78  41 --48 
#>  [9] 30 --41  37 --41  67 --85  40 --41  9  --12  62 --74  8  --28  12 --14 
#> [17] 8  --9   95 --103 74 --84  55 --56  48 --54  5  --32  9  --28  14 --19 
#> [25] 62 --64  32 --37  54 --55  30 --54  68 --80  5  --37  64 --74  100--107
#> [33] 41 --50  86 --100 56 --76  56 --61  103--105 67 --76  8  --30  7  --8  
#> + ... omitted several edges
```
