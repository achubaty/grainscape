# Produce a grains of connectivity model at multiple scales (patch-based or lattice GOC)

Produce a grains of connectivity (GOC) model at multiple scales
(resistance thresholds) by scalar analysis. Patch-based or lattice GOC
modelling can be done with this function.

## Usage

``` r
GOC(x, ...)

# S4 method for class 'mpg'
GOC(
  x,
  nThresh = NULL,
  doThresh = NULL,
  weight = "lcpPerimWeight",
  verbose = 0,
  ...
)
```

## Arguments

- x:

  A `mpg` object produced by
  [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md).
  For lattice GOC `MPG` must be run with patch set as an integer value.

- ...:

  Additional arguments (not used).

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
  models. Use
  [`threshold()`](https://www.alexchubaty.com/grainscape/reference/threshold.md)
  to identify thresholds of interest. Provide either `nThresh` or
  `doThresh` not both.

- weight:

  A string giving the link weight or attribute to use for threshold.
  `"lcpPerimWeight"` uses the accumulated resistance or least-cost path
  distance from the perimeters of patches as the link weight.

- verbose:

  Set `verbose=0` for no progress information to console.

## Value

A
[`goc()`](https://www.alexchubaty.com/grainscape/reference/goc-class.md)
object.

## Details

Grain or scalar analysis of connectivity may be appropriate for a
variety of purposes, not limited to visualization and improving
connectivity estimates for highly-mobile organisms. See Galpern *et al.*
(2012), Galpern & Manseau (2013a, 2013b) for applications and review of
these capabilities.

## Note

Researchers should consider whether the use of a patch-based GOC or a
lattice GOC model is appropriate based on the patch-dependency of the
organism under study. Patch-based models make most sense when animals
are restricted to, or dependent on, a resource patch. Lattice models can
be used as a generalized and functional approach to scaling resistance
surfaces.

See [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md)
for warning related to areal measurements.

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

[`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md),
[`grain()`](https://www.alexchubaty.com/grainscape/reference/grain.md),
[`distance()`](https://www.alexchubaty.com/grainscape/reference/distance.md),
[`point()`](https://www.alexchubaty.com/grainscape/reference/point.md)

## Author

Paul Galpern

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
## Examine the properties of the GOC graph of grain 3 of 5
graphdf(grain(tinyPatchGOC, whichThresh = 3))
#> [[1]]
#> [[1]]$v
#>    name polygonId centroidX centroidY polygonArea totalPatchArea
#> 1    14        14 92.042714  6.057789         199             26
#> 2     1         1 31.420170 71.401486        2355            349
#> 3     9         9 82.782258 30.815412        1116             80
#> 4    13        13 55.778330 12.235586         503             32
#> 5     8         8 40.950139 33.540166         722             88
#> 6     2         2 72.223608 87.089251        1563            202
#> 7    12        12 22.579545 12.465909         616             42
#> 8     7         7 11.834690 32.172431         983             68
#> 9     5         5 92.936709 58.797468         316              6
#> 10   10        10 96.461240 35.647287         129              3
#> 11   15        15 64.756757  3.216216          74              2
#> 12    4         4  8.658451 58.595070         284              9
#> 13    6         6 73.383598 51.611111         189              4
#> 14   11        11 60.343206 33.088850         287             15
#> 15   17        17 76.234513  4.216814         113              2
#> 16   16        16 36.907692  3.884615         130              2
#> 17    3         3 60.785036 71.659145         421              2
#>    totalPatchEdgeArea totalCoreArea
#> 1                  26             0
#> 2                 336            13
#> 3                  80             0
#> 4                  32             0
#> 5                  84             4
#> 6                 197             5
#> 7                  39             3
#> 8                  68             0
#> 9                   6             0
#> 10                  3             0
#> 11                  2             0
#> 12                  9             0
#> 13                  4             0
#> 14                 15             0
#> 15                  2             0
#> 16                  2             0
#> 17                  2             0
#>                                         patchId
#> 1                                           100
#> 2  5, 7, 22, 30, 32, 37, 40, 41, 48, 54, 55, 56
#> 3                                    68, 80, 86
#> 4                                       95, 103
#> 5                                        67, 85
#> 6                  8, 9, 12, 14, 19, 28, 29, 31
#> 7                                            93
#> 8                                62, 64, 74, 84
#> 9                                            60
#> 10                                       73, 78
#> 11                                          105
#> 12                                           50
#> 13                                           61
#> 14                                           76
#> 15                                          107
#> 16                                          106
#> 17                                           46
#> 
#> [[1]]$e
#>    e1 e2 maxWeight linkIdMaxWeight minWeight linkIdMinWeight medianWeight
#> 1  14 17        54              32        54              32         54.0
#> 2   1  4        55              33        55              33         55.0
#> 3  14  9        55              34        55              34         55.0
#> 4   1 11        60              35        60              35         60.0
#> 5   1  6        59              36        59              36         59.0
#> 6  13 15        80              41        65              37         72.5
#> 7   8 11        70              38        70              38         70.0
#> 8   1  2        70              40        69              39         69.5
#> 9   1  8        95              56        75              42         75.0
#> 10 12 16        80              43        80              43         80.0
#> 11 12  7        80              45        77              44         78.5
#> 12  2  3       220              79        81              48        150.5
#> 13  5 10        88              49        88              49         88.0
#> 14 13  8        85              51        85              51         85.0
#> 15  9 17        85              52        85              52         85.0
#> 16  9 10       129              67       100              57        114.0
#> 17  1  3       132              69       100              58        125.0
#> 18 15 17       105              59       105              59        105.0
#> 19  8 12       105              60       105              60        105.0
#> 20  7  4       130              68       105              61        117.5
#> 21 13 16       125              70       115              63        120.0
#> 22 13 12       115              64       115              64        115.0
#> 23  9  6       123              65       123              65        123.0
#> 24 13 11       123              66       123              66        123.0
#> 25  9 11       144              72       144              72        144.0
#> 26  9  5       130              73       130              73        130.0
#> 27  8  7       149              74       149              74        149.0
#> 28  2  5       162              75       162              75        162.0
#> 29  1  7       192              76       192              76        192.0
#> 30  9 13       208              78       201              77        204.5
#> 31  9  2       358              80       358              80        358.0
#> 32  2  6       432              81       432              81        432.0
#>    meanWeight numEdgesWeight  linkIdAll eucCentroidWeight
#> 1    54.00000              1         32          15.91504
#> 2    55.00000              1         33          26.11705
#> 3    55.00000              1         34          26.43286
#> 4    60.00000              1         35          48.00417
#> 5    59.00000              1         36          46.39599
#> 6    72.50000              2     37, 41          12.72640
#> 7    70.00000              1         38          19.39832
#> 8    69.50000              2     39, 40          43.71529
#> 9    81.66667              3 42, 46, 56          39.04228
#> 10   80.00000              1         43          16.70133
#> 11   78.50000              2     44, 45          22.44547
#> 12  150.50000              2     48, 79          19.20753
#> 13   88.00000              1         49          23.41694
#> 14   85.00000              1         51          25.95689
#> 15   85.00000              1         52          27.39267
#> 16  114.33333              3 57, 62, 67          14.50729
#> 17  119.00000              3 58, 69, 71          29.36600
#> 18  105.00000              1         59          11.52129
#> 19  105.00000              1         60          27.95716
#> 20  117.50000              2     61, 68          26.61286
#> 21  120.00000              2     63, 70          20.63588
#> 22  115.00000              1         64          33.19958
#> 23  123.00000              1         65          22.82095
#> 24  123.00000              1         66          21.34705
#> 25  144.00000              1         72          22.55393
#> 26  130.00000              1         73          29.76757
#> 27  149.00000              1         74          29.14756
#> 28  162.00000              1         75          35.06362
#> 29  192.00000              1         76          43.84643
#> 30  204.50000              2     77, 78          32.77838
#> 31  358.00000              1         80          57.25583
#> 32  432.00000              1         81          35.49710
#> 
#> 

## Extract grains of connectivity
## representation of the finest grain and three others
## by giving thresholds in link weights (doThresh)
tinyPatchGOC <- GOC(tinyPatchMPG, doThresh = c(0, 20, 40))
```
