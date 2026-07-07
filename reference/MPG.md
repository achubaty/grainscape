# Extract a minimum planar graph (MPG) model from a landscape resistance surface

Extracts a minimum planar graph (MPG) and is also the first step in
grains of connectivity (GOC) modelling. Both patch-based and lattice
MPGs can be extracted.

## Usage

``` r
MPG(cost, patch, ...)

# S4 method for class 'SpatRaster,SpatRaster'
MPG(cost, patch, ...)

# S4 method for class 'SpatRaster,numeric'
MPG(cost, patch, ...)
```

## Arguments

- cost:

  A `SpatRaster` giving a landscape resistance surface, where the values
  of each raster cell are proportional to the resistance to movement,
  dispersal, or gene flow for an organism in the landscape feature they
  represent. Missing values `NA` are acceptable (but see below).
  Negative values are not. To extract an MPG with Euclidean links (i.e.,
  and not least-cost path links) set `cost[] <- 1`.

- patch:

  A raster of class `SpatRaster` for a patch-based analysis OR an
  integer for a lattice analysis. If a raster is given it must be of the
  same extent, origin and projection as `cost` and be binary, without
  missing values, where patches=1 and non-patches=0. For lattice
  analyses, an integer gives the spacing in raster cells between focal
  points in the lattice.

- ...:

  Additional arguments (not used).

## Value

A
[`mpg()`](https://www.alexchubaty.com/grainscape/reference/mpg-class.md)
object.

## Details

Use this function to create a minimum planar graph (MPG) that can be
further analyzed using
[`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html)
routines. It is also the first step in grains of connectivity (GOC)
modelling.

## Note

Researchers should consider whether the use of a patch-based MPG or a
lattice MPG model is appropriate based on the patch-dependency of the
organism under study. Patch-based models make most sense when animals
are restricted to, or dependent on, a resource patch. Lattice models can
be used as a generalized and functional approach to scaling resistance
surfaces.

Rasters should be projected and not in geographic coordinates (i.e.
`terra::crs(cost)` should not contain `"longlat"`) or the function will
issue a warning. In unprojected cases consider using
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
to change to an appropriate coordinate system for the location and
extent of interest that balances both distance and areal accuracy. See
<https://spatialreference.org/> for location-specific suggestions. Use
of geographic coordinates will result in inaccurate areal and distance
measurements, rendering the models themselves inaccurate.

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
[`threshold()`](https://www.alexchubaty.com/grainscape/reference/threshold.md)

## Author

Paul Galpern, Sam Doctolero, Alex Chubaty

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
## Explore the graph structure and node/link attributes
graphdf(tinyPatchMPG)
#> [[1]]
#> [[1]]$v
#>    name patchId patchArea patchEdgeArea coreArea centroidX centroidY
#> 1     5       5        38            37        1  8.684211 95.157895
#> 2     7       7        30            28        2 19.700000 98.000000
#> 3     8       8        46            45        1 43.847826 96.021739
#> 4     9       9        61            58        3 60.795082 96.811475
#> 5    12      12         1             1        0 71.500000 99.500000
#> 6    14      14        65            64        1 84.084615 96.346154
#> 7    19      19         9             9        0 98.944444 95.055556
#> 8    22      22         7             7        0 20.214286 94.642857
#> 9    28      28        14            14        0 51.642857 86.714286
#> 10   29      29         2             2        0 99.000000 88.000000
#> 11   30      30       100            95        5 31.660000 74.740000
#> 12   31      31         4             4        0 98.750000 84.750000
#> 13   32      32         4             4        0  3.000000 83.000000
#> 14   37      37        11            11        0 12.045455 79.954545
#> 15   40      40         7             7        0 24.071429 76.500000
#> 16   41      41        72            67        5 18.680556 70.902778
#> 17   46      46         2             2        0 56.000000 72.500000
#> 18   48      48        32            32        0 26.593750 60.875000
#> 19   50      50         9             9        0 10.500000 61.166667
#> 20   54      54         5             5        0 37.700000 56.700000
#> 21   55      55         4             4        0 47.500000 57.250000
#> 22   56      56        39            39        0 58.448718 50.782051
#> 23   60      60         6             6        0 98.500000 51.666667
#> 24   61      61         4             4        0 71.500000 48.750000
#> 25   62      62        14            14        0 19.000000 43.214286
#> 26   64      64         4             4        0 10.750000 43.500000
#> 27   67      67        76            72        4 40.171053 37.276316
#> 28   68      68         4             4        0 85.000000 42.250000
#> 29   73      73         1             1        0 99.500000 39.500000
#> 30   74      74        32            32        0 17.125000 35.312500
#> 31   76      76        15            15        0 60.166667 34.766667
#> 32   78      78         2             2        0 99.000000 36.000000
#> 33   80      80        21            21        0 78.404762 31.166667
#> 34   84      84        18            18        0  6.555556 24.944444
#> 35   85      85        12            12        0 42.833333 25.500000
#> 36   86      86        55            55        0 86.063636 21.445455
#> 37   93      93        42            39        3 24.285714 14.190476
#> 38   95      95        31            31        0 55.919355 11.725806
#> 39  100     100        26            26        0 90.038462  3.807692
#> 40  103     103         1             1        0 54.500000  4.500000
#> 41  105     105         2             2        0 63.000000  1.000000
#> 42  106     106         2             2        0 35.000000  0.500000
#> 43  107     107         2             2        0 80.000000  0.500000
#> 
#> [[1]]$e
#>     e1  e2 linkId lcpPerimWeight startPerimX startPerimY endPerimX endPerimY
#> 1   80  86      1             10        82.5        28.5      81.5      28.5
#> 2   30  40      2             10        25.5        78.5      25.5      79.5
#> 3   29  31      3             10        98.5        85.5      98.5      86.5
#> 4   19  29      4             10        99.5        89.5      99.5      90.5
#> 5    7  22      5              5        18.5        95.5      18.5      96.5
#> 6    5   7      6              5        12.5        98.5      11.5      98.5
#> 7   73  78      7             15        99.5        38.5      99.5      37.5
#> 8   41  48      8             20        22.5        63.5      22.5      64.5
#> 9   30  41      9             12        24.5        71.5      24.5      72.5
#> 10  37  41     10             20        14.5        76.5      14.5      77.5
#> 11  67  85     11             20        41.5        27.5      41.5      30.5
#> 12  40  41     12             20        22.5        75.5      20.5      75.5
#> 13   9  12     13             20        67.5        99.5      70.5      99.5
#> 14  62  74     14             12        17.5        40.5      17.5      41.5
#> 15   8  28     15             25        50.5        90.5      48.5      92.5
#> 16  12  14     16             30        76.5        98.5      72.5      99.5
#> 17   8   9     17             32        50.5        95.5      53.5      94.5
#> 18  95 103     18             40        56.5         7.5      54.5       5.5
#> 19  74  84     19             40        10.5        26.5      13.5      30.5
#> 20  55  56     20             40        52.5        53.5      49.5      57.5
#> 21  48  54     21             35        35.5        55.5      30.5      56.5
#> 22   5  32     22             40         3.5        85.5       6.5      89.5
#> 23   9  28     23             40        52.5        88.5      54.5      93.5
#> 24  14  19     24             40        96.5        96.5      90.5      97.5
#> 25  62  64     25             44        15.5        41.5      11.5      41.5
#> 26  32  37     26             45         3.5        81.5       9.5      80.5
#> 27  54  55     27             50        45.5        57.5      39.5      56.5
#> 28  30  54     28             45        37.5        65.5      37.5      58.5
#> 29  68  80     29             45        80.5        35.5      83.5      40.5
#> 30   5  37     30             45        12.5        82.5      12.5      90.5
#> 31  64  74     31             52        14.5        38.5      11.5      41.5
#> 32 100 107     32             54        80.5         1.5      86.5       1.5
#> 33  41  50     33             55        10.5        63.5      11.5      70.5
#> 34  86 100     34             55        86.5         6.5      84.5      13.5
#> 35  56  76     35             60        58.5        37.5      57.5      46.5
#> 36  56  61     36             59        69.5        47.5      62.5      48.5
#> 37 103 105     37             65        54.5         3.5      62.5       2.5
#> 38  67  76     38             70        58.5        37.5      45.5      37.5
#> 39   8  30     39             69        36.5        85.5      40.5      91.5
#> 40   7   8     40             70        38.5        97.5      25.5      97.5
#> 41  95 105     41             80        62.5         2.5      58.5       7.5
#> 42  55  67     42             75        47.5        55.5      45.5      44.5
#> 43  93 106     43             80        34.5         1.5      31.5      13.5
#> 44  74  93     44             77        20.5        18.5      20.5      31.5
#> 45  84  93     45             80        18.5        17.5       9.5      23.5
#> 46  56  67     46             75        45.5        44.5      53.5      50.5
#> 47   7  30     47             80        32.5        85.5      24.5      92.5
#> 48  28  46     48             81        55.5        73.5      54.5      83.5
#> 49  60  73     49             88        98.5        39.5      98.5      49.5
#> 50   7  40     50             85        24.5        78.5      24.5      92.5
#> 51  85  95     51             85        52.5        15.5      45.5      23.5
#> 52  86 107     52             85        82.5        13.5      80.5       1.5
#> 53  30  55     53             85        45.5        57.5      37.5      65.5
#> 54  22  37     54             85        14.5        81.5      19.5      92.5
#> 55  14  31     55             85        97.5        85.5      87.5      91.5
#> 56  54  67     56             95        43.5        44.5      39.5      56.5
#> 57  78  86     57            100        88.5        26.5      98.5      34.5
#> 58  30  46     58            100        36.5        71.5      54.5      72.5
#> 59 105 107     59            105        64.5         0.5      78.5       0.5
#> 60  85  93     60            105        30.5        14.5      40.5      23.5
#> 61  50  64     61            105        10.5        45.5      10.5      58.5
#> 62  68  78     62            114        98.5        36.5      84.5      40.5
#> 63 103 106     63            115        53.5         4.5      35.5       1.5
#> 64  93  95     64            115        51.5        12.5      31.5      13.5
#> 65  61  68     65            123        83.5        42.5      70.5      46.5
#> 66  76  95     66            123        60.5        15.5      61.5      32.5
#> 67  68  73     67            129        85.5        41.5      98.5      39.5
#> 68  50  62     68            130        16.5        46.5      10.5      58.5
#> 69  46  56     69            132        56.5        56.5      55.5      71.5
#> 70  95 106     70            125        35.5         1.5      51.5       9.5
#> 71  46  55     71            125        45.5        57.5      55.5      71.5
#> 72  76  80     72            144        75.5        34.5      64.5      34.5
#> 73  60  68     73            130        96.5        51.5      86.5      44.5
#> 74  67  74     74            149        37.5        29.5      20.5      31.5
#> 75  31  60     75            162        97.5        83.5      97.5      53.5
#> 76  48  62     76            192        20.5        46.5      24.5      57.5
#> 77  86  95     77            201        81.5        20.5      60.5      15.5
#> 78  80  95     78            208        60.5        15.5      76.5      27.5
#> 79  14  46     79            220        78.5        93.5      56.5      73.5
#> 80  14  68     80            358        82.5        90.5      83.5      42.5
#> 81  14  61     81            432        78.5        93.5      72.5      50.5
#> 
#> 

## Find the mean patch area (see igraph manual for use of V() and E())
mean(igraph::V(tinyPatchMPG@mpg)$patchArea)
#> [1] 21.67442

## Quick visualization of the MPG
if (interactive()) {
  plot(tinyPatchMPG, col = c("grey", "black"), legend = FALSE)
}

## Additional graph extraction scenarios
## Produce a lattice MPG where focal points are spaced 10 cells apart
tinyLatticeMPG <- MPG(cost = tinyCost, patch = 10)
if (interactive()) {
  plot(tinyLatticeMPG)
}
```
