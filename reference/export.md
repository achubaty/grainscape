# Export spatial data from MPG and GOC models

This function automates the export of raster and vector spatial data
from `mpg` and `grain` objects. By default it places them in a new
directory, unless an existing one is specified with `overwrite = TRUE`.

It can also be used to process `mpg` and `grain` objects to produce R
spatial objects that are convenient for plotting or analysis within R.
Use `R = TRUE` in which case all parameters related to file export are
ignored. (Default `R = FALSE`)

The
[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
function is used for rasters, and
[`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html)
is used to export ESRI compatible shape files.

## Usage

``` r
export(
  x,
  dirname = NULL,
  path = ".",
  rasterFormat = "GTiff",
  overwrite = FALSE,
  R = FALSE,
  vorBound = FALSE,
  ...
)

# S4 method for class 'mpg'
export(
  x,
  dirname = NULL,
  path = ".",
  rasterFormat = "GTiff",
  overwrite = FALSE,
  R = FALSE,
  vorBound = FALSE,
  ...
)

# S4 method for class 'grain'
export(
  x,
  dirname = NULL,
  path = ".",
  rasterFormat = "GTiff",
  overwrite = FALSE,
  R = FALSE,
  vorBound = FALSE,
  ...
)

# S4 method for class 'goc'
export(
  x,
  dirname = NULL,
  path = ".",
  rasterFormat = "GTiff",
  overwrite = FALSE,
  R = FALSE,
  vorBound = FALSE,
  ...
)
```

## Arguments

- x:

  A `mpg` or `grain` object

- dirname:

  The name of a new directory to create. If `NULL` a directory with a
  name containing the date and time will be created.

- path:

  A path to where this new directory `dirname` should be created.
  Defaults to the working directory.

- rasterFormat:

  The format for exported rasters. See
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
  for options. Defaults to GeoTiff (`rasterFormat='GTiff'`).

- overwrite:

  If directory already exists will overwrite existing files inside.
  Defaults to `FALSE`.

- R:

  If `TRUE`, return the spatial objects that would be written to files.
  Do not write these files and ignore `dirname`, `path`, `rasterFormat`,
  `overwrite` parameters. This is useful for visualization using R
  plotting functions, or spatial analysis within R. Defaults to `FALSE`

- vorBound:

  Specify whether to create a raster with the boundaries of the Voronoi
  polygons `=1` and the remainder `=NA`. This may be useful for
  visualizing relationships among polygons in a grain of connectivity.
  This can add time to the export on very large rasters. Defaults to
  `FALSE`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the path to the folder created.

Side effect of exporting files representing raster and vector spatial
data in the object.

Please note that for vector data export the attribute name is limited to
8 characters in shape files. See the tables below for the abbreviations
used and their meaning.

**Exported from `mpg` objects:**

`nodes`, `linksCentroid`, `linksPerim` are shape files giving the
locations of the patch centroids, links among centroids, and links among
perimeters of patches respectively. `patchId, voronoi` are rasters
giving the patch identifier of the patch, or of the patch that the
Voronoi polygon refers to. `lcpPerimWeight, lcpLinkId` give the weight
in cost surface units of the shortest paths between perimeters, and the
identifiers of those links respectively. `vorBound` gives the boundaries
of the Voronoi polygons (if specified).

Description of node (vertex) and link (edge) weights in `mpg` objects
and their corresponding attribute names in the shape files created.

|  |  |  |  |
|----|----|----|----|
| **type** | **MPG name** | **SHP name** | **Description** |
| node | `patchId` | `patchId` | Patch ID from `patchId` raster |
| node | `patchArea` | `patchA` | Area of patch |
| node | `patchEdgeArea` | `patchEA` | Edge area of patch |
| node | `coreArea` | `coreA` | Area excluding edge of patch |
| node | `centroidX` | `ctrX` | Centroid of the patch (X) |
| node | `centroidY` | `ctrY` | Centroid of the patch (Y) |
| link | `e1` | `e1` | Id of first patch at end of link |
| link | `e2` | `e2` | Id of second patch at end of link |
| link | `linkId` | `linkId` | Link ID from `lcpLinkId` raster |
| link | `lcPerimWeight` | `lcpPerWt` | Cost length of link from patch perimeters |
| link | `startPerimX` | `strtPerX` | Coordinate of link endpoint on first patch (X) |
| link | `startPerimY` | `strtPerY` | Coordinate of link endpoint on first patch (Y) |
| link | `endPerimX` | `endPerX` | Coordinate of link endpoint on second patch (X) |
| link | `endPerimY` | `endPerY` | Coordinate of link endpoint on second patch (Y) |

**Exported from `grain` objects:**

`nodes, linksCentroid` are shape files giving the locations of the
Voronoi polygon centroids and links among them respectively. `voronoi`
are rasters gives the polygon identifier of each cluster of patches.
`vorBound` gives the boundaries of the Voronoi polygons (if specified).

Description of node (vertex) and link (edge) weights in `grain` objects
and their corresponding attribute names in the shape files created.

|  |  |  |  |
|----|----|----|----|
| **Type** | **GOC name** | **SHP name** | **Description** |
| node | `polygonId` | `polyId` | Polygon ID from grain voronoi raster |
| node | `polygonArea` | `polyA` | Area of polygon from grain voronoi raster |
| node | `totalPatchArea` | `patchA` | Total area of all patches in polygon |
| node | `totalPatchEdgeArea` | `patchEA` | Total area of all patch edges in polygon |
| node | `totalCoreArea` | `coreA` | Total area of patches in polygon excluding edges |
| node | `centroidX` | `ctrX` | Centroid of the polygon (X) |
| node | `centroidY` | `ctrY` | Centroid of the polygon (Y) |
| link | `e1` | `e1` | ID of first patch at end of link |
| link | `e2` | `e2` | ID of second patch at end of link |
| link | `maxWeight` | `maxWt` | The maximum weight of all links connecting patches between polygons |
| link | `linkIdMaxWeight` | `maxWt` | The link id of that maximum weight `link (lcpLinkId)` |
| link | `minWeight` | `min` | The minimum weight of all links connecting patches between polygons |
| link | `linkIdMinWeight` | `minWt` | The link id of that minimum weight `link (lcpLinkId)` |
| link | `medianWeight` | `medWt` | The median weight of all links connecting patches between polygons |
| link | `meanWeight` | `meanWT` | The minimum weight of all links connecting patches between polygons |
| link | `numlinksWeight` | `numEWt` | The number of links connecting patches between polygons |
| link | `eucCentroidWeight` | `eucCtrWt` | The Euclidean distance between centroids of polygons |

## See also

[`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md),
[`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md),
[`grain()`](https://www.alexchubaty.com/grainscape/reference/grain.md)

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
## Export rasters and vectors and place in an R object
sp_tinyPatchGOC <- export(grain(tinyPatchGOC, 2), R = TRUE)
sp_tinyPatchMPG <- export(tinyPatchMPG, R = TRUE)

## Export raster and vectors to a specified directory
exportPath <- tempdir()
export(grain(tinyPatchGOC, 2), dirname = "tiny_goc_thresh2", path = exportPath)
#> writing: substituting ENGCRS["Undefined Cartesian SRS with unknown unit"] for missing CRS
#> Writing layer `nodes' to data source 
#>   `/tmp/RtmpaGKvQp/tiny_goc_thresh2' using driver `ESRI Shapefile'
#> Writing 28 features with 5 fields and geometry type Point.
#> writing: substituting ENGCRS["Undefined Cartesian SRS with unknown unit"] for missing CRS
#> Writing layer `linksCentroid' to data source 
#>   `/tmp/RtmpaGKvQp/tiny_goc_thresh2' using driver `ESRI Shapefile'
#> Writing 56 features with 15 fields and geometry type Line String.
#> Exported to:/tmp/RtmpaGKvQp/tiny_goc_thresh2
export(tinyPatchMPG, dirname = "tiny_mpg", path = exportPath, vorBound = TRUE)
#> Extracting voronoi boundaries...
#> writing: substituting ENGCRS["Undefined Cartesian SRS with unknown unit"] for missing CRS
#> Writing layer `nodes' to data source 
#>   `/tmp/RtmpaGKvQp/tiny_mpg' using driver `ESRI Shapefile'
#> Writing 43 features with 4 fields and geometry type Point.
#> writing: substituting ENGCRS["Undefined Cartesian SRS with unknown unit"] for missing CRS
#> Writing layer `linksCentroid' to data source 
#>   `/tmp/RtmpaGKvQp/tiny_mpg' using driver `ESRI Shapefile'
#> Writing 81 features with 12 fields and geometry type Line String.
#> writing: substituting ENGCRS["Undefined Cartesian SRS with unknown unit"] for missing CRS
#> Writing layer `linksPerim' to data source 
#>   `/tmp/RtmpaGKvQp/tiny_mpg' using driver `ESRI Shapefile'
#> Writing 81 features with 8 fields and geometry type Line String.
#> Exported to:/tmp/RtmpaGKvQp/tiny_mpg

## clean up
unlink(file.path(exportPath, "tiny_goc_thresh2"), recursive = TRUE)
unlink(file.path(exportPath, "tiny_mpg"), recursive = TRUE)
```
