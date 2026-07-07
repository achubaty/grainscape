# Test maps included with `grainscape`

Simple, artificial land cover maps intended for users to explore the
functionality of the package. Each map has discrete integer land cover
classes representing distinct land cover types. Typical analyses begin
by reclassifying these to reflect resistance to movement (see the
examples below and the package vignette).

## Format

Three *ESRI ArcASCII* (`.asc`) raster grids stored in `extdata` and read
with
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html):
`patchy.asc` and `fragmented.asc` are 400 x 400 cells, `tiny.asc` is 100
x 100 cells. Cell values are integer land cover classes (1-5 for
`patchy.asc`, 1-4 for `fragmented.asc` and `tiny.asc`), with no
associated coordinate reference system.

## Source

Simulated, artificial land cover maps created for demonstrating
`grainscape`. `patchy.asc` also illustrates graph-based connectivity
methods in Galpern *et al.* (2011) (see references in the package
documentation,
[grainscape-package](https://www.alexchubaty.com/grainscape/reference/grainscape-package.md)).

## Details

The maps are distributed as *ESRI ArcASCII* (`.asc`) grids in the
package's `extdata` directory and are read with
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).
They use an arbitrary unprojected coordinate system (no CRS) with a unit
cell size.

- `patchy.asc`:

  A caricatured map of five land cover classes (integer values 1-5),
  where patches are large, easy to identify polygonal regions for
  heuristic purposes. This unrealistic map can be used to illustrate the
  method and understand how it works. The map also serves a similar
  heuristic purpose in a review of graph-based connectivity methods
  (Galpern *et al.*, 2011). (400 x 400 raster cells.)

- `fragmented.asc`:

  A simulated land cover map with four land cover classes (integer
  values 1-4), produced using an algorithm that generates fragmentation.
  (400 x 400 raster cells.)

- `tiny.asc`:

  Similar to `fragmented.asc` (four land cover classes, integer values
  1-4) but smaller in extent for lightning-fast computation and
  experimental use. (100 x 100 raster cells.)

## Examples

``` r
## load a bundled map and reclassify it into a resistance surface
patchy <- terra::rast(
  system.file("extdata", "patchy.asc", package = "grainscape", mustWork = TRUE)
)
patchyCost <- terra::classify(patchy, rcl = cbind(1:5, c(1, 10, 8, 3, 6)))
terra::plot(patchyCost)
```
