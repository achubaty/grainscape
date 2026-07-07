# Filter out patches smaller than a specified area

Pre-process patch rasters prior to their use with
[`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md).

## Usage

``` r
patchFilter(x, cells = NULL, area = NULL, ...)

# S4 method for class 'SpatRaster'
patchFilter(x, cells = NULL, area = NULL, ...)
```

## Arguments

- x:

  A binary raster (i.e. consisting of `0`, `1`, or `NA` cells), where
  cells `=1` represent patches

- cells:

  The minimum number of cells that constitute a patch. Default `NULL`.
  Only one of `cells` or `area` may be specified.

- area:

  The minimum area that constitutes a patch (where area is calculated in
  the coordinate reference system of the raster by multiplying the count
  of cells in patch by the x and y resolution of a raster cell). Default
  `NULL`. Only one of `cells` or `area` may be specified.

- ...:

  Additional arguments passed to
  [`terra::patches()`](https://rspatial.github.io/terra/reference/patches.html).
  For example, `directions = 4` may be used to be more conservative
  about which cells constitute a patch.

## Value

A binary raster where all patches (i.e. clumped areas `=1`) are greater
than the specified area.

## Details

It examines a binary raster to identify all patches or clumps of cells
with values `=1`, determines their area, and returns a binary raster
where only patches of area greater than or equal to the specified amount
are represented.

This is helpful when analyzing habitat connectivity models where patches
represent a land cover or habitat type. For example, a raster may have
patches of a certain habitat type of insufficient area to support the
ecological process of interest. Another use case is remote sensing
classification errors that have introduced artifacts. Filtering can help
in both cases.

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
## Produce a patch-based MPG where patches are resistance features = 10
## and all patches are greater than or equal to 2 cells in size
filteredPatch <- patchFilter(tinyCost == 10, cells = 2)
tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
if (interactive()) {
  plot(tinyPatchMPG)
}

## Compare to removal of patches greater than or equal to 40 cells in size!
filteredPatch <- patchFilter(tinyCost == 10, cells = 40)
tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
if (interactive()) {
  plot(tinyPatchMPG)
}

## Use a rook/castle 4-direction case rather than the queen 8-direction case
## to identify neighbouring cells in a patch
filteredPatch <- patchFilter(tinyCost == 10, cells = 40, directions = 4)
tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
if (interactive()) {
  plot(tinyPatchMPG)
}
```
