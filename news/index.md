# Changelog

## grainscape 1.0.0

### Dependency changes

- Migrated from `raster`/`sp` to `terra`/`sf` for all spatial
  operations:
  - `terra` and `sf` added to `Imports`;
  - `raster` and `sp` dependencies removed entirely.
- Vignette diagrams are now generated from code with `DiagrammeRsvg` and
  `rsvg` (added to `Suggests`); `webshot2` is no longer needed for the
  vignettes and was removed from `Suggests`.

### Breaking changes

- [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md),
  [`patchFilter()`](https://www.alexchubaty.com/grainscape/reference/patchFilter.md),
  and
  [`ggGS()`](https://www.alexchubaty.com/grainscape/reference/ggGS.md)
  no longer accept `RasterLayer` inputs; pass `SpatRaster` objects (from
  `terra`) instead;
- [`corridor()`](https://www.alexchubaty.com/grainscape/reference/corridor.md),
  [`distance()`](https://www.alexchubaty.com/grainscape/reference/distance.md),
  and
  [`point()`](https://www.alexchubaty.com/grainscape/reference/point.md)
  no longer accept `SpatialPoints` inputs; pass a two-column matrix or
  `sf` object instead;
- Slot types in `mpg`, `goc`, `grain`, and `corridor` S4 classes have
  changed from
  `RasterLayer`/`SpatialPoints`/`SpatialLines`/`SpatialLinesDataFrame`
  to `SpatRaster`/`sf`;
- The long-deprecated `gs`-prefixed functions
  ([`gsMPG()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsGOC()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsGOCCorridor()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsGOCDistance()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsGOCPoint()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsGOCVisualize()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsGraphDataFrame()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md),
  [`gsMPGstitch()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md))
  are now defunct and error with a pointer to their replacement
  (deprecated since 0.4.0;
  [\#10](https://github.com/achubaty/grainscape/issues/10));

### New features

- New exported plotting helpers
  [`plotResistance()`](https://www.alexchubaty.com/grainscape/reference/plotResistance.md)
  and
  [`plotWithResistance()`](https://www.alexchubaty.com/grainscape/reference/plotWithResistance.md):
  draw a resistance surface with a consistent colour scheme (and a
  discrete-value legend), and place it as an equally-sized reference
  panel beside a network plot for visual comparison. Both are
  demonstrated in the vignette’s introductory figures
  ([`plotWithResistance()`](https://www.alexchubaty.com/grainscape/reference/plotWithResistance.md)
  uses `cowplot`).

### Bugfixes

- Fixed incorrect least-cost paths in MPG
  ([\#72](https://github.com/achubaty/grainscape/issues/72));
- Fixed a regression in the
  [\#72](https://github.com/achubaty/grainscape/issues/72) work where
  legitimate, non-redundant MPG links were dropped on high-variance
  resistance surfaces: the indirect-path search could treat an unset
  cell id (the internal `-99` sentinel) as a pivot patch, fabricating a
  bogus two-hop route that suppressed a valid direct link. Such
  malformed links are now rejected and non-patch pivots are ignored
  ([\#72](https://github.com/achubaty/grainscape/issues/72));
- Fixed patches being left isolated in the MPG: a patch cell that was
  never added as an active cell during spreading (e.g. a single-cell
  patch) kept the internal `-99` id, so it was an unresolved link
  endpoint and all of its links were silently dropped. Every cell’s id
  is now seeded from the voronoi map at initialization, so such patches
  are linked correctly
  ([\#72](https://github.com/achubaty/grainscape/issues/72));
- Fixed an infinite loop (hang) in
  [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md): a
  resistance surface whose first cell is `NA` left the C++ engine’s
  internal cost range as `NaN`, so every patch cell was assigned a `NaN`
  resistance and never “settled”, spinning forever. The engine now
  derives the cost range while skipping `NA` cells, and fully
  initializes its spreading-state struct, hardening it against this
  class of hang
  ([\#72](https://github.com/achubaty/grainscape/issues/72));
- Fixed a Voronoi tessellation bias on high-variance resistance
  surfaces: patch (source) cells now begin spreading at the uniform
  minimum cost rather than the resistance of the cell they happen to
  overlap. Previously a patch sitting on a high-resistance cell spread
  late and lost territory to its neighbours, distorting the tessellation
  (and, in the extreme, leaving a patch with a single-cell region and no
  links). The fix changes only the tessellation: link weights are
  accumulated from the entered cells’ resistances (independent of the
  source cell), so on the package’s test surfaces the MPG and its
  weights are unchanged, and still match independent least-cost paths
  exactly ([\#72](https://github.com/achubaty/grainscape/issues/72));
- Fixed `graphdf` returning a transposed (single-column) data frame for
  MPGs with only one link;
- [`patchFilter()`](https://www.alexchubaty.com/grainscape/reference/patchFilter.md)
  no longer errors on R \< 4.6 (with recent `terra`): it masked small
  patches via `out[out %in% rmpatch] <- NA`, which relies on `%in%`
  dispatching for a `SpatRaster` and fails with “‘match’ requires vector
  arguments” under older R; it now uses
  [`terra::subst()`](https://rspatial.github.io/terra/reference/subst.html)
  ([\#76](https://github.com/achubaty/grainscape/issues/76));
- [`point()`](https://www.alexchubaty.com/grainscape/reference/point.md)
  (and
  [`distance()`](https://www.alexchubaty.com/grainscape/reference/distance.md),
  which calls it) now emit the “coords correspond to NA cells” warning
  once per call instead of once per coordinate-threshold combination
  ([\#50](https://github.com/achubaty/grainscape/issues/50));
- The `corridor`
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method now
  uses the `linewidth` aesthetic (rather than the deprecated `size`
  aesthetic) for its link segments, silencing a `ggplot2` (\>= 3.4.0)
  deprecation warning;

### Vignette

- The “Calculating the minimum planar graph” vignette now generates its
  UML class diagrams, the algorithm flow chart, and the `Engine` call
  graphs directly from Graphviz/`DiagrammeR` specifications (rendered to
  vector PDFs with `DiagrammeRsvg` and `rsvg`) rather than embedding
  static images, so they stay in sync with the C++ engine; its technical
  reference was also updated for the
  [\#72](https://github.com/achubaty/grainscape/issues/72)
  least-cost-path changes (the `settled_map`, `createLinks()`, and the
  `parentResistance` field)
  ([\#75](https://github.com/achubaty/grainscape/issues/75));
- Corrected the thresholded-MPG figure (Figure 6), which plotted links
  *above* the dispersal threshold instead of the retained links *below*
  it (so the cheap, low-resistance links between neighbouring patches
  were hidden);
- Made the Euclidean-vs-resistance path-distance table robust by
  matching rows on the node pair (rather than column-binding two
  independently sorted tables), and the “patch 7 neighbours” table now
  filters explicitly for patch 7. The minimum-planar-graph figure above
  that table now labels exactly node 7 and its linked neighbours,
  derived from the graph rather than a hard-coded list of patch ids.

## grainscape 0.5.0

CRAN release: 2025-01-15

### Dependency changes

- none

### Bugfixes

- none

### New features & enhancements

- none

## grainscape 0.5.0

CRAN release: 2025-01-15

### Dependency changes

- Require R 4.2 or higher (for native pipe and placeholder)
- Add `DiagrammeR`, `dplyr`, `webshot2` and `withr` to Suggests for use
  with vignettes and tests

### Bugfixes

- Fixed documentation issues and ‘memory not mapped’ segfault errors on
  R-devel ([\#73](https://github.com/achubaty/grainscape/issues/73))
- Improved handling of user-added attributes in exported objects
  ([\#71](https://github.com/achubaty/grainscape/issues/71))
- Fixed partial match warnings
- Use replacements for deprecated `igraph` functions

### New features & enhancements

- Use native R pipe throughout instead of `magrittr` pipe
- Improved documentation

## grainscape 0.4.4

CRAN release: 2023-04-20

### Dependency changes

- Removed retiring spatial packages `rgdal` and `rgeos`
  ([\#68](https://github.com/achubaty/grainscape/issues/68));
- Use `sf` for writing shapefiles;

### Bugfixes

- Improved plotting of lattice MPGs
  ([\#51](https://github.com/achubaty/grainscape/issues/51));
- Removed unused `eucPerimWeight` from documentation
  ([\#56](https://github.com/achubaty/grainscape/issues/56));
- Improved error messaging for undefined thresholds in
  [`GOC()`](https://www.alexchubaty.com/grainscape/reference/GOC.md)
  ([\#57](https://github.com/achubaty/grainscape/issues/57));

### New features & enhancements

- none

## grainscape 0.4.3

CRAN release: 2020-09-01

### Dependency changes

- none

### Bugfixes

- none

### New features & enhancements

- added MEE paper citation and updated package `DESCRIPTION` & `README`.

## grainscape 0.4.2

CRAN release: 2019-12-06

### Dependency changes

- removed unused dependency `hunspell` from Suggests

### Bugfixes

- reduced package tarball size

### New features & enhancements

- none

## grainscape 0.4.1

### Dependency changes

- none

### Bugfixes

- fix error on R-devel exposed by change to `matrix` class
- fixed error in
  [`theme_grainscape()`](https://www.alexchubaty.com/grainscape/reference/theme_grainscape.md)
  example

### New features & enhancements

- added package hexsticker

## grainscape 0.4.0

CRAN release: 2019-08-09

### Dependency changes

- Requires R \>= 3.5
- remove `rgeos` dependency and deprecate use of `sp` argument to many
  functions ([\#38](https://github.com/achubaty/grainscape/issues/38))

### Bugfixes

- numerous bug fixes

### New features & enhancements

- reimplement an algorithm to calculate the minimum planar graph using
  C++; no longer relies on SELES binary and is now cross-platform.
- use updated `igraph` functions internally
- updated vignette / tutorial
  ([\#5](https://github.com/achubaty/grainscape/issues/5))
- renamed functions to remove `gs` and `gsGOC` prefixes; old names have
  been deprecated and will be removed in a future release
  ([\#10](https://github.com/achubaty/grainscape/issues/10))
- [`MPG()`](https://www.alexchubaty.com/grainscape/reference/MPG.md) can
  now handle large rasters
- use S4 classes and methods
  ([\#3](https://github.com/achubaty/grainscape/issues/3),
  [\#19](https://github.com/achubaty/grainscape/issues/19))
- improved plotting using `ggplot2` by default
  ([\#22](https://github.com/achubaty/grainscape/issues/22),
  [\#47](https://github.com/achubaty/grainscape/issues/47))
- new functions
  [`export()`](https://www.alexchubaty.com/grainscape/reference/export.md)
  ([\#39](https://github.com/achubaty/grainscape/issues/39)) and
  [`patchFilter()`](https://www.alexchubaty.com/grainscape/reference/patchFilter.md)
  ([\#40](https://github.com/achubaty/grainscape/issues/40))
- fix typos ([\#58](https://github.com/achubaty/grainscape/issues/58),
  [@jsta](https://github.com/jsta))

### Removed features

- removed
  [`gsMPGstitch()`](https://www.alexchubaty.com/grainscape/reference/grainscape-defunct.md)
  as it was unreliable

## grainscape 0.3.0

- original version from R-forge
  (<http://grainscape.r-forge.r-project.org/>)
- uses a SELES binary to calculate the minimum planar graph (Windows
  only)
