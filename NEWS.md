# grainscape 1.0.0

## Dependency changes

* Migrated from `raster`/`sp` to `terra`/`sf` for all spatial operations:
  - `terra` and `sf` added to `Imports`;
  - `raster` and `sp` dependencies removed entirely.
* Vignette diagrams are now generated from code with `DiagrammeRsvg` and `rsvg` (added to
  `Suggests`); `webshot2` is no longer needed for the vignettes and was removed from `Suggests`.

## Breaking changes

* `MPG()`, `patchFilter()`, and `ggGS()` no longer accept `RasterLayer` inputs;
  pass `SpatRaster` objects (from `terra`) instead;
* `corridor()`, `distance()`, and `point()` no longer accept `SpatialPoints` inputs;
  pass a two-column matrix or `sf` object instead;
* Slot types in `mpg`, `goc`, `grain`, and `corridor` S4 classes have changed from
  `RasterLayer`/`SpatialPoints`/`SpatialLines`/`SpatialLinesDataFrame` to `SpatRaster`/`sf`;

## New features

* New exported plotting helpers `plotResistance()` and `plotWithResistance()`: draw a
  resistance surface with a consistent colour scheme (and a discrete-value legend), and
  place it as an equally-sized reference panel beside a network plot for visual comparison.
  Both are demonstrated in the vignette's introductory figures (`plotWithResistance()` uses
  `cowplot`).

## Bugfixes

* Fixed incorrect least-cost paths in MPG (#72);
* Fixed a regression in the #72 work where legitimate, non-redundant MPG links were dropped on
  high-variance resistance surfaces: the indirect-path search could treat an unset cell id
  (the internal `-99` sentinel) as a pivot patch, fabricating a bogus two-hop route that
  suppressed a valid direct link. Such malformed links are now rejected and non-patch pivots
  are ignored (#72);
* Fixed patches being left isolated in the MPG: a patch cell that was never added as an
  active cell during spreading (e.g. a single-cell patch) kept the internal `-99` id, so it
  was an unresolved link endpoint and all of its links were silently dropped. Every cell's id
  is now seeded from the voronoi map at initialization, so such patches are linked correctly
  (#72);
* Fixed a Voronoi tessellation bias on high-variance resistance surfaces: patch (source) cells
  now begin spreading at the uniform minimum cost rather than the resistance of the cell they
  happen to overlap. Previously a patch sitting on a high-resistance cell spread late and lost
  territory to its neighbours, distorting the tessellation (and, in the extreme, leaving a
  patch with a single-cell region and no links). The fix changes only the tessellation: link
  weights are accumulated from the entered cells' resistances (independent of the source cell),
  so on the package's test surfaces the MPG and its weights are unchanged, and still match
  independent least-cost paths exactly (#72);
* Fixed `graphdf` returning a transposed (single-column) data frame for MPGs with only one link;
* `point()` (and `distance()`, which calls it) now emit the "coords correspond to NA cells" warning once per call instead of once per coordinate-threshold combination (#50);
* The `corridor` `plot()` method now uses the `linewidth` aesthetic (rather than the deprecated
  `size` aesthetic) for its link segments, silencing a `ggplot2` (>= 3.4.0) deprecation warning;

## Vignette

* The "Calculating the minimum planar graph" vignette now generates its UML class diagrams, the
  algorithm flow chart, and the `Engine` call graphs directly from Graphviz/`DiagrammeR`
  specifications (rendered to vector PDFs with `DiagrammeRsvg` and `rsvg`) rather than embedding
  static images, so they stay in sync with the C++ engine; its technical reference was also
  updated for the #72 least-cost-path changes (the `settled_map`, `createLinks()`, and the
  `parentResistance` field) (#75);
* Corrected the thresholded-MPG figure (Figure 6), which plotted links *above* the dispersal
  threshold instead of the retained links *below* it (so the cheap, low-resistance links
  between neighbouring patches were hidden);
* Made the Euclidean-vs-resistance path-distance table robust by matching rows on the node
  pair (rather than column-binding two independently sorted tables), and the "patch 7
  neighbours" table now filters explicitly for patch 7. The minimum-planar-graph figure above
  that table now labels exactly node 7 and its linked neighbours, derived from the graph rather
  than a hard-coded list of patch ids.

# grainscape 0.5.0

## Dependency changes

* Require R 4.2 or higher (for native pipe and placeholder)
* Add `DiagrammeR`, `dplyr`, `webshot2` and `withr` to Suggests for use with vignettes and tests

## Bugfixes

* Fixed documentation issues and 'memory not mapped' segfault errors on R-devel (#73)
* Improved handling of user-added attributes in exported objects (#71)
* Fixed partial match warnings
* Use replacements for deprecated `igraph` functions

## New features & enhancements

* Use native R pipe throughout instead of `magrittr` pipe
* Improved documentation

# grainscape 0.4.4

## Dependency changes

* Removed retiring spatial packages `rgdal` and `rgeos` (#68);
* Use `sf` for writing shapefiles;

## Bugfixes

* Improved plotting of lattice MPGs (#51);
* Removed unused `eucPerimWeight` from documentation (#56);
* Improved error messaging for undefined thresholds in `GOC()` (#57);

## New features & enhancements

* none

# grainscape 0.4.3

## Dependency changes

* none

## Bugfixes

* none

## New features & enhancements

* added MEE paper citation and updated package `DESCRIPTION` & `README`.

# grainscape 0.4.2

## Dependency changes

* removed unused dependency `hunspell` from Suggests

## Bugfixes

* reduced package tarball size

## New features & enhancements

* none

# grainscape 0.4.1

## Dependency changes

* none

## Bugfixes

* fix error on R-devel exposed by change to `matrix` class
* fixed error in `theme_grainscape()` example

## New features & enhancements

* added package hexsticker

# grainscape 0.4.0

## Dependency changes

* Requires R >= 3.5
* remove `rgeos` dependency and deprecate use of `sp` argument to many functions (#38)

## Bugfixes

* numerous bug fixes

## New features & enhancements

* reimplement an algorithm to calculate the minimum planar graph using C++;
  no longer relies on SELES binary and is now cross-platform.
* use updated `igraph` functions internally
* updated vignette / tutorial (#5)
* renamed functions to remove `gs` and `gsGOC` prefixes; old names have been deprecated and will be removed in a future release (#10)
* `MPG()` can now handle large rasters
* use S4 classes and methods (#3, #19)
* improved plotting using `ggplot2` by default (#22, #47)
* new functions `export()` (#39) and `patchFilter()` (#40)
* fix typos (#58, @jsta)

## Removed features

* removed `gsMPGstitch()` as it was unreliable

# grainscape 0.3.0

* original version from R-forge (http://grainscape.r-forge.r-project.org/)
* uses a SELES binary to calculate the minimum planar graph (Windows only)
