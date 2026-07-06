# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`grainscape` is an R package for modeling landscape connectivity using two approaches:
- **MPG (Minimum Planar Graph)**: Graph-based connectivity linking habitat patches across a resistance surface
- **GOC (Grains of Connectivity)**: Multi-scale connectivity using Voronoi tessellation at multiple resistance thresholds

The package includes an Rcpp-powered C++ computation engine (`src/`) for Voronoi tessellation and least-cost path calculations.

## Common Commands

All commands are run from within R (or via `Rscript`):

```r
# Build and check (as-CRAN)
devtools::check(args = "--as-cran", build_args = "--compact-vignettes=both")

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-MPG.R")

# Generate documentation from roxygen2
devtools::document()
```

Formatting is handled by [air](https://posit-dev.github.io/air/) (configured in `air.toml`), run from the shell:

```sh
air format .
```

```r
# Build pkgdown site
pkgdown::build_site()

# Check test coverage
covr::package_coverage()
```

From the shell:
```sh
R CMD build --compact-vignettes=both .
R CMD check --as-cran grainscape_*.tar.gz
```

## Branching Model

- `main`: Stable CRAN release
- `development`: Active development (target PRs here)
- Feature branches follow git-flow conventions

## Code Architecture

### S4 Class Hierarchy (`R/classes.R`)

```
hce         → internal habitat connectivity engine (C++ wrapper)
mpg         → minimum planar graph (igraph + SpatRaster layers)
goc         → grains of connectivity (list of graphs at multiple thresholds)
grain       → single GOC at one threshold (SpatRaster voronoi, sf centroids, igraph)
corridor    → shortest path between two coordinates (SpatRaster voronoi, sf lines/points)
```

### terra/sf Migration

All spatial operations use `terra` (rasters) and `sf` (vectors).
The `raster` and `sp` packages are no longer dependencies — neither in `Imports`
nor `Suggests`. Legacy `RasterLayer`/`SpatialPoints` inputs are no longer accepted:
- `MPG()`, `patchFilter()`, `ggGS()` require `SpatRaster` inputs
- `corridor()`, `distance()`, `point()` require a two-column matrix or `sf` object

Slots in `mpg`, `goc`, `grain`, and `corridor` S4 classes store `SpatRaster` and
`sf` objects.

### Core Workflow

1. `MPG(cost, patch)` — extracts minimum planar graph from a resistance raster and patch raster
2. `GOC(mpg, nThresh/whichThresh)` — builds multi-scale GOC from an MPG
3. `grain(goc, threshold)` — extracts a single GOC grain at a given threshold
4. `corridor(grain, coords, whichThresh)` — finds least-cost corridors between points
5. `distance(goc, coords)` — computes effective distances

### Key File Roles

- `R/MPG.R`: MPG extraction logic
- `R/GOC.R`: Multi-scale GOC construction
- `R/habitatConnectivityEngine.R`: R wrapper around the C++ engine
- `src/Engine.cpp`, `src/rcpp_HabConnEngine.cpp`: C++ computation core
- `R/ggGS.R`, `R/plot.R`: Visualization (ggplot2 and base R)
- `R/export.R`: Export to shapefiles/rasters
- `R/grainscape-defunct.R`: Defunct functions (removed; error with a pointer to their replacement)

### Rcpp Integration

The C++ engine is accessed via `R/RcppExports.R` (auto-generated — do not edit). Rebuild with `Rcpp::compileAttributes()` after modifying C++ signatures.

## Code Style

- Formatting via [air](https://posit-dev.github.io/air/) (`air format .`, configured in `air.toml`)
- 2-space indentation, UTF-8 encoding, LF line endings
- Line length limit: 100 characters (enforced by `air.toml`)
- Object names: **camelCase** (by convention)
- Documentation via roxygen2 with markdown enabled (`Config/roxygen2/version: 8.0.0`)
- Tests use `withr::local_options(warnPartialMatchArgs = TRUE, warnPartialMatchAttr = TRUE, warnPartialMatchDollar = TRUE)`
- Comments use `##` for prose comments; single `#` is reserved for commented-out code
