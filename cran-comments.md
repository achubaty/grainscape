## Submission notes

grainscape 1.0.0 is a major release. It migrates the package's spatial stack from
the retiring `raster`/`sp` packages to `terra`/`sf`, fixes several long-standing
bugs in the minimum planar graph (MPG) engine (#72), and makes the long-deprecated
`gs`-prefixed functions defunct. These changes include breaking changes to
function inputs and to the S4 slot types of returned objects; see `NEWS.md` for the
full list. There are no reverse dependencies to break, and downstream users were
given advance notice of the breaking changes via a GitHub announcement (#77).

## Test environments

* local: Ubuntu 26.04, R 4.6.1 -- 0 errors | 0 warnings | 0 notes
* GitHub Actions: Ubuntu, macOS, and Windows (R oldrel, release, devel)
* win-builder: R oldrelease, release, and devel
* mac-builder: R release

## R CMD check results

There were no ERRORs or WARNINGs.

Local `R CMD check --as-cran` reported 0 NOTEs.

## Reverse dependencies

There are currently no reverse dependencies for this package.
