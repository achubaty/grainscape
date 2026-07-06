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
* GitHub Actions:
  - Ubuntu 24.04: R-devel, R 4.6.1 (release), R 4.5.3, R 4.4.3, R 4.3.3, and
    R 4.2.3 (the minimum R version declared in DESCRIPTION)
  - Windows: R-devel, R 4.6.1 (release), R 4.5.3, R 4.4.3
  - macOS: R 4.6.1 (release)
* win-builder: R-devel, R 4.6.1 (release), R 4.5.3 (oldrelease)
* mac-builder: R 4.6.1 (release)

## R CMD check results

There were no ERRORs or WARNINGs. The submission tarball is built with
`--compact-vignettes=both`, so the vignette PDFs are compacted.

There is 1 NOTE, from the URL check of README.md:

    Found the following (possibly) invalid URLs:
      URL: https://support.posit.co/hc/en-us/articles/200486498-Package-Development-Prerequisites
        Status: 403

This URL is valid and opens in a web browser; Posit's support site returns a 403
to automated (non-browser) requests only.

## Reverse dependencies

There are currently no reverse dependencies for this package.
