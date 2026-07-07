# CRAN submission

The release checklist is generated as a GitHub issue by
[`usethis::use_release_issue()`](https://usethis.r-lib.org/reference/use_release_issue.html):

```r
usethis::use_release_issue()
```

That issue combines usethis's default CRAN-release checklist with grainscape's own
steps, which are defined by `release_bullets()` in
[`R/release-bullets.R`](R/release-bullets.R) — air formatting, spell checks,
minimum-R-version coverage, win-builder / mac-builder / R-hub, deploying the
pkgdown site before submitting, and submitting with compacted vignettes. **Edit
the custom steps there**, not here, so they travel with the package and can't
drift from a parallel document.

The actual submission (build with compacted vignette PDFs, then upload) is:

```r
devtools::submit_cran(args = "--compact-vignettes=both")
```

Reference notes that don't fit a one-line checklist item live next to the code
they describe — e.g. the renderer-sensitive `vdiffr` plot tests are explained in
the header comment of `tests/testthat/test-plot.R`.
