# Extra checklist items appended to the issue created by
# `usethis::use_release_issue()`. usethis calls this (unexported) function via
# `release_extra_bullets()` and turns each string into a `- [ ]` checklist item,
# inserted just before the "Submit to CRAN" section. These capture the
# grainscape-specific steps from `cran-submission.md` that usethis's default
# checklist does not cover, as exact copy-paste-and-run commands where possible.
# See `?usethis::use_release_issue`.
release_bullets <- function() {
  c(
    paste(
      "`air format .` (update `air` to the latest first -- CI uses the latest",
      "via `setup-air@v1`), then confirm the `format-check` GitHub Action passes"
    ),
    "`spelling::spell_check_package()` then `spelling::update_wordlist()`",
    paste(
      "Confirm the R-CMD-check matrix in `.github/workflows/R-CMD-check.yaml`",
      "covers the DESCRIPTION minimum R version (currently R 4.2, via `oldrel-4`)"
    ),
    "`devtools::check_win_oldrelease(args = \"--compact-vignettes=both\")`",
    "`devtools::check_win_release(args = \"--compact-vignettes=both\")`",
    "`devtools::check_win_devel(args = \"--compact-vignettes=both\")`",
    "`devtools::check_mac_release(args = \"--compact-vignettes=both\")`",
    "`rhub::rhub_check()` (or run the R-hub GitHub Action)",
    paste(
      "Deploy the pkgdown site (merge to `main`) before submitting, so the",
      "README URLs (e.g. the Code of Conduct page) resolve"
    ),
    "`devtools::submit_cran(args = \"--compact-vignettes=both\")`"
  )
}
