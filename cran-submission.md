# CRAN submission checklist

1.  ensure all tests pass locally on as many machines as possible (mac,
    linux, win)

    ``` r

    devtools::check(args = c("--as-cran"), build_args = c("--compact-vignettes=both"))
    ```

    **Extended (vdiffr) tests:** the visual-regression plot tests in
    `tests/testthat/test-plot.R` are gated with `skip_on_cran()` **and**
    `skip_on_ci()`, so both CRAN and CI (`R CMD check` on GitHub
    Actions) **skip them**. Their SVG snapshots are renderer-sensitive
    (svglite/freetype/ggplot2 versions) and only match a controlled
    local environment, so they run only locally, as a maintainer
    regression tool:

    ``` r

    devtools::test() ## runs the vdiffr plot tests locally (requires CI != "true")
    ```

    Review any flagged plot diffs with
    `testthat::snapshot_review("plot/")` (or `vdiffr::manage_cases()`);
    accept intended changes with `testthat::snapshot_accept("plot")`.

2.  ensure passing on GitHub Actions

    - <https://github.com/achubaty/grainscape/actions>

3.  format the package

    ``` sh
    # https://posit-dev.github.io/air/
    air format .
    ```

4.  ensure passing on additional platforms

    ``` r

    ## macOS
    devtools::check_mac_release(args = "--compact-vignettes=both")

    ## Windows (win-builder: oldrelease, release, devel)
    devtools::check_win_oldrelease(args = "--compact-vignettes=both")
    devtools::check_win_release(args = "--compact-vignettes=both")
    devtools::check_win_devel(args = "--compact-vignettes=both")
    ```

    For broader coverage, run the `R-hub` workflow on GitHub Actions
    (Actions → “R-hub” → “Run workflow”), or locally via
    `rhub::rhub_check()` after a one-time `rhub::rhub_setup()`.

5.  bump version number in DESCRIPTION (use non-devel suffix – no
    `.9000`)

    ``` r

    usethis::use_version() ## e.g., usethis::use_version("minor")
    ```

6.  update `NEWS.md`

    ``` r

    devtools::show_news()
    ```

7.  rebuild docs (vignettes are compacted automatically via
    `--compact-vignettes=both`)

    ``` r

    devtools::document()
    ```

8.  run spell checks

    ``` r

    spelling::spell_check_package()
    spelling::update_wordlist()
    ```

9.  run reverse dependency checks (see `revdep/check.R`)

10. update `cran-comments.md` (incl. versions tested)

11. switch to `main` branch and merge in `development`

12. remove `Remotes` from `DESCRIPTION` (on `main` branch), if present

13. submit to CRAN

    ``` r

    devtools::release(args = "--compact-vignettes=both")
    ```

14. once accepted, create a new GitHub release:

    ``` r

    usethis::use_github_release()
    ```
