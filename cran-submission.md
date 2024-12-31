# CRAN submission checklist

1. ensure all tests passing locally on as many machines as possible (mac, linux, win)

    ```r
    devtools::check(args = c('--as-cran'), build_args = c('--compact-vignettes=both'))
    ```

2. ensure passing on GitHub Actions

    * <https://github.com/achubaty/grainscape/actions>

2. check code formatting and cleanup as needed

    ```r
    # install_github("jimhester/lintr")
    lintr::lint_package()
    ```

    ```r
    # install_github("r-lib/styler")
    styler:::style_active_pkg()
    ```

3. ensure passing win-builder oldrelease, release, and devel

    ```r
    ## macOS
    check_mac_release(args = "--compact-vignettes=both")
    
    ## Windows
    check_win_oldrelease(args = "--compact-vignettes=both")
    check_win_release(args = "--compact-vignettes=both")
    check_win_devel(args = "--compact-vignettes=both")
    
    ## using rhub
    rhub::check_for_cran()
    ```

4. bump version number in DESCRIPTION (use non-devel suffix -- no `.9000`)

    ```r
    usethis::use_version() ## e.g., usethis::use_version("minor")
    ````

5. update `NEWS.md`

    ```r
    devtools::show_news()
    ```

6. rebuild docs and ensure vignettes are compressed

   ```r
   devtools::document()
   tools::compactPDF("vignettes", qpdf = Sys.which(Sys.getenv("R_QPDF", "qpdf")), gs_quality = "ebook")
   ```

7. run spell checks

   ```r
   spelling::spell_check_package()
   spelling::update_wordlist()
   ```

8. run reverse dependency checks (see `revdep/check.R`)

9. update cran-comments (incl. versions tested)

10. switch to `main` branch and merge in `development`

11. remove `Remotes` from `DESCRIPTION` (on `main` branch)

12. submit to CRAN 

    ```r
    devtools::release(args = "--compact-vignettes=both")
    ```

13. once accepted, create a new GitHub release:

    ```r
    usethis::use_github_release()
    ```
