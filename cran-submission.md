# CRAN submission checklist

1. ensure all tests passing locally on as many machines as possible (mac, linux, win)

    ```r
    devtools::check(args = c('--as-cran'), build_args = c('--compact-vignettes=both'))
    ```

2. ensure passing on GitHub Actions

    * <https://github.com/achubaty/grainscape/actions>

2. check code formatting and cleanup as needed

    ```r
    #install_github("jimhester/lintr")
    library(lintr)
    lint_package()
    ```

3. ensure passing win-builder olrelease, release, and devel

    ```r
    check_win_oldrelease(args = "--compact-vignettes=both")
    check_win_release(args = "--compact-vignettes=both")
    check_win_devel(args = "--compact-vignettes=both")
    ```

4. bump version number in DESCRIPTION (use non-devel suffix -- no `.9000`)

5. update `NEWS.md`

    ```r
    devtools::show_news()
    ```

6. rebuild docs and ensure vignettes are compressed

   ```r
   devtools::document()
   tools::compactPDF("vignettes", qpdf = "/usr/bin/qpdf", gs_quality = "ebook")
   ```

7. update cran-comments (incl. versions tested)

8. run spell checks

   ```r
   spelling::spell_check_package()
   spelling::update_wordlist()
   ```

9. run revdep checks (see `revdep/check.R`)

10. merge `development` branch into `main`

11. remove `Remotes` from `DESCRIPTION`

12. submit to CRAN 

    ```r
    devtools::release(args = "--compact-vignettes=both")
    ```
