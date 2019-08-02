# CRAN submission checklist

1. ensure all tests passing locally on as many machines as possible (mac, linux, win)

    ```r
    devtools::check(args = c('--as-cran'), build_args = c('--compact-vignettes=both'))
    ```

2. ensure passing on travis, appveyor

    * <https://ci.appveyor.com/project/achubaty/grainscape/history>
    * <https://travis-ci.org/achubaty/grainscape/builds>

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

6. rebuild docs

   ```r
   devtools::document()
   ```

7. update cran-comments (incl. versions tested)

8. run spell checks

   ```r
   spelling::spell_check_package()
   spelling::update_wordlist()
   
   ## see also tests/spelling.R
   ```

9. run revdep checks (see `revdep/check.R`)

10. merge `development` branch into `master`

11. remove `Remotes` from `DESCRIPTION`

12. rebuild package website (`master` branch)

    ```r
    pkgdown::build_site()
    ```

13. submit to CRAN 

    ```r
    devtools::release(args = "--compact-vignettes=both")
    ```
