## New submission

This is a manitenance release to fix problems exposed by recent changes to R-devel.
See `NEWS.md` for a complete list of changes.

## Test environments

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.5.3
* Windows                    (appveyor), R 3.5.3
* Windows                 (win-builder), R 3.5.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 3.6.1
* macOS 10.15.1 Catalina        (local), R 3.6.1
* Ubuntu 16.04              (travis-ci), R 3.6.1
* Ubuntu 18.04                  (local), R 3.6.1
* Windows                    (appveyor), R 3.6.1
* Windows                 (win-builder), R 3.6.1

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.0.0 (2019-12-05 r77523)
* Ubuntu 18.04                  (local), R 4.0.0 (2019-12-04 r77518)
* Windows                    (appveyor), R 4.0.0 (2019-12-04 r77519)
* Windows                 (win-builder), R 4.0.0 (2019-12-02 r77499)

## R CMD check results

There are no ERRORs. There is 1 WARNING caused by a problem in a dependency package.

1. Vignette error caused by `rmarkdown` (see <https://github.com/rstudio/rmarkdown/issues/1716>):

    * checking re-building of vignette outputs ... [135s] WARNING
    Error in re-building vignettes:
      ...
    ! Misplaced \crcr.
    \endtabular ->\crcr 
                        \egroup \egroup $\egroup 
    l.143 \maketitle
    
    Error: processing vignette 'grainscape_vignette.Rmd' failed with diagnostics:
    Failed to compile grainscape_vignette.tex. See https://yihui.name/tinytex/r/#debugging for debugging tips.
    See grainscape_vignette.log for more info.
    Execution halted

## Downstream dependencies

There are currently no reverse dependencies for this package.
