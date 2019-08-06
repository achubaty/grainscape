## New submission

This is a new package submission providing tools for efficient modelling of landscape connectivity, habitat, and protected areas.
We have included an extensive introductory vignette illustrating the use of the package (modelling, analysis, and visualizations) in `vignettes/grainscape_vignette.Rmd`.

I have fixed the NOTE regarding keywords. Apologies for this oversight.

I have added citations with DOIs to the DESCRIPTION as requested.
I have replaced `dontrun{}` in several plotting examples with `if (interactive()) {}` to ensure they don't run during automated tests.
I have reduced the execution time of some examples.

## Test environments

### Previous R versions
* macOS Mojave       (travis-ci), R 3.5.3
* Ubuntu 16.04       (travis-ci), R 3.5.3
* Windows             (appveyor), R 3.5.3
* Windows          (win-builder), R 3.5.3

### Current R versions
* macOS Mojave       (travis-ci), R 3.6.1
* macOS Mojave           (local), R 3.6.1
* Ubuntu 16.04       (travis-ci), R 3.6.1
* Ubuntu 18.04           (local), R 3.6.1
* Windows             (appveyor), R 3.6.1
* Windows          (win-builder), R 3.6.1
* Windows 7              (local), R 3.6.1

### Development R version
* Ubuntu 16.04       (travis-ci), R 3.7.0 (2019-07-29 r76904)
* Ubuntu 18.04           (local), R 3.7.0 (2019-08-02 r76911)
* Windows             (appveyor), R 3.7.0 (2019-07-26 r76894)
* Windows          (win-builder), R 3.7.0 (2019-07-05 r76784)

## R CMD check results

There are no ERRORs nor WARNINGs.

There is 1 NOTE:

1. this is a new package submission

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Alex M Chubaty <alex.chubaty@gmail.com>'
    
    New submission

## Downstream dependencies

There are currently no reverse dependencies for this package.
