## New submission
This release fixes CRAN check problems and issues arising from changes in dependency packages.
See `NEWS.md` for a complete list of changes.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.2.3, 4.3.3
* Windows                      (GitHub), R 4.2.3, 4.3.3
* Windows                       (local), R 4.2.3, 4.3.3
* Windows                 (win-builder), R 4.3.3

### Current R versions
* macOS 13.3.1            (mac-builder), R 4.4.2
* macOS 14.7.2                 (GitHub), R 4.4.2
* macOS 15.1.1                  (local), R 4.4.2
* Ubuntu 22.04                 (GitHub), R 4.4.2
* Ubuntu 24.04                  (local), R 4.4.2
* Windows                      (GitHub), R 4.4.2
* Windows                       (local), R 4.4.2
* Windows                 (win-builder), R 4.4.2

### Development R version
* Ubuntu 22.04                 (GitHub), R-devel (2025-01-02 r87517)
* Ubuntu 24.04                  (local), R-devel (2024-12-29 r87484)
* Windows                      (GitHub), R-devel (2025-01-02 r87517 ucrt)
* Windows                 (win-builder), R-devel (2025-01-02 r87517 ucrt)

## R CMD check results

There are no WARNINGS nor ERRORs.

There was 1 NOTE, which appears to be spurious, as the URL works in a web browser.

    Found the following (possibly) invalid URLs:
      URL: https://support.posit.co/hc/en-us/articles/200486498-Package-Development-Prerequisites
        From: README.md
        Status: 403
        Message: Forbidden

## Downstream dependencies

There are currently no reverse dependencies for this package.
