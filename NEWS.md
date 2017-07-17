Known issues: https://github.com/achubaty/grainscape/issues

version 0.3.0.9000
==================
* reimplement an algorithm to calculate the minimum planar graph using C++;
  no longer relies on SELES binary and is now cross-platform.
* use updated `igraph` functions internally
* updated vignette / tutorial (#5)
* Renamed functions to remove `gs` and `gsGOC` prefixes. Old names have been deprecated. (#10)
* removed `gsMPGstitch()` as it was unreliable, and  `MPG()` can now handle large rasters.
* use S4 classes and methods (#3, #19)
* improved plotting using `ggplot2` by default (#22, #47)
* new functions `export()` (#39) and `patchFilter()` (#40)
* remove `rgeos` dependency and deprecate use of `sp` argument to many functions (#38)
* numerous bug fixes

version 0.3.0
=============
* original version from R-forge (http://grainscape.r-forge.r-project.org/)
* uses a SELES binary to calculate the minimum planar graph (Windows only)
