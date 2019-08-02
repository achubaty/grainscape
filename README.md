# grainscape

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/grainscape)](https://cran.r-project.org/package=grainscape)
[![Build Status](https://travis-ci.org/achubaty/grainscape.svg?branch=master)](https://travis-ci.org/achubaty/grainscape)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/f8eddi541e19h2kv/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/grainscape/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/achubaty/grainscape/badge.svg?branch=master)](https://coveralls.io/github/achubaty/grainscape?branch=master)

## Efficient Modelling of Landscape Connectivity, Habitat, and Protected Area Networks

Given a landscape resistance surface, creates grains of connectivity and minimum planar graph models that can be used to calculate effective distances for landscape connectivity at multiple scales.
This is a cross-platform reimplementation and update of the `grainscape` package (http://grainscape.r-forge.r-project.org).

### Installation

#### From CRAN

```r
install.packages("grainscape")
```

#### From GitHub

1. **Install development libraries:** building packages from source requires the appropriate development libraries for your operating system.
  See [here](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) for more details.
    
    - *Windows:* install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
    - *macOS:* install Xcode commandline tools from the terminal: `xcode-select install`. 
    - *Debian/Ubuntu Linux:* ensure `r-base-dev` is installed.

2. **Install from GitHub:**
    
    ```r
    #install.packages("devtools")
    library("devtools")
    install_github("achubaty/grainscape")
    ```

### Reporting bugs

Contact us via the package GitHub site: [https://github.com/achubaty/grainscape/issues](https://github.com/achubaty/grainscape/issues).
