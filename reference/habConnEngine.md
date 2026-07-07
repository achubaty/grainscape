# Habitat connectivity engine

Internal function. Serves as a wrapper around the habitat connectivity
engine developed in C++.

## Usage

``` r
.habConnEngine(cost, patches)
```

## Arguments

- cost:

  Numeric raster cost (resistance) map.

- patches:

  Logical raster indicating presence of habitat patches.

## Value

An object of class
[hce](https://www.alexchubaty.com/grainscape/reference/hce-class.md).

## See also

`link{habConnRcpp}`

## Author

Alex Chubaty
