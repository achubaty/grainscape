# Habitat connectivity engine (C++)

Internal function, not intended to be called directly.

## Usage

``` r
.habConnRcpp(cost, patches, ncol, nrow)
```

## Arguments

- cost:

  Numeric vector of habitat cost (resistance) values extracted from a
  cost raster map.

- patches:

  Numeric vector that have binary values (`0` and `1`) where ones
  corresponds to patch cells and zeroes to non-habitat (i.e., matrix)
  cells.

- ncol:

  Number of columns in the cost and patch raster maps.

- nrow:

  Number of rows in both the cost and patch raster maps.

## Author

Sam Doctolero
