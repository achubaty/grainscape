# Abbreviate names

Manually specify field names for writing to shapefile instead of relying
on `sf:::abbreviate_shapefile_names()` (which uses
[`abbreviate()`](https://rdrr.io/r/base/abbreviate.html)) during save.
Names must contain fewer than 10 characters.

## Usage

``` r
.abbrev(x)
```

## Author

Alex Chubaty
