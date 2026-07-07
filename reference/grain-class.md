# The `grain` class

The `grain` class

## Slots

- `voronoi`:

  A `SpatRaster` describing the regions of proximity in resistance units
  around the focal patches or points.

- `summary`:

  A summary of the the grains of connectivity generated and their
  properties.

- `centroids`:

  An `sf` object indicating the grain's polygon centroids.

- `th`:

  A list of `igraph` objects giving the graphs describing the
  relationship among the polygons in that grain

  See
  [`grain()`](https://www.alexchubaty.com/grainscape/reference/grain.md)
  for more information.

## Author

Alex Chubaty and Paul Galpern
