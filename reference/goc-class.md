# The `goc` class

The `goc` class

## Slots

- `voronoi`:

  A `SpatRaster` describing the regions of proximity in resistance units
  around the focal patches or points.

- `summary`:

  A summary of the the grains of connectivity generated and their
  properties.

- `th`:

  A list giving the GOC graph at each threshold.

  Each element of `th` contains a `goc` object giving the GOC graph as
  class
  [`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html).
  Vertex attributes describes qualities of each polygon including the
  coordinates of each polygon centroid, the area of these polygons, and
  the original patch IDs in the MPG that are included in each polygon.
  All areal measurements are given as raster cell counts. A variety of
  edge attributes are also given in the GOC graph. See
  [`distance()`](https://www.alexchubaty.com/grainscape/reference/distance.md)
  for more information.

## Author

Alex Chubaty and Paul Galpern
