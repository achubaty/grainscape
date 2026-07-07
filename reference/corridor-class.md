# The `corridor` class

The `corridor` class

## Slots

- `voronoi`:

  A `SpatRaster` representation of the boundaries of the voronoi
  polygons.

- `linksSP`:

  An `sf` representation of links in the grains of connectivity graph.

- `nodesSP`:

  An `sf` representation of the nodes in the grains of connectivity
  graph

- `shortestLinksSP`:

  An `sf` representation of the links in the shortest path between
  coordinates

- `shortestNodesSP`:

  An `sf` representation of the nodes in the shortest path between
  coordinates

- `corridorLength`:

  A `numeric` of length 1 giving the length of the shortest path between
  coordinates in accumulated resistance units.

  See
  [`corridor()`](https://www.alexchubaty.com/grainscape/reference/corridor.md)
  for more information.

## Author

Alex Chubaty and Paul Galpern
