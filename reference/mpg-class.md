# The `mpg` class

The `mpg` class

## Slots

- `mpg`:

  The minimum planar graph as class `igraph`.

- `patchId`:

  The input `patch` raster with patch cells assigned to their id
  (`SpatRaster`).

- `voronoi`:

  The Voronoi tessellation of the patches and resistance surface
  (`SpatRaster`).

- `lcpPerimWeight`:

  The paths of the links between patches and their accumulated costs
  (`SpatRaster`).

- `lcpLinkId`:

  The paths of the links between patches and their id (`SpatRaster`).

- `mpgPlot`:

  A `SpatRaster` version of the `mpg`, which can be easily plotted to
  visualize the MPG.

  The `mpg` slot contains useful vertex and edge attributes. Vertex
  attributes give attributes of patches including patch area, the area
  of patch edges, the core area of each patch, and the coordinates of
  the patch centroid. All areal measurements are given as raster cell
  counts. Edge attributes give attributes of the graph links including
  link weights giving accumulated resistance/least-cost path distance,
  Euclidean distance, and the start and end coordinates of each link.

## Author

Alex Chubaty and Paul Galpern
