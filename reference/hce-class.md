# The `hce` class

Used internally.

## Slots

- `voronoi`:

  The Voronoi tessellation of the patches and resistance surface
  (`SpatRaster`).

- `patchLinks`:

  A `SpatRaster` whose values indicate patch ids (positive integers) and
  link ids (negative integers).

- `linkData`:

  A `data.frame` of link attributes.

## Author

Alex Chubaty and Sam Doctolero
