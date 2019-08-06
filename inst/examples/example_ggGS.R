if (interactive()) {
  library(ggplot2)

  ## Plot the patches in a minimum planar graph
  theme_set(theme_grainscape())
  ggplot() +
    geom_tile(data = ggGS(tinyPatchMPG, "patchId"),
                aes(x = x, y = y, fill = value))

  ## Plot the grain polygons in a grain of connectivity
  ggplot() +
    geom_tile(data = ggGS(grain(tinyPatchGOC, 3), "voronoi"),
                aes(x = x, y = y, fill = value))

  ## Plot the grain polygon boundaries
  ggplot() +
    geom_tile(data = ggGS(grain(tinyPatchGOC, 3), "vorBound"),
                aes(x = x, y = y, fill = value))

  ## Plot the patches and perimeter links of a minimum planar graph
  ggplot() +
    geom_tile(data = ggGS(tinyPatchMPG, "patchId"),
                aes(x = x, y = y, fill = value)) +
    geom_segment(data = ggGS(tinyPatchMPG, "links"),
                 aes(x = x1p, y = y1p, xend = x2p, yend = y2p))

  ## Plot the patches and linear representations of the perimeter links
  ## of a minimum planar graph
  ggplot() +
    geom_tile(data = ggGS(tinyPatchMPG, "patchId"),
                 aes(x = x, y = y, fill = value)) +
    geom_segment(data = ggGS(tinyPatchMPG, "links"),
                 aes(x = x1p, y = y1p, xend = x2p, yend = y2p))

  ## Plot the nodes and links of a grains of connectivity network
  ## superimposed over the grain polygons
  focalGrain <- grain(tinyPatchGOC, 3)
  ggplot() +
    geom_tile(data = ggGS(focalGrain, "vorBound"),
                aes(x = x, y = y, fill = value)) +
    geom_point(data = ggGS(focalGrain, "nodes"), aes(x = x, y = y)) +
    geom_segment(data = ggGS(focalGrain, "links"),
                 aes(x = x1, y = y1, xend = x2, yend = y2))
}
