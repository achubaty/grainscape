if (interactive()) {
  library(ggplot2)

  ## Plot the patches in a minimum planar graph
  ggplot() +
    geom_tile(ggGS(tinyPatchMPG, "patchId"), mapping = aes(x = x, y = y, fill = value)) +
    theme_grainscape()
}
