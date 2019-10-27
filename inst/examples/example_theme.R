if (interactive()) {
  library(ggplot2)

  ## Plot the patches in a minimum planar graph
  theme_set(theme_grainscape())
  plot(tinyPatchMPG, quick = "mpgPlot")
}
