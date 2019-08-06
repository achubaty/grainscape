if (interactive()) {
  library(ggplot2)

  ## MPG and showing simplified links among the perimeters of patches
  plot(tinyPatchMPG)

  ## MPG showing links among the nodes of connected patches
  plot(tinyPatchMPG, quick = "network")

  ## MPG showing the shortest paths between patches actually used to
  ## to calculate link weight values
  plot(tinyPatchMPG, quick = "mpgPlot")

  ## A grain of connectivity network plot with Voronoi boundaries
  plot(grain(tinyPatchGOC, 3), quick = "grainPlot")

  ## Capture plot output for further processing with ggplot
  g <- plot(tinyPatchMPG, print = FALSE, theme = FALSE)
  g <- g + theme_minimal() + ggtitle("Minimum planar graph") +
         theme(plot.title = element_text(size = 20, hjust = 0.5)) +
         theme(legend.position = "none") +
         xlab("Easting") + ylab("Northing")
  g

  ## To change aesthetics it is best to build the plot from scratch
  ## using grainscape::ggGS(). See examples therein.
}
