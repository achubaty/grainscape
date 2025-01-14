## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(igraph)
library(grainscape)
library(raster)
library(ggplot2)

opts_chunk$set(cache = TRUE)
opts_chunk$set(echo = TRUE)

opts_chunk$set(fig.height = 3)
opts_chunk$set(fig.width = 3)
opts_chunk$set(fig.show = "hold")

options(knitr.kable.NA = "")

## ----figure_06, fig.cap="\\label{fig:thresholdedgraph}The thresholded MPG depicted with a link length of 250 resistance units. An organism that can disperse a maximum of 250 resistance units would experience this landscape as 6 connected regions in the depicted spatial configuration. Note that the plotting has been customized to emphasize which patches are connected. This was done by plotting links with less than the threshold length from the centroids of patches", warning=FALSE----
links_df <- ggGS(patchyMPG, "links") |>
  dplyr::filter(lcpPerimWeight >= 250)

ggplot() +
  geom_raster(
    data = ggGS(patchyMPG, "patchId"),
    mapping = aes(x = x, y = y, fill = value > 0)
  ) +
  scale_fill_manual(values = "grey") +
  geom_segment(
    data = links_df,
    mapping = aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = as.factor(lcpPerimWeight)
    )
  ) +
  scale_colour_manual(values = rep("forestgreen", nrow(links_df))) +
  geom_point(
    data = ggGS(patchyMPG, "nodes"),
    mapping = aes(x = x, y = y),
    colour = "darkgreen"
  )

