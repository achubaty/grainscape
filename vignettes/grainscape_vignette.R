## ----setup, include=FALSE------------------------------------------------
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

## ----part_A, dependson="figure_20", include=FALSE------------------------
## build visual table of contents using vignette figures
partA <- cowplot::plot_grid(
  figure09 + theme(plot.title = element_text(size = 8)),
  figure10 + theme(plot.title = element_text(size = 8)),
  figure11 + theme(plot.title = element_text(size = 8)),
  figure12 + theme(plot.title = element_text(size = 8)),
  figure13 + theme(plot.title = element_text(size = 8)),
  figure14 + theme(plot.title = element_text(size = 8)),
  figure15 + theme(plot.title = element_text(size = 8)),
  figure16 + theme(plot.title = element_text(size = 8)),
  figure17 + theme(plot.title = element_text(size = 8)),
  figure18 + theme(plot.title = element_text(size = 8)),
  figure19 + theme(plot.title = element_text(size = 8)),
  figure20 + theme(plot.title = element_text(size = 8)),
  nrow = 4, ncol = 3, labels = paste0("(", 9:20, ")"), label_size = 8,
  vjust = 1.75, hjust = -0.05) +
  theme(panel.background = element_blank())

## currently writing to tempdir for CRAN checks;
## be sure to save to vignettes/figures directory when changing figures.
ggsave(file.path(tempdir(), "figure_partA.png"), plot = partA,
       width = 8, height = 8, dpi = 600) 

## ----part_B, dependson="figure_25", include=FALSE------------------------
partB <- cowplot::plot_grid(
  figure21 + theme(plot.title = element_text(size = 8)),
  figure22 + theme(plot.title = element_text(size = 8)),
  figure23 + theme(plot.title = element_text(size = 8)),
  figure24 + theme(plot.title = element_text(size = 8)),
  figure25 + theme(plot.title = element_text(size = 8)),
  nrow = 4, ncol = 3, labels = paste0("(", 21:25, ")"), label_size = 8,
  vjust = 1.75, hjust = -0.05) +
  theme(panel.background = element_blank())

## currently writing to tempdir for CRAN checks;
## be sure to save to vignettes/figures directory when changing figures.
ggsave(file.path(tempdir(), "figure_partB.png"), plot = partB,
       width = 8, height = 8, dpi = 600)

