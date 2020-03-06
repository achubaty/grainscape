#install.packages("hexSticker")
#if (Sys.info()["sysname"] == "Darwin") install.packages("sysfonts")

library(ggplot2)
library(grainscape)
library(hexSticker)
library(igraph)
library(raster)
library(showtext)
library(sysfonts)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto Slab", "roboto")
## Automatically use showtext to render text for future devices
showtext_auto()

## build plot (based on vignette fig 20, with patchyMPG)
patchy <- raster(system.file("extdata/patchy.asc", package = "grainscape"))
isBecomes <- cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6))
patchyCost <- reclassify(patchy, rcl = isBecomes)
patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))

startEnd <- c(5, 42)
shPath <- shortest_paths(patchyMPG$mpg,
                         from = which(V(patchyMPG$mpg)$patchId == startEnd[1]),
                         to = which(V(patchyMPG$mpg)$patchId == startEnd[2]),
                         weights = E(patchyMPG$mpg)$lcpPerimWeight,
                         output = "both")

## Extract the nodes and links of this shortest path
shPathN <- as.integer(names(shPath$vpath[[1]]))
shPathL <- E(patchyMPG$mpg)[shPath$epath[[1]]]$linkId

## Produce shortest path tables for plotting
shPathNodes <- subset(ggGS(patchyMPG, "nodes"), patchId %in% shPathN)
shPathLinks <- subset(ggGS(patchyMPG, "links"), linkId %in% shPathL)

## Find the distance of the shortest path
shPathD <- distances(patchyMPG$mpg,
                     v = which(V(patchyMPG$mpg)$patchId == startEnd[1]),
                     to = which(V(patchyMPG$mpg)$patchId == startEnd[2]),
                     weights = E(patchyMPG$mpg)$lcpPerimWeight)[1]

colours <- RColorBrewer::brewer.pal(8, "Dark2")
f <- file.path("stickers", "patchyMPG.png")
g <- ggplot() +
        geom_tile(data = ggGS(patchyMPG, "patchId"),
                    aes(x = x, y = y,
                        fill = ifelse(value %in% shPathN, colours[1], colours[3]))) +
        scale_fill_identity() +
        geom_segment(data  = shPathLinks, aes(x = x1, y = y1, xend = x2, yend = y2),
                     colour = colours[2], size = 2) +
        geom_point(data = shPathNodes, aes(x = x, y = y), colour = colours[2]) +
        theme_void()
ggsave(f, g, width = 6, height = 6, bg = "transparent")

sticker(f,
        package = "grainscape",
        h_color = colours[1], h_fill = "#ececec",
        p_color = colours[2], p_family = "roboto", p_size = 22, p_x = 1, p_y = 1.4,
        s_x = 1.0, s_y = 0.75, s_width = 0.4, s_height = 0.4,
        #url = "https://achubaty.github.io/grainscape", u_color = "#000000", u_size = 4,
        filename = "stickers/hexsticker.png", spotlight = FALSE)
