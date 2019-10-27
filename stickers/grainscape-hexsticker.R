#install.packages("hexSticker")
#if (Sys.info()["sysname"] == "Darwin") install.packages("sysfonts")

library(ggplot2)
library(hexSticker)
library(showtext)
library(sysfonts)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto Slab", "roboto")
## Automatically use showtext to render text for future devices
showtext_auto()

## build plot
patchy <- raster(system.file("extdata/patchy.asc", package = "grainscape"))
isBecomes <- cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6))
patchyCost <- reclassify(patchy, rcl = isBecomes)
patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))

theme <- theme_grainscape()
theme_set(theme)

sticker(expression(plot(patchyMPG, quick = "mpgPlot", theme = FALSE)),
        package = "grainscape",
        h_color = "darkred", h_fill = "#cccccc",
        p_color = "darkred", p_family = "bree", p_size = 22, p_x = 1, p_y = 1.55,
        s_x = 0.8, s_y = 0.75, s_width = 1.2, s_height = 1.2,
        #url = "https://achubaty.github.io/grainscape", u_color = "#000000", u_size = 4,
        filename = "stickers/hexsticker.png", spotlight = FALSE)
