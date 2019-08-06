## Load raster landscape
tiny <- raster::raster(system.file("extdata/tiny.asc", package = "grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- raster::reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

