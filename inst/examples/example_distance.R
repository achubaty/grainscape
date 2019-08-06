## Three sets of coordinates in the study area
loc <- cbind(c(30, 60, 90), c(30, 60, 90))

## Find the GOC network distance matrices between these points
## for each of the 5 grains of connectivity
tinyDist <- grainscape::distance(tinyPatchGOC, loc)
