## Three sets of coordinates in the study area
loc <- cbind(c(30, 60, 90), c(30, 60, 90))

## Find the GOC polygon containing these three locations
## for each of the 5 grains of connectivity
tinyPts <- point(tinyPatchGOC, loc)
