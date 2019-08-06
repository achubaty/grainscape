## Very quick visualization at the finest scale/grain/threshold
tinyPatchGOCgrain <- grain(tinyPatchGOC, whichThresh = 1)
if (interactive())
  plot(tinyPatchGOCgrain, col = topo.colors(10))

## Visualize the model at the finest scale/grain/threshold
## Manual control of plotting
if (interactive()) {
  plot(grain(tinyPatchGOC, whichThresh = 1)@voronoi,
       col = sample(rainbow(100)), legend = FALSE, main = "Threshold 1")
}
