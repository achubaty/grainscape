## Explore the graph structure and node/link attributes
graphdf(tinyPatchMPG)

## Find the mean patch area (see igraph manual for use of V() and E())
mean(igraph::V(tinyPatchMPG@mpg)$patchArea)

## Quick visualization of the MPG
if (interactive())
  plot(tinyPatchMPG, col = c("grey", "black"), legend = FALSE)

## Additional graph extraction scenarios
## Produce a lattice MPG where focal points are spaced 10 cells apart
tinyLatticeMPG <- MPG(cost = tinyCost, patch = 10)
if (interactive())
  plot(tinyLatticeMPG)
