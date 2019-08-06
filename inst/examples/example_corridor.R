## Quick visualization of a corridor
corridorStartEnd <- rbind(c(10, 10), c(90, 90))
tinyPatchCorridor <- corridor(tinyPatchGOC, whichThresh = 3, coords = corridorStartEnd)
if (interactive())
  plot(tinyPatchCorridor)

## More control over a corridor visualization
if (interactive()) {
  plot(tinyPatchCorridor@voronoi, col = "lightgrey", lwd = 2)
  plot(tinyPatchCorridor@linksSP, col = "darkred", lty = "dashed", add = TRUE)
  plot(tinyPatchCorridor@nodesSP, col = "darkred", pch = 21, bg = "white", add = TRUE)
  plot(tinyPatchCorridor@shortestLinksSP, col = "darkred", lty = "solid", lwd = 2, add = TRUE)
  plot(tinyPatchCorridor@shortestNodesSP, col = "darkred", pch = 21, bg = "darkred", add = TRUE)
  mtext(paste("Corridor shortest path length:",
              round(tinyPatchCorridor@corridorLength, 2),
              "resistance units"), side = 1)
}
