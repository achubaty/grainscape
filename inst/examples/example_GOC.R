## Examine the properties of the GOC graph of grain 3 of 5
graphdf(grain(tinyPatchGOC, whichThresh = 3))

## Extract grains of connectivity
## representation of the finest grain and three others
## by giving thresholds in link weights (doThresh)
tinyPatchGOC <- GOC(tinyPatchMPG, doThresh = c(0, 20, 40))
