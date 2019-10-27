## Threshold this graph at a representative subset of 10 thresholds
tinyThresh <- threshold(tinyPatchMPG, nThresh = 10)

## Examine the properties of one of these threshold graphs
print(tinyThresh$th[[7]], vertex = TRUE, edge = TRUE)
