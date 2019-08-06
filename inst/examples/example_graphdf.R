## Create a data.frame with the structure and attributes of a MPG object
tinyPatchMPG_df <- graphdf(tinyPatchMPG)

## Create a data.frame with the structure and attributes of a GOC object
tinyPatchGOC_df <- graphdf(tinyPatchGOC)

## Create a data.frame with the structure and attributes of any igraph object
graphdf(tinyPatchGOC@th[[1]]$goc)
