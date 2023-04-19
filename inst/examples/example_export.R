## Export rasters and vectors and place in an R object
sp_tinyPatchGOC <- export(grain(tinyPatchGOC, 2), R = TRUE)  # nolint
sp_tinyPatchMPG <- export(tinyPatchMPG, R = TRUE) # nolint

## Export raster and vectors to a specified directory
exportPath <- tempdir()
export(grain(tinyPatchGOC, 2), dirname = "tiny_goc_thresh2", path = exportPath)
export(tinyPatchMPG, dirname = "tiny_mpg", path = exportPath, vorBound = TRUE)

## clean up
unlink(file.path(exportPath, "tiny_goc_thresh2"), recursive = TRUE)
unlink(file.path(exportPath, "tiny_mpg"), recursive = TRUE)
