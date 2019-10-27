## Export raster and vectors of a grain to a specified directory
tmpdir <- tempdir()
export(grain(tinyPatchGOC, 2), dirname = "tiny_goc_thresh2", path = tmpdir)
unlink(file.path(tmpdir, "tiny_goc_thresh2"), recursive = TRUE)

## Export rasters and vectors of the MPG and place in an R object
sp_tinyPatchMPG <- export(tinyPatchMPG, R = TRUE)
