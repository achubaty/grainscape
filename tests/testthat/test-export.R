test_that("export() handles user-added node attributes (#71)", {
  withr::local_package("igraph")

  tiny <- raster::raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- raster::reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)

  tiny_mpg_graph <- tinyPatchMPG$mpg
  ig <- graph_from_adjacency_matrix(as_adjacency_matrix(tiny_mpg_graph), mode = "undirected")
  betweenness_scores <- betweenness(ig)

  V(tinyPatchMPG$mpg)$betweenness <- betweenness_scores

  ## Export raster and vectors to a specified directory
  exportPath <- tempdir()
  fout <- export(
    tinyPatchMPG,
    dirname = "tiny_mpg",
    path = exportPath,
    vorBound = TRUE,
    overwrite = TRUE
  )
  expect_true(file.exists(fout))

  unlink(file.path(exportPath, "tiny_mpg"), recursive = TRUE)
})
