test_that("$ gets and sets slots on mpg objects", {
  withr::local_package("igraph")
  mpg <- .tinyMPG()

  ## getter returns the named slot
  expect_identical(mpg$patchId, mpg@patchId)
  expect_s4_class(mpg$voronoi, "SpatRaster")

  ## replacement method updates the slot
  g <- mpg$mpg
  V(g)$flagged <- TRUE
  mpg$mpg <- g
  expect_true("flagged" %in% vertex_attr_names(mpg$mpg))
})

test_that("$ gets slots on goc objects", {
  goc <- .tinyGOC()
  expect_identical(goc$summary, goc@summary)
})
