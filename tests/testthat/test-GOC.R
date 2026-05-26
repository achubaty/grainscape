test_that("GOC() returns a goc object with the correct structure", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  goc <- GOC(mpg, nThresh = 5)

  expect_s4_class(goc, "goc")
  expect_s4_class(goc@voronoi, "SpatRaster")
  expect_s3_class(goc@summary, "data.frame")
  expect_type(goc@th, "list")
  expect_length(goc@th, 5)
})

test_that("GOC() nThresh produces at most nThresh unique thresholds", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  goc5 <- GOC(mpg, nThresh = 5)
  goc3 <- GOC(mpg, nThresh = 3)

  expect_lte(nrow(goc5@summary), 5)
  expect_lte(nrow(goc3@summary), 3)
  expect_true(nrow(goc5@summary) >= nrow(goc3@summary))
})

test_that("GOC() doThresh uses the specified thresholds", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  doT <- c(0, 20, 40)
  goc <- GOC(mpg, doThresh = doT)

  expect_s4_class(goc, "goc")
  expect_length(goc@th, length(doT))
})

test_that("GOC() summary has expected columns", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  expected_cols <- c("id", "maxLink", "nPolygon", "ECS", "ECSCore")
  expect_true(all(expected_cols %in% names(goc@summary)))
})

test_that("GOC() th elements contain igraph objects for defined thresholds", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  defined <- which(!is.na(goc@summary$nPolygon))
  for (i in defined) {
    expect_true(is(goc@th[[i]]$goc, "igraph"))
  }
})

test_that("GOC() show() runs without error", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  expect_output(show(goc))
})

test_that("$ accessor works for goc slots", {
  withr::local_package("igraph")

  goc <- .tinyGOC()

  expect_s4_class(goc$voronoi, "SpatRaster")
  expect_s3_class(goc$summary, "data.frame")
  expect_type(goc$th, "list")
})

test_that("$<- replacement works for goc slots", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  orig_summary <- goc$summary
  orig_summary$testCol <- 99L
  goc$summary <- orig_summary

  expect_true("testCol" %in% names(goc$summary))
  expect_equal(goc$summary$testCol, rep(99L, nrow(goc$summary)))
})

test_that("$ accessor works for mpg slots", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()

  expect_true(is_igraph(mpg$mpg))
  expect_s4_class(mpg$patchId, "SpatRaster")
  expect_s4_class(mpg$voronoi, "SpatRaster")
  expect_s4_class(mpg$lcpPerimWeight, "SpatRaster")
  expect_s4_class(mpg$lcpLinkId, "SpatRaster")
  expect_s4_class(mpg$mpgPlot, "SpatRaster")
})
