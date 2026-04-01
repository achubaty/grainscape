test_that("ggGS() for mpg 'patchId' returns data.frame with x, y, value", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  df <- ggGS(mpg, "patchId")

  expect_s3_class(df, "data.frame")
  expect_named(df, c("value", "x", "y"))
  expect_gt(nrow(df), 0)
})

test_that("ggGS() for mpg 'voronoi' returns data.frame with x, y, value", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  df <- ggGS(mpg, "voronoi")

  expect_s3_class(df, "data.frame")
  expect_named(df, c("value", "x", "y"))
})

test_that("ggGS() for mpg 'links' returns data.frame with link coordinate columns", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  df <- ggGS(mpg, "links")

  expect_s3_class(df, "data.frame")
  expect_true(all(c("x1p", "y1p", "x2p", "y2p") %in% names(df)))
})

test_that("ggGS() for grain 'voronoi' returns data.frame with x, y, value", {
  withr::local_package("raster")
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 3)
  df <- ggGS(gr, "voronoi")

  expect_s3_class(df, "data.frame")
  expect_named(df, c("value", "x", "y"))
})

test_that("ggGS() for grain 'vorBound' returns data.frame with x, y, value", {
  withr::local_package("raster")
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 3)
  df <- ggGS(gr, "vorBound")

  expect_s3_class(df, "data.frame")
  expect_named(df, c("value", "x", "y"))
})

test_that("ggGS() for grain 'nodes' returns data.frame with x, y columns", {
  withr::local_package("raster")
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 3)
  df <- ggGS(gr, "nodes")

  expect_s3_class(df, "data.frame")
  expect_true(all(c("x", "y") %in% names(df)))
})

test_that("ggGS() for grain 'links' returns data.frame with link coordinate columns", {
  withr::local_package("raster")
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 3)
  df <- ggGS(gr, "links")

  expect_s3_class(df, "data.frame")
  expect_true(all(c("x1", "y1", "x2", "y2") %in% names(df)))
})

test_that("ggGS() for RasterLayer returns data.frame with x, y, value", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  df <- ggGS(mpg@patchId, "patchId")

  expect_s3_class(df, "data.frame")
  expect_named(df, c("value", "x", "y"))
})

test_that("theme_grainscape() returns a ggplot2 theme", {
  skip_if_not_installed("ggplot2")

  th <- theme_grainscape()

  expect_s3_class(th, "theme")
  expect_s3_class(th, "gg")
})
