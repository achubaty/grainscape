test_that("grain() returns a grain object with the correct slots", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 1)

  expect_s4_class(gr, "grain")
  expect_s4_class(gr@voronoi, "SpatRaster")
  expect_s3_class(gr@summary, "data.frame")
  expect_true(inherits(gr@centroids, "sf"))
  expect_true(is_igraph(gr@th))
})

test_that("grain() extracts the correct threshold", {
  withr::local_package("igraph")

  goc <- .tinyGOC()

  gr1 <- grain(goc, whichThresh = 1)
  gr4 <- grain(goc, whichThresh = 4) ## threshold 5 is undefined (NA) in tiny.asc

  ## finer grain (lower threshold) has more polygons
  expect_gte(vcount(gr1@th), vcount(gr4@th))
})

test_that("grain() errors on invalid whichThresh", {
  withr::local_package("igraph")

  goc <- .tinyGOC()

  expect_error(grain(goc, whichThresh = 0))
  expect_error(grain(goc, whichThresh = 99))
  expect_error(grain(goc, whichThresh = c(1, 2)))
  ## threshold 5 has nPolygon = NA in tiny.asc GOC
  expect_error(grain(goc, whichThresh = 5))
})

test_that("grain() show() runs without error", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 1)

  expect_output(show(gr))
})

test_that("grain() centroids match the number of polygons", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 3) ## threshold 3 is defined (nPolygon not NA)

  expect_equal(nrow(gr@centroids), vcount(gr@th))
})
