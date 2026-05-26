test_that("point() returns expected list structure", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  loc <- cbind(c(30, 60, 90), c(30, 60, 90))
  pts <- grainscape::point(goc, loc)

  expected_names <- c("pointPolygon", "pointTotalPatchArea", "pointTotalCoreArea",
                      "pointECS", "pointECSCore")
  expect_named(pts, expected_names)
})

test_that("point() polygon matrix has nCoords rows and nThresh cols", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  loc <- cbind(c(30, 60, 90), c(30, 60, 90))
  pts <- grainscape::point(goc, loc)

  ncoords <- nrow(loc)
  nthresh <- nrow(goc@summary)

  expect_equal(nrow(pts$pointPolygon), ncoords)
  expect_equal(ncol(pts$pointPolygon), nthresh)
})

test_that("point() polygon IDs are positive integers", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  loc <- cbind(c(30, 60, 90), c(30, 60, 90))
  pts <- grainscape::point(goc, loc)

  ## last threshold in tiny.asc GOC has only 1 polygon (all connected), so may be NA
  valid_ids <- pts$pointPolygon[, seq_len(nrow(goc@summary) - 1)]
  expect_true(all(valid_ids > 0 | is.na(valid_ids)))
})

test_that("point() ECS values are non-negative", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  loc <- cbind(c(30, 60, 90), c(30, 60, 90))
  pts <- grainscape::point(goc, loc)

  ## pointECS is a vector of length nThresh
  expect_true(all(pts$pointECS >= 0 | is.nan(pts$pointECS)))
  expect_true(all(pts$pointECSCore >= 0 | is.nan(pts$pointECSCore)))
})

test_that("point() accepts sf input", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 3)
  mat <- cbind(c(30, 60, 90), c(30, 60, 90))
  sf_pts <- sf::st_as_sf(as.data.frame(mat), coords = c("V1", "V2"))

  pts_mat <- grainscape::point(goc, mat)
  pts_sf <- grainscape::point(goc, sf_pts)

  expect_equal(pts_mat$pointPolygon, pts_sf$pointPolygon)
})
