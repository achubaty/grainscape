test_that("corridor() returns a corridor object with all slots populated", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  coords <- rbind(c(10, 10), c(90, 90))
  corr <- corridor(goc, whichThresh = 3, coords = coords)

  expect_s4_class(corr, "corridor")
  expect_s4_class(corr@voronoi, "SpatRaster")
  expect_true(inherits(corr@linksSP, "sf"))
  expect_true(inherits(corr@nodesSP, "sf"))
  expect_true(inherits(corr@shortestLinksSP, "sf"))
  expect_true(inherits(corr@shortestNodesSP, "sf"))
  expect_type(corr@corridorLength, "double")
  expect_length(corr@corridorLength, 1)
  expect_true(corr@corridorLength >= 0)
})

test_that("corridor() corridorLength is non-negative and finite", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  coords <- rbind(c(10, 10), c(90, 90))

  for (i in seq_len(nrow(goc@summary))) {
    if (!is.na(goc@summary$nPolygon[i])) {
      corr <- corridor(goc, whichThresh = i, coords = coords)
      expect_true(is.finite(corr@corridorLength))
      expect_gte(corr@corridorLength, 0)
    }
  }
})

test_that("corridor() show() runs without error", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  coords <- rbind(c(10, 10), c(90, 90))
  corr <- corridor(goc, whichThresh = 3, coords = coords)

  expect_output(show(corr))
})

test_that("corridor() nodes lie within the voronoi raster extent", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  coords <- rbind(c(10, 10), c(90, 90))
  corr <- corridor(goc, whichThresh = 3, coords = coords)

  e <- terra::ext(corr@voronoi)
  node_coords <- sf::st_coordinates(corr@nodesSP)

  expect_true(all(node_coords[, 1] >= e$xmin))
  expect_true(all(node_coords[, 1] <= e$xmax))
  expect_true(all(node_coords[, 2] >= e$ymin))
  expect_true(all(node_coords[, 2] <= e$ymax))
})
