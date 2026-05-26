test_that("graphdf() for mpg returns correct structure", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  gdf <- graphdf(mpg)

  expect_type(gdf, "list")
  expect_length(gdf, 1)
  expect_named(gdf[[1]], c("v", "e"))
  expect_s3_class(gdf[[1]]$v, "data.frame")
  expect_s3_class(gdf[[1]]$e, "data.frame")
})

test_that("graphdf() for mpg v has expected vertex attribute columns", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  gdf <- graphdf(mpg)

  expected_v_cols <- c(
    "patchId",
    "patchArea",
    "patchEdgeArea",
    "coreArea",
    "centroidX",
    "centroidY"
  )
  expect_true(all(expected_v_cols %in% names(gdf[[1]]$v)))
})

test_that("graphdf() for mpg e has expected edge attribute columns", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  gdf <- graphdf(mpg)

  expected_e_cols <- c("e1", "e2", "linkId", "lcpPerimWeight")
  expect_true(all(expected_e_cols %in% names(gdf[[1]]$e)))
})

test_that("graphdf() row counts match igraph vertex and edge counts", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  gdf <- graphdf(mpg)
  g <- mpg@mpg

  expect_equal(nrow(gdf[[1]]$v), vcount(g))
  expect_equal(nrow(gdf[[1]]$e), ecount(g))
})

test_that("graphdf() for goc returns a list of length nThresh", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  gdf <- graphdf(goc)

  expect_type(gdf, "list")
  expect_length(gdf, 5)
  for (i in seq_along(gdf)) {
    expect_named(gdf[[i]], c("v", "e"))
  }
})

test_that("graphdf() for grain returns correct structure", {
  withr::local_package("igraph")

  goc <- .tinyGOC()
  gr <- grain(goc, whichThresh = 3)
  gdf <- graphdf(gr)

  expect_type(gdf, "list")
  expect_length(gdf, 1)
  expect_named(gdf[[1]], c("v", "e"))
  expect_s3_class(gdf[[1]]$v, "data.frame")
  expect_s3_class(gdf[[1]]$e, "data.frame")
})

test_that("graphdf() for igraph returns correct structure", {
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  g <- mpg@mpg
  gdf <- graphdf(g)

  expect_type(gdf, "list")
  expect_length(gdf, 1)
  expect_named(gdf[[1]], c("v", "e"))
  expect_equal(nrow(gdf[[1]]$v), vcount(g))
  expect_equal(nrow(gdf[[1]]$e), ecount(g))
})

test_that("graphdf() handles single-edge graphs without transposing rows/cols", {
  withr::local_package("igraph")

  ## two-vertex, one-edge graph
  g <- igraph::make_graph(c(1, 2), directed = FALSE)
  igraph::E(g)$weight <- 42
  gdf <- graphdf(g)

  expect_equal(nrow(gdf[[1]]$e), 1)
  expect_equal(ncol(gdf[[1]]$e), 3) ## e1, e2, weight
})
