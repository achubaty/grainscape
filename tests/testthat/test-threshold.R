test_that("threshold() returns expected structure with nThresh", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  th <- threshold(mpg, nThresh = 10)

  expect_type(th, "list")
  expect_named(th, c("summary", "th"))
  expect_s3_class(th$summary, "data.frame")
  expect_type(th$th, "list")
  expect_lte(length(th$th), 10)
})

test_that("threshold() returns expected structure with doThresh", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  doT <- c(0, 10, 50, 100)
  th <- threshold(mpg, doThresh = doT)

  expect_length(th$th, length(doT))
  expect_equal(nrow(th$summary), length(doT))
})

test_that("threshold() graphs are igraph objects", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  th <- threshold(mpg, nThresh = 5)

  for (i in seq_along(th$th)) {
    expect_true(is_igraph(th$th[[i]]))
  }
})

test_that("threshold() graphs have decreasing component counts as threshold increases", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  th <- threshold(mpg, nThresh = 5)

  ncomp <- vapply(th$th, count_components, numeric(1))
  ## Components should be non-increasing (more connections at higher thresholds)
  expect_true(all(diff(ncomp) <= 0))
})

test_that("threshold() summary has expected columns", {
  withr::local_package("raster")
  withr::local_package("igraph")

  mpg <- .tinyMPG()
  th <- threshold(mpg, nThresh = 5)

  expect_true("maxLink" %in% names(th$summary))
  expect_true("nComponents" %in% names(th$summary))
})
