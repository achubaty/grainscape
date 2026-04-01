test_that("distance() with matrix coords returns a list with th", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  loc <- cbind(c(30, 60, 90), c(30, 60, 90))
  d <- grainscape::distance(goc, loc)

  expect_type(d, "list")
  expect_named(d, "th")
  expect_length(d$th, 5)
})

test_that("distance() grainD matrices are symmetric, square, and non-negative", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 5)
  loc <- cbind(c(30, 60, 90), c(30, 60, 90))
  d <- grainscape::distance(goc, loc)

  for (i in seq_along(d$th)) {
    if (!is.list(d$th[[i]])) next  ## NA for undefined thresholds
    mat <- d$th[[i]]$grainD
    if (is.matrix(mat)) {
      expect_equal(nrow(mat), ncol(mat))
      expect_true(isSymmetric(mat))
      expect_true(all(mat >= 0))
      ## diagonal should be zero (ignore names)
      expect_equal(unname(diag(mat)), rep(0, nrow(mat)))
    }
  }
})

test_that("distance() with SpatialPoints input works", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 3)
  mat <- cbind(c(30, 60, 90), c(30, 60, 90))
  sp_pts <- sp::SpatialPoints(mat)
  d <- grainscape::distance(goc, sp_pts)

  expect_type(d, "list")
  expect_length(d$th, 3)
})

test_that("distance() matrix and SpatialPoints give same result", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 3)
  mat <- cbind(c(30, 60, 90), c(30, 60, 90))
  sp_pts <- sp::SpatialPoints(mat)

  d_mat <- grainscape::distance(goc, mat)
  d_sp <- grainscape::distance(goc, sp_pts)

  for (i in seq_along(d_mat$th)) {
    if (!is.list(d_mat$th[[i]])) next
    m1 <- d_mat$th[[i]]$grainD
    m2 <- d_sp$th[[i]]$grainD
    if (is.matrix(m1) && is.matrix(m2)) {
      expect_equal(m1, m2)
    }
  }
})

test_that("distance() errors on wrong weight attribute name", {
  withr::local_package("igraph")

  goc <- .tinyGOC(nThresh = 3)
  mat <- cbind(c(30, 60), c(30, 60))

  expect_error(grainscape::distance(goc, mat, weight = "nonExistentAttr"))
})
