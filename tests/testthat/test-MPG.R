test_that("MPG handles NA values correctly (#28)", {
  library(igraph)
  library(raster)

  ## simplest case
  x <- 100
  y <- 100
  m <- matrix(2L, ncol = y, nrow = y)
  m[1:95, 46:55] <- NA_integer_
  m[16:35, 24:35] <- 3L
  m[16:35, 76:90] <- 3L

  r <- raster(m)
  c <- reclassify(r, rcl = cbind(c(1, 2, 3), c(10, 5, 1)))
  p <- (c == 1)

  mpg <- MPG(cost = c, patch = p)

  NAs <- which(is.na(p[])) # nolint

  ## no voronoi spread in the NA region
  ids_v <- which(mpg@voronoi[] > 0) # nolint
  expect_false(any(ids_v %in% NAs))

  ## no links in the NA region
  ids_l <- which(mpg@lcpLinkId[] < 0) # nolint
  expect_false(any(ids_l %in% NAs))

  ## more sophisticatied case
  tinyNA <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:95, 46:55] <- NA_integer_

  tinyNACost <- reclassify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

  tinyPatchMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  tinyNAs <- which(is.na(tinyNA[]))

  ## no voronoi spread in the NA region
  ids_v <- which(tinyPatchMPG@voronoi[] > 0) # nolint
  expect_false(any(ids_v %in% tinyNAs))

  ## no links in the NA region
  ids_l <- which(tinyPatchMPG@lcpLinkId[] < 0) # nolint
  expect_false(any(ids_l %in% tinyNAs))

  ## even more complex map with NA regions
  naError <- raster(system.file("extdata/naErrorExample.asc", package = "grainscape"))
  naErrorCost <- reclassify(naError, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  naErrorMPG <- MPG(cost = naErrorCost, patch = (naErrorCost == 1))

  naErrorNAs <- which(is.na(naError[]))

  ## no voronoi spread in the NA region
  ids_v <- which(naErrorMPG@voronoi[] > 0) # nolint
  expect_false(any(ids_v %in% naErrorNAs))

  ## no links in the NA region
  ids_l <- which(naErrorMPG@lcpLinkId[] < 0) # nolint
  expect_false(any(ids_l %in% naErrorNAs))
})

test_that("MPG contains links to all patches (#32)", {
  library(igraph)
  library(raster)

  ## simple map with no NA regions
  tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))

  g <- tinyMPG$mpg
  #g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  expect_equal(count_components(g), 1)

  ## simple map with NA regions
  tinyNA <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:95, 46:55] <- NA_integer_
  tinyNACost <- reclassify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  #g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  expect_equal(count_components(g), 1)

  ## simple map with non-connected subgraphs expected
  tinyNA <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:100, 49:50] <- NA_integer_
  tinyNACost <- reclassify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  #g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  expect_equal(count_components(g), 2)

  ## more complex map with NA regions
  naError <- raster(system.file("extdata/naErrorExample.asc", package = "grainscape"))
  naErrorMPG <- MPG(cost = naError, patch = (naError == 1))

  g <- naErrorMPG$mpg
  #g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  expect_equal(count_components(g), 1)
})
