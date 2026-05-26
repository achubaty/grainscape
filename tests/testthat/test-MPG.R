## Helper: terra equivalent of raster::extent(r, r1, r2, c1, c2)
.rowcol_ext <- function(r, r1, r2, c1, c2) {
  terra::ext(
    terra::xFromCol(r, c1) - terra::res(r)[1] / 2,
    terra::xFromCol(r, c2) + terra::res(r)[1] / 2,
    terra::yFromRow(r, r2) - terra::res(r)[2] / 2,
    terra::yFromRow(r, r1) + terra::res(r)[2] / 2
  )
}

test_that("MPG handles NA values correctly (#28)", {
  withr::local_package("igraph")

  ## simplest case
  x <- 100
  y <- 100
  m <- matrix(2L, ncol = y, nrow = y)
  m[1:95, 46:55] <- NA_integer_
  m[16:35, 24:35] <- 3L
  m[16:35, 76:90] <- 3L

  r <- terra::rast(m)
  c <- terra::classify(r, rcl = cbind(c(1, 2, 3), c(10, 5, 1)))
  p <- (c == 1)

  mpg <- MPG(cost = c, patch = p)

  NAs <- which(is.na(terra::values(p)[, 1])) # nolint

  ## no voronoi spread in the NA region
  ids_v <- which(terra::values(mpg@voronoi)[, 1] > 0) # nolint
  expect_false(any(ids_v %in% NAs))

  ## no links in the NA region
  ids_l <- which(terra::values(mpg@lcpLinkId)[, 1] < 0) # nolint
  expect_false(any(ids_l %in% NAs))

  ## more sophisticated case
  tinyNA <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:95, 46:55] <- NA_integer_

  tinyNACost <- terra::classify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

  tinyPatchMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  tinyNAs <- which(is.na(terra::values(tinyNA)[, 1]))

  ## no voronoi spread in the NA region
  ids_v <- which(terra::values(tinyPatchMPG@voronoi)[, 1] > 0) # nolint
  expect_false(any(ids_v %in% tinyNAs))

  ## no links in the NA region
  ids_l <- which(terra::values(tinyPatchMPG@lcpLinkId)[, 1] < 0) # nolint
  expect_false(any(ids_l %in% tinyNAs))

  ## even more complex map with NA regions
  naError <- terra::rast(system.file("extdata/naErrorExample.asc", package = "grainscape"))
  naErrorCost <- terra::classify(naError, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  naErrorMPG <- MPG(cost = naErrorCost, patch = (naErrorCost == 1))

  naErrorNAs <- which(is.na(terra::values(naError)[, 1]))

  ## no voronoi spread in the NA region
  ids_v <- which(terra::values(naErrorMPG@voronoi)[, 1] > 0) # nolint
  expect_false(any(ids_v %in% naErrorNAs))

  ## no links in the NA region
  ids_l <- which(terra::values(naErrorMPG@lcpLinkId)[, 1] < 0) # nolint
  expect_false(any(ids_l %in% naErrorNAs))
})

test_that("MPG contains links to all patches (#32)", {
  skip_if_not_installed("dplyr")

  withr::local_package("igraph")

  ## simple map with no NA regions
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))

  g <- tinyMPG$mpg
  expect_equal(count_components(g), 1)

  ## simple map with NA regions
  tinyNA <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:95, 46:55] <- NA_integer_
  tinyNACost <- terra::classify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  expect_equal(count_components(g), 1)

  ## simple map with non-connected subgraphs expected
  tinyNA <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:100, 49:50] <- NA_integer_
  tinyNACost <- terra::classify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  expect_equal(count_components(g), 2)

  ## more complex map with NA regions
  naError <- terra::rast(system.file("extdata/naErrorExample.asc", package = "grainscape"))
  naErrorMPG <- MPG(cost = naError, patch = (naError == 1))

  g <- naErrorMPG$mpg
  expect_equal(count_components(g), 1)
})

test_that("least-cost paths are correctly calculated (#72)", {
  debug_cpp <- FALSE
  # debug_cpp <- TRUE

  patchMap <- terra::rast(system.file("extdata/issue72_patchID.tif", package = "grainscape"))
  patchIDs <- which(!is.na(terra::values(patchMap)[, 1]))
  patchMap[] <- 0L
  patchMap[patchIDs] <- 1L

  resistanceMap <- terra::rast(
    system.file("extdata/issue72_resistance.tif", package = "grainscape")
  )

  combinedMap <- resistanceMap
  combinedMap[patchIDs] <- 1L  ## patchVal = 1

  ## crop to smaller extent to better visualize the issue [416 px]
  zoomExtent1 <- .rowcol_ext(patchMap, r1 = 120, r2 = 145, c1 = 90, c2 = 105)
  patchMap_zoom1 <- terra::crop(patchMap, zoomExtent1)
  combinedMap_zoom1 <- terra::crop(combinedMap, zoomExtent1)

  patchyMPG_zoom1 <- MPG(combinedMap_zoom1, patch = patchMap_zoom1)

  lcpWeight_zoom1 <- terra::values(patchyMPG_zoom1@lcpPerimWeight)[, 1] |>
    unique() |>
    na.omit()
  expect_true(lcpWeight_zoom1 == 3L)

  expect_true(nrow(graphdf(patchyMPG_zoom1)[[1]]$v) == 2)
  expect_true(nrow(graphdf(patchyMPG_zoom1)[[1]]$e) == 1)

  ## crop to different, larger extent with more patches (nodes) [1681 px]
  zoomExtent2 <- .rowcol_ext(patchMap, r1 = 170, r2 = 210, c1 = 80, c2 = 120)
  patchMap_zoom2 <- terra::crop(patchMap, zoomExtent2)
  combinedMap_zoom2 <- terra::crop(combinedMap, zoomExtent2)

  patchyMPG_zoom2 <- MPG(combinedMap_zoom2, patch = patchMap_zoom2)

  lcpWeight_zoom2 <- terra::values(patchyMPG_zoom2@lcpPerimWeight)[, 1] |>
    unique() |>
    na.omit()
  expect_identical(sort(as.integer(lcpWeight_zoom2)), c(1L, 8L))

  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$v) == 3)
  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$e) == 2)

  ## crop to even larger extent with more patches (nodes) [7371 px]
  zoomExtent3 <- .rowcol_ext(patchMap, r1 = 120, r2 = 210, c1 = 50, c2 = 130)
  patchMap_zoom3 <- terra::crop(patchMap, zoomExtent3)
  combinedMap_zoom3 <- terra::crop(combinedMap, zoomExtent3)

  patchyMPG_zoom3 <- MPG(combinedMap_zoom3, patch = patchMap_zoom3)

  lcpWeight_zoom3 <- terra::values(patchyMPG_zoom3@lcpPerimWeight)[, 1] |>
    unique() |>
    na.omit()
  expect_identical(sort(as.integer(lcpWeight_zoom3)), c(1L, 3L, 8L, 25L, 32L, 43L, 44L, 60L))

  expect_true(nrow(graphdf(patchyMPG_zoom3)[[1]]$v) == 7)
  expect_true(nrow(graphdf(patchyMPG_zoom3)[[1]]$e) == 8)

  ## now with the full original extent [32550 px]
  patchyMPG <- MPG(combinedMap, patch = patchMap)

  lcpWeights <- terra::values(patchyMPG@lcpPerimWeight)[, 1] |>
    unique() |>
    na.omit()
  expect_identical(
    sort(as.integer(lcpWeights)),
    c(1L, 3L, 8L, 25L, 32L, 43L, 44L, 60L, 148L, 150L, 181L, 194L)
  )

  expect_true(nrow(graphdf(patchyMPG)[[1]]$v) == 9)
  expect_true(nrow(graphdf(patchyMPG)[[1]]$e) == 12)
})
