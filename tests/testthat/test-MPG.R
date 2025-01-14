test_that("MPG handles NA values correctly (#28)", {
  withr::local_package("igraph")
  withr::local_package("raster")

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
  skip_if_not_installed("dplyr")

  withr::local_package("igraph")
  withr::local_package("raster")

  ## simple map with no NA regions
  tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))

  g <- tinyMPG$mpg
  expect_equal(count_components(g), 1)

  ## simple map with NA regions
  tinyNA <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:95, 46:55] <- NA_integer_
  tinyNACost <- reclassify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  expect_equal(count_components(g), 1)

  ## simple map with non-connected subgraphs expected
  tinyNA <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyNA[1:100, 49:50] <- NA_integer_
  tinyNACost <- reclassify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  expect_equal(count_components(g), 2)

  ## more complex map with NA regions
  naError <- raster(system.file("extdata/naErrorExample.asc", package = "grainscape"))
  naErrorMPG <- MPG(cost = naError, patch = (naError == 1))

  g <- naErrorMPG$mpg
  expect_equal(count_components(g), 1)
})

test_that("least-cost paths are correctly calculated (#72)", {
  debug_cpp = FALSE
  # debug_cpp = TRUE

  patchMap <- system.file("extdata/issue72_patchID.tif", package = "grainscape") |>
    raster::raster()
  patchIDs <- which(!is.na(raster::getValues(patchMap)))
  patchMap[] <- 0L
  patchMap[patchIDs] <- 1L

  if (!debug_cpp && interactive()) {
    raster::plot(patchMap)
  }

  resistanceMap <- system.file("extdata/issue72_resistance.tif", package = "grainscape") |>
    raster::raster()

  if (!debug_cpp && interactive()) {
    raster::plot(resistanceMap)
  }

  combinedMap <- resistanceMap

  mapVals <- raster::getValues(resistanceMap) |> unique() |> sort() ## 1, 10, 1000
  patchVal <- 1L ## should be 1 (i.e., no additional resistance to movement than distance alone)

  combinedMap[patchIDs] <- patchVal

  ## crop to smaller extent to better visualize the issue [416 px]
  zoomExtent1 <- raster::extent(patchMap, r1 = 120, r2 = 145, c1 = 90, c2 = 105)
  patchMap_zoom1 <- raster::crop(patchMap, zoomExtent1)
  resistanceMap_zoom1 <- raster::crop(resistanceMap, zoomExtent1)
  combinedMap_zoom1 <- raster::crop(combinedMap, zoomExtent1)

  if (!debug_cpp && interactive()) {
    raster::plot(combinedMap_zoom1)
  }

  patchyMPG_zoom1 <- MPG(combinedMap_zoom1, patch = patchMap_zoom1)

  if (!debug_cpp && interactive()) {
    # plot(patchyMPG_zoom@mpgPlot)
    plot(patchyMPG_zoom1, quick = "mpgPlot", theme = FALSE)
  }

  lcpWeight_zoom1 <- raster::getValues(patchyMPG_zoom1@lcpPerimWeight) |> unique() |> na.omit()
  expect_true(lcpWeight_zoom1 == 3L)

  expect_true(nrow(graphdf(patchyMPG_zoom1)[[1]]$v) == 2)
  expect_true(nrow(graphdf(patchyMPG_zoom1)[[1]]$e) == 1)

  ## crop to different, larger extent with more patches (nodes) [1681 px]
  zoomExtent2 <- raster::extent(patchMap, r1 = 170, r2 = 210, c1 = 80, c2 = 120)
  patchMap_zoom2 <- raster::crop(patchMap, zoomExtent2)
  resistanceMap_zoom2 <- raster::crop(resistanceMap, zoomExtent2)
  combinedMap_zoom2 <- raster::crop(combinedMap, zoomExtent2)

  if (!debug_cpp && interactive()) {
    raster::plot(combinedMap_zoom2)
  }

  patchyMPG_zoom2 <- MPG(combinedMap_zoom2, patch = patchMap_zoom2)

  if (!debug_cpp && interactive()) {
    # plot(patchyMPG_zoom2@mpgPlot)
    plot(patchyMPG_zoom2, quick = "mpgPlot", theme = FALSE)
  }

  lcpWeight_zoom2 <- raster::getValues(patchyMPG_zoom2@lcpPerimWeight) |> unique() |> na.omit()
  expect_identical(as.integer(lcpWeight_zoom2), c(8L, 1L))

  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$v) == 3)
  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$e) == 2) ## TODO: remove dupes from cpp

  ## crop to even larger extent with more patches (nodes) [7371 px]
  zoomExtent3 <- raster::extent(patchMap, r1 = 120, r2 = 210, c1 = 50, c2 = 130)
  patchMap_zoom3 <- raster::crop(patchMap, zoomExtent3)
  resistanceMap_zoom3 <- raster::crop(resistanceMap, zoomExtent)
  combinedMap_zoom3 <- raster::crop(combinedMap, zoomExtent3)

  if (!debug_cpp && interactive()) {
    raster::plot(combinedMap_zoom3)
  }

  patchyMPG_zoom3 <- MPG(combinedMap_zoom3, patch = patchMap_zoom3) ## TODO: fix excessive RAM use

  if (!debug_cpp && interactive()) {
    # plot(patchyMPG_zoom2@mpgPlot)
    plot(patchyMPG_zoom3, quick = "mpgPlot", theme = FALSE)
    graphdf(patchyMPG_zoom3)[[1]]
  }

  lcpWeight_zoom3 <- raster::getValues(patchyMPG_zoom3@lcpPerimWeight) |> unique() |> na.omit()
  expect_identical(as.integer(lcpWeight_zoom3), c(8L, 1L)) ## TODO

  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$v) == 7) ## TODO
  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$e) == 9) ## TODO: remove dupes from cpp


  ## now with the full original extent [32550 px]
  patchyMPG <- MPG(combinedMap, patch = patchMap) ## TODO: fix excessive RAM use

  if (!debug_cpp && interactive()) {
    # plot(patchyMPG@mpgPlot)
    plot(patchyMPG, quick = "mpgPlot", theme = FALSE)
  }

  lcpWeights <- raster::getValues(patchyMPG@lcpPerimWeight) |> unique() |> na.omit()
  expect_identical(as.integer(lcpWeight_zoom3), c(8L, 1L)) ## TODO

  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$v) == 9) ## TODO
  expect_true(nrow(graphdf(patchyMPG_zoom2)[[1]]$e) == 13) ## TODO: remove dupes from cpp
})
