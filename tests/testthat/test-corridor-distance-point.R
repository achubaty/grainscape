## Make a random raster with a border of NA values
.makeRaster <- function(dim, naBorder = NULL,
                        FUN = function() floor(rgamma(dim * dim, 2.5)) + 1) { # nolint
  ras <- raster::raster(xmn = 0, xmx = dim, ymn = 0, ymx = dim, resolution = 1)
  ras[] <- FUN() # nolint
  if (!is.null(naBorder)) {
    ras[1:naBorder, ] <- NA
    ras[, 1:naBorder] <- NA
    ras[(dim - naBorder):dim, ] <- NA
    ras[, (dim - naBorder):dim] <- NA
  }
  return(ras)
}

test_that("corridor handles NA values", {
  ## based on https://github.com/achubaty/grainscape/issues/50 # nolint

  ## Create a random raster with a border of five NA cells
  cost <- .makeRaster(100, 5)

  ## Extract MPG and GOC
  mpg <- MPG(cost, cost == 1)
  goc <- GOC(mpg, nThresh = 5)

  ## Create coordinates inside and outside NA border
  coordNoneNA <- rbind(c(10, 10), c(90, 90))
  coordSomeNA <- rbind(c(2, 2), c(90, 90))
  coordAllNA <- rbind(c(2, 2), c(98, 98))

  expect_warning(corridorNoneNA <- corridor(goc, whichThresh = 2, coordNoneNA), NA) ## OK
  expect_warning(corridorSomeNA <- corridor(goc, whichThresh = 2, coordSomeNA))
  expect_error(corridorAllNA <- corridor(goc, whichThresh = 2, coordAllNA))
})

test_that("distance handles NA values", {
  ## based on https://github.com/achubaty/grainscape/issues/50 # nolint

  ## Create a random raster with a border of five NA cells
  cost <- .makeRaster(100, 5)

  ## Extract MPG and GOC
  mpg <- MPG(cost, cost == 1)
  goc <- GOC(mpg, nThresh = 5)

  ## Create coordinates inside and outside NA border
  coordNoneNA <- rbind(c(10, 10), c(90, 90))
  coordSomeNA <- rbind(c(2, 2), c(90, 90))
  coordAllNA <- rbind(c(2, 2), c(98, 98))

  ## Fails
  expect_warning(distanceNoneNA <- distance(goc, coordNoneNA), NA)
  expect_warning(distanceSomeNA <- distance(goc, coordSomeNA))
  expect_warning(distanceAllNA <- distance(goc, coordAllNA))
})

test_that("point handles NA values", {
  ## based on https://github.com/achubaty/grainscape/issues/50 # nolint

  ## Create a random raster with a border of five NA cells
  cost <- .makeRaster(100, 5)

  ## Extract MPG and GOC
  mpg <- MPG(cost, cost == 1)
  goc <- GOC(mpg, nThresh = 5)

  ## Create coordinates inside and outside NA border
  coordNoneNA <- rbind(c(10, 10), c(90, 90))
  coordSomeNA <- rbind(c(2, 2), c(90, 90))
  coordAllNA <- rbind(c(2, 2), c(98, 98))

  ## names of list items returned by `point`
  pntNames <- c("pointPolygon", "pointTotalPatchArea", "pointTotalCoreArea",
                "pointECS", "pointECSCore")

  ## no NA coords; only column 5 of the result is NA
  pntNoneNA <- point(goc, coordNoneNA)
  expect_identical(names(pntNoneNA), pntNames)

  for (i in pntNames[1:3]) {
    for (j in 1:4) {
      expect_false(any(vapply(pntNoneNA[[i]][, j], is.na, logical(1)))) ## cols 1:4 OK
    }
    expect_true(all(vapply(pntNoneNA[[i]][, 5], is.na, logical(1)))) ## col 5 is NA
  }
  for (i in pntNames[4:5]) {
    expect_false(any(vapply(pntNoneNA[[pntNames[i]]][1:4], is.na, logical(1)))) ## items 1:4 OK
    expect_true(all(vapply(pntNoneNA[[pntNames[i]]][5], is.na, logical(1)))) ## item 5 is NA
  }

  ## some coords NA; second row of coords are NA
  expect_warning(pntSomeNA <- point(goc, coordSomeNA))
  for (i in pntNames[1:3]) {
    for (j in 1:4) {
      expect_true(all(vapply(pntSomeNA[[i]][1, j], is.na, logical(1)))) ## row 1, cols 1:4 NA
      expect_false(any(vapply(pntSomeNA[[i]][2, j], is.na, logical(1)))) ## row 2, cols 1:4 OK
    }
    expect_true(all(vapply(pntSomeNA[[i]][, 5], is.na, logical(1)))) ## col 5 is NA
  }
  for (i in pntNames[4:5]) {
    expect_false(any(vapply(pntSomeNA[[pntNames[i]]][1:4], is.na, logical(1)))) ## items 1:4 OK
    expect_true(all(vapply(pntSomeNA[[pntNames[i]]][5], is.na, logical(1)))) ## item 5 is NaN
  }

  ## all coords NA
  expect_warning(pntAllNA <- point(goc, coordAllNA))
  expect_true(all(is.na(unlist(pntAllNA))))
})
