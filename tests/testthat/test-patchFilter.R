test_that("patchFilter() errors when neither cells nor area is given", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  patch <- tinyCost == 10

  expect_error(patchFilter(patch))
})

test_that("patchFilter() errors when both cells and area are given", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  patch <- tinyCost == 10

  expect_error(patchFilter(patch, cells = 2, area = 100))
})

test_that("patchFilter() errors on non-binary raster", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

  expect_error(patchFilter(tinyCost, cells = 2))
})

test_that("patchFilter() with cells removes small patches", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  patch <- tinyCost == 10

  ## small cell threshold keeps more patches
  fp_small <- patchFilter(patch, cells = 2)
  ## large cell threshold removes more patches
  fp_large <- patchFilter(patch, cells = 40)

  n_small <- sum(terra::values(fp_small)[, 1] == 1, na.rm = TRUE)
  n_large <- sum(terra::values(fp_large)[, 1] == 1, na.rm = TRUE)

  expect_gte(n_small, n_large)
  expect_s4_class(fp_small, "SpatRaster")
  expect_s4_class(fp_large, "SpatRaster")
})

test_that("patchFilter() with area removes patches below area threshold", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  patch <- tinyCost == 10

  res <- terra::res(patch)
  cell_area <- res[1] * res[2]

  fp_cells <- patchFilter(patch, cells = 40)
  fp_area <- patchFilter(patch, area = 40 * cell_area)

  ## cells and equivalent area should give identical results
  expect_equal(terra::values(fp_cells)[, 1], terra::values(fp_area)[, 1])
})

test_that("patchFilter() with directions=4 is more conservative than default (8)", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  patch <- tinyCost == 10

  fp8 <- patchFilter(patch, cells = 5)
  fp4 <- patchFilter(patch, cells = 5, directions = 4)

  n8 <- sum(terra::values(fp8)[, 1] == 1, na.rm = TRUE)
  n4 <- sum(terra::values(fp4)[, 1] == 1, na.rm = TRUE)

  ## 4-direction is more restrictive: fewer or equal cells retained
  expect_lte(n4, n8)
})

test_that("patchFilter() output is a binary raster", {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  patch <- tinyCost == 10

  fp <- patchFilter(patch, cells = 2)
  vals <- unique(terra::values(fp)[, 1])

  expect_true(all(vals %in% c(0, 1, NA)))
})
