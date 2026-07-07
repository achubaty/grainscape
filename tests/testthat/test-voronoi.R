test_that("MPG cost-Voronoi matches a Euclidean Voronoi on a uniform surface (terra::voronoi)", {
  withr::local_options(
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE
  )

  ## On a uniform resistance surface the cost-allocation Voronoi computed by MPG() reduces to
  ## a nearest-patch (Euclidean) allocation, so it should closely match terra::voronoi() of the
  ## patch seeds. They differ only by a few percent at region boundaries, owing to MPG()'s
  ## 4-connected marching (vs Euclidean geometry) and raster discretisation.
  r <- terra::rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100, crs = "")
  terra::values(r) <- 1
  pts <- cbind(
    c(20, 80, 50, 25, 75, 52, 15, 85),
    c(20, 22, 50, 80, 78, 15, 60, 55)
  )
  patchR <- r
  terra::values(patchR) <- 0
  patchR[terra::cellFromXY(patchR, pts)] <- 1

  mpg <- MPG(r, patch = patchR)

  gVor <- terra::values(mpg@voronoi)[, 1]
  gPid <- terra::values(mpg@patchId)[, 1]

  ## independent Euclidean Voronoi of the same seeds, clipped to the raster, then rasterised
  sv <- terra::vect(pts, type = "points")
  tv <- terra::voronoi(sv, bnd = terra::ext(r))
  tv$pidx <- seq_len(nrow(tv))
  tVor <- terra::values(terra::rasterize(tv, r, "pidx"))[, 1]

  ## relabel terra polygon ids to the matching grainscape patch ids (using the seed cells)
  seedCells <- terra::cellFromXY(r, pts)
  relabel <- stats::setNames(gPid[seedCells], as.character(tVor[seedCells]))
  tVorMapped <- relabel[as.character(tVor)]

  ## every seed must lie in its own (distinct) grainscape patch
  expect_length(unique(gPid[seedCells]), nrow(pts))

  ok <- !is.na(gVor) & gVor > 0 & !is.na(tVorMapped)
  agreement <- mean(gVor[ok] == tVorMapped[ok])
  expect_gt(agreement, 0.9)
})
