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

  NAs <- which(is.na(terra::values(p)[, 1]))

  ## no voronoi spread in the NA region
  ids_v <- which(terra::values(mpg@voronoi)[, 1] > 0)
  expect_false(any(ids_v %in% NAs))

  ## no links in the NA region
  ids_l <- which(terra::values(mpg@lcpLinkId)[, 1] < 0)
  expect_false(any(ids_l %in% NAs))

  ## more sophisticated case
  tinyNA <- terra::rast(
    system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
  )
  tinyNA[1:95, 46:55] <- NA_integer_

  tinyNACost <- terra::classify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

  tinyPatchMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  tinyNAs <- which(is.na(terra::values(tinyNA)[, 1]))

  ## no voronoi spread in the NA region
  ids_v <- which(terra::values(tinyPatchMPG@voronoi)[, 1] > 0)
  expect_false(any(ids_v %in% tinyNAs))

  ## no links in the NA region
  ids_l <- which(terra::values(tinyPatchMPG@lcpLinkId)[, 1] < 0)
  expect_false(any(ids_l %in% tinyNAs))

  ## even more complex map with NA regions
  naError <- terra::rast(test_path("fixtures", "naErrorExample.asc"))
  naErrorCost <- terra::classify(naError, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  naErrorMPG <- MPG(cost = naErrorCost, patch = (naErrorCost == 1))

  naErrorNAs <- which(is.na(terra::values(naError)[, 1]))

  ## no voronoi spread in the NA region
  ids_v <- which(terra::values(naErrorMPG@voronoi)[, 1] > 0)
  expect_false(any(ids_v %in% naErrorNAs))

  ## no links in the NA region
  ids_l <- which(terra::values(naErrorMPG@lcpLinkId)[, 1] < 0)
  expect_false(any(ids_l %in% naErrorNAs))
})

test_that("MPG contains links to all patches (#32)", {
  skip_if_not_installed("dplyr")

  withr::local_package("igraph")

  ## simple map with no NA regions
  tiny <- terra::rast(
    system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
  )
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))

  g <- tinyMPG$mpg
  expect_equal(count_components(g), 1)

  ## simple map with NA regions
  tinyNA <- terra::rast(
    system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
  )
  tinyNA[1:95, 46:55] <- NA_integer_
  tinyNACost <- terra::classify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  expect_equal(count_components(g), 1)

  ## simple map with non-connected subgraphs expected
  tinyNA <- terra::rast(
    system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
  )
  tinyNA[1:100, 49:50] <- NA_integer_
  tinyNACost <- terra::classify(tinyNA, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyNAMPG <- MPG(cost = tinyNACost, patch = (tinyNACost == 1))

  g <- tinyNAMPG$mpg
  expect_equal(count_components(g), 2)

  ## more complex map with NA regions
  naError <- terra::rast(test_path("fixtures", "naErrorExample.asc"))
  naErrorMPG <- MPG(cost = naError, patch = (naError == 1))

  g <- naErrorMPG$mpg
  expect_equal(count_components(g), 1)
})

test_that("least-cost paths are correctly calculated (#72)", {
  debug_cpp <- FALSE
  # debug_cpp <- TRUE

  patchMap <- terra::rast(test_path("fixtures", "issue72_patchID.tif"))
  patchIDs <- which(!is.na(terra::values(patchMap)[, 1]))
  patchMap[] <- 0L
  patchMap[patchIDs] <- 1L

  resistanceMap <- terra::rast(test_path("fixtures", "issue72_resistance.tif"))

  combinedMap <- resistanceMap
  combinedMap[patchIDs] <- 1L ## patchVal = 1

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

test_that("MPG does not drop non-redundant links via a bogus indirect path (#72)", {
  ## Regression test: lookForIndirectPath() could use an unset Cell id (the -99 sentinel
  ## from DataStruct.h) as a pivot patch, fabricating a cheap two-hop A -> -99 -> B that
  ## suppressed a legitimate direct A-B link. It surfaced on noisy resistance surfaces (the
  ## resistance-surface network in the vignette), where strongly Voronoi-adjacent patches
  ## lost their direct link. A patch pair sharing a large Voronoi boundary has the cheapest
  ## possible direct connection and can never be redundant, so the most-adjacent pairs must
  ## always be directly linked.
  set.seed(674)
  res <- terra::rast(xmin = 0, xmax = 100, ymin = 0, ymax = 100, resolution = 1)
  res[] <- 1
  pts <- data.frame(
    x = rep(seq(10, 90, length.out = 5), 4),
    y = seq(10, 90, length.out = 4)
  ) +
    cbind(runif(20) * 10, runif(20) * 10)
  patchPts <- terra::setValues(res, 0)
  patchPts[terra::cellFromXY(patchPts, pts)] <- 1
  res2 <- res
  res2[] <- floor(runif(terra::ncell(res2)) * 10 + 1) # high-variance resistance (1..10)

  mpg <- MPG(res2, patch = patchPts)
  edges <- graphdf(mpg)[[1]]$e

  ## every link must connect two real patches (patch ids are positive; -99 is the Cell sentinel)
  expect_true(all(edges$e1 > 0 & edges$e2 > 0))

  ## shared Voronoi boundary length (8-neighbour) for every adjacent patch pair
  linkset <- apply(edges[, c("e1", "e2")], 1, function(z) paste(min(z), max(z)))
  vor <- terra::as.matrix(mpg@voronoi, wide = TRUE)
  nr <- nrow(vor)
  nc <- ncol(vor)
  bnd <- new.env(parent = emptyenv())
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      id <- vor[i, j]
      if (is.na(id) || id <= 0) {
        next
      }
      for (d in list(c(0L, 1L), c(1L, 0L), c(1L, 1L), c(1L, -1L))) {
        ii <- i + d[1]
        jj <- j + d[2]
        if (ii < 1 || ii > nr || jj < 1 || jj > nc) {
          next
        }
        nb <- vor[ii, jj]
        if (is.na(nb) || nb <= 0 || nb == id) {
          next
        }
        k <- paste(min(id, nb), max(id, nb))
        assign(k, (if (exists(k, bnd, inherits = FALSE)) get(k, bnd) else 0L) + 1L, bnd)
      }
    }
  }
  keys <- ls(bnd)
  boundaryLen <- vapply(keys, function(k) get(k, bnd), integer(1))

  ## the five most strongly Voronoi-adjacent patch pairs must all be directly linked
  ## (before the fix, the 163-cell pair "11 19" was wrongly suppressed via pivot -99)
  topPairs <- keys[order(-boundaryLen)][1:5]
  expect_true(all(topPairs %in% linkset))

  ## no patch is left isolated: single-cell patches whose iLinkMap id was never set (-99)
  ## used to be unresolved link endpoints and dropped, leaving them disconnected (#72).
  ## On this fully-connected landscape every patch must belong to one component.
  expect_equal(igraph::count_components(mpg$mpg), 1L)
})

test_that("MPG link weights equal independent least-cost paths", {
  ## Correctness check that does not trust the C++ engine: recompute every MPG link weight
  ## with a standalone igraph Dijkstra in grainscape's cost convention -- 4-connected
  ## (the engine spreads to rook neighbours only), cost = sum of the resistances of the
  ## matrix cells entered, with the path constrained to the two patches' Voronoi corridor
  ## (cells whose voronoi id is one of the two endpoints). The engine's lcpPerimWeight must
  ## match this independent value for every link.
  skip_if_not_installed("igraph")
  withr::local_package("igraph")

  tiny <- terra::rast(
    system.file("extdata", "tiny.asc", package = "grainscape", mustWork = TRUE)
  )
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  mpg <- MPG(cost = tinyCost, patch = (tinyCost == 1))
  edges <- graphdf(mpg)[[1]]$e

  ## fixed 4-connected adjacency over the grid (entering a cell costs its resistance)
  rr <- terra::values(tinyCost)[, 1]
  vor <- terra::values(mpg@voronoi)[, 1]
  pid <- terra::values(mpg@patchId)[, 1]
  n <- terra::ncell(tinyCost)
  adj <- terra::adjacent(tinyCost, cells = seq_len(n), directions = 4)
  from <- rep(seq_len(n), each = 4)
  to <- as.vector(t(adj))
  ok <- !is.na(to) & !is.na(rr[to]) & !is.na(rr[from])
  from <- from[ok]
  to <- to[ok]

  corridorLCP <- function(a, b) {
    incorr <- (vor == a | vor == b)
    incorr[is.na(incorr)] <- FALSE
    k <- incorr[from] & incorr[to]
    cn <- rr
    cn[!is.na(pid) & (pid == a | pid == b)] <- 0 # source/target patch cells are free
    aCells <- which(pid == a)
    bCells <- which(pid == b)
    src <- n + 1L
    g <- make_empty_graph(n = n + 1L, directed = TRUE)
    g <- add_edges(g, as.vector(rbind(from[k], to[k])), attr = list(weight = cn[to[k]]))
    g <- add_edges(g, as.vector(rbind(src, aCells)), attr = list(weight = rep(0, length(aCells))))
    suppressWarnings(min(distances(g, v = src, to = bCells, mode = "out", algorithm = "dijkstra")))
  }

  independent <- vapply(
    seq_len(nrow(edges)),
    function(i) corridorLCP(edges$e1[i], edges$e2[i]),
    numeric(1)
  )

  ## allow at most a one-unit discretization difference at a single corridor-boundary cell
  expect_true(all(abs(edges$lcpPerimWeight - independent) <= 1))
  expect_true(mean(abs(edges$lcpPerimWeight - independent)) < 0.05) # essentially all exact
})

test_that("MPG() does not hang when the resistance surface starts with NA (#72)", {
  ## A leading NA cell previously left the engine's costRes/maxCost as NaN (the running
  ## min/max was seeded from cost_vec[0] with NaN-unsafe comparisons), so every patch cell
  ## received resistance = NaN and never "settled" -- Engine::start() then looped forever.
  ## Reaching the expectations below at all confirms MPG() returns; a regression would hang
  ## here until the test process times out.
  r <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = 0,
    xmax = 12,
    ymin = 0,
    ymax = 12,
    crs = "EPSG:3857"
  )
  terra::values(r) <- 1
  r[1, 1] <- NA
  patches <- terra::setValues(r, 0)
  patches[5, 5] <- 1
  patches[9, 9] <- 1

  mpg <- MPG(r, patch = patches)
  expect_s4_class(mpg, "mpg")
  expect_gte(nrow(graphdf(mpg)[[1]]$e), 1L)
})
