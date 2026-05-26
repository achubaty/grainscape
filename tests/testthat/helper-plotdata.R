## Deterministic figure inputs for the vdiffr plot snapshot tests (test-plot.R).
## Mirrors the data setup in vignettes/grainscape_vignette.Rmd, but pins every source
## of randomness so the snapshots are reproducible. Built lazily and cached so this
## (somewhat expensive) work only runs when the plot tests actually execute, not for
## every other test file in the suite.

.plotDataCache <- new.env(parent = emptyenv())

plotTestData <- function() {
  if (!is.null(.plotDataCache$data)) {
    return(.plotDataCache$data)
  }

  suppressWarnings({
    ## --- patchy landscape (deterministic; bundled extdata) ---
    patchy <- terra::rast(system.file("extdata/patchy.asc", package = "grainscape"))
    patchyCost <- terra::classify(patchy, rcl = cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6)))
    patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))
    patchyGOC <- GOC(patchyMPG, nThresh = 10)

    ## --- 1D nodes on a random resistance surface (vignette figure 10) ---
    ## res2 is random in the vignette; pin it with a fixed seed for a stable snapshot.
    res <- terra::rast(xmin = 0, xmax = 100, ymin = 0, ymax = 100, resolution = 1)
    res[] <- 1
    pts <- withr::with_seed(674, {
      data.frame(
        x = rep(seq(10, 90, length.out = 5), 4),
        y = seq(10, 90, length.out = 4)
      ) +
        cbind(stats::runif(20) * 10, stats::runif(20) * 10)
    })
    patchPts <- terra::setValues(res, 0)
    patchPts[terra::cellFromXY(patchPts, pts)] <- 1
    res2 <- res
    res2[] <- withr::with_seed(674, floor(stats::runif(terra::ncell(res2)) * 10 + 1))
    mpgRes <- MPG(res2, patchPts)

    ## --- fragmented landscape (deterministic; bundled extdata) ---
    frag <- terra::rast(system.file("extdata/fragmented.asc", package = "grainscape"))
    fragRes <- terra::classify(frag, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
    fragMPG <- MPG(fragRes, patch = (frag == 1))

    fragLatticeMPG <- MPG(fragRes, patch = 25)
    fragLatticeGOC <- GOC(fragLatticeMPG, nThresh = 5)

    fragPatchGOC <- GOC(fragMPG, nThresh = 5)
    fragPatchGrain4 <- grain(fragPatchGOC, whichThresh = 4)

    fragCorridor3 <- corridor(fragPatchGOC, whichThresh = 3, coords = rbind(c(5, 180), c(395, 312)))
  })

  .plotDataCache$data <- list(
    patchyMPG = patchyMPG,
    patchyGOC = patchyGOC,
    mpgRes = mpgRes,
    fragRes = fragRes,
    fragMPG = fragMPG,
    fragLatticeGOC = fragLatticeGOC,
    fragPatchGOC = fragPatchGOC,
    fragPatchGrain4 = fragPatchGrain4,
    fragCorridor3 = fragCorridor3
  )
  .plotDataCache$data
}
