## Visual-regression snapshots (vdiffr) of the link-affected vignette figures.
##
## Each test snapshots the NETWORK/links plot only -- not the resistance companion
## panel (`plotWithResistance()` / `plotResistance()`), which is unaffected by the
## least-cost-path computation. Grain plots assign random Voronoi polygon colours
## (see R/plot.R) and figure 10 uses a random resistance surface, so those are seeded
## for reproducibility. Deterministic landscapes come from bundled `extdata`.
##
## vdiffr auto-skips on CRAN and when the svglite/ggplot2 versions differ from the
## recorded baseline, so these act as a local/maintainer regression tool.

test_that("patchy MPG / GOC figures", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  d <- plotTestData()

  ## figure 04: quick MPG plot
  vdiffr::expect_doppelganger(
    "mpg-quick-mpgplot",
    plot(d$patchyMPG, quick = "mpgPlot", print = FALSE, theme = FALSE)
  )

  ## figure 06: thresholded links (<= 250) drawn from patch centroids
  links_df <- dplyr::filter(ggGS(d$patchyMPG, "links"), lcpPerimWeight <= 250)
  figure06 <- ggplot() +
    geom_raster(
      data = ggGS(d$patchyMPG, "patchId"),
      mapping = aes(x = x, y = y, fill = value > 0)
    ) +
    scale_fill_manual(values = "grey") +
    geom_segment(
      data = links_df,
      mapping = aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2,
        colour = as.factor(lcpPerimWeight)
      )
    ) +
    scale_colour_manual(values = rep("forestgreen", nrow(links_df))) +
    geom_point(
      data = ggGS(d$patchyMPG, "nodes"),
      mapping = aes(x = x, y = y),
      colour = "darkgreen"
    )
  vdiffr::expect_doppelganger("mpg-thresholded-links-250", figure06)

  ## figure 07: Voronoi tessellation (deepcopy before mutating the cached raster)
  patchPlusVoronoi <- terra::deepcopy(d$patchyMPG@voronoi)
  patchPlusVoronoi[d$patchyMPG@patchId > 0] <- 0
  figure07 <- ggplot() +
    geom_raster(data = ggGS(patchPlusVoronoi), aes(x = x, y = y, fill = value)) +
    coord_equal()
  vdiffr::expect_doppelganger("mpg-voronoi", figure07)

  ## figure 08: GOC grain plot (random polygon colours -> seed)
  vdiffr::expect_doppelganger(
    "goc-grainplot-thresh6",
    withr::with_seed(
      42,
      plot(grain(d$patchyGOC, whichThresh = 6), quick = "grainPlot", print = FALSE, theme = FALSE)
    )
  )
})

test_that("scalar analysis line plot (figure 05)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  d <- plotTestData()

  scalarAnalysis <- threshold(d$patchyMPG, nThresh = 100)
  figure05 <- ggplot(scalarAnalysis$summary, aes(x = maxLink, y = nComponents)) +
    geom_line(colour = "forestgreen") +
    xlab("Link Threshold (resistance units)") +
    ylab("Number of components") +
    scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
    scale_y_continuous(breaks = 1:20) +
    theme_light() +
    theme(axis.title = element_text())
  vdiffr::expect_doppelganger("scalar-analysis-ncomponents", figure05)
})

test_that("1D nodes on resistance surface (figure 10)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  d <- plotTestData()

  figure10 <- plot(d$mpgRes, quick = "mpgPlot", print = FALSE, theme = FALSE) +
    geom_text(
      data = ggGS(d$mpgRes, "nodes"),
      aes(x = x + 3, y = y + 3, label = patchId)
    ) +
    ggtitle("Planar 1D; Resistance surface")
  vdiffr::expect_doppelganger("mpg-1d-resistance-mpgplot", figure10)
})

test_that("frag MPG link figures", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  withr::local_package("igraph")
  d <- plotTestData()

  ## figure 11: mpgPlot with selected node labels
  links7 <- graphdf(d$fragMPG)[[1]]$e
  labelNodes <- c(7, links7$e2[links7$e1 == 7], links7$e1[links7$e2 == 7])
  figure11 <- plot(d$fragMPG, quick = "mpgPlot", print = FALSE, theme = FALSE) +
    geom_text(
      data = ggGS(d$fragMPG, "nodes"),
      aes(x = x, y = y, label = ifelse(patchId %in% labelNodes, patchId, "")),
      size = 2
    ) +
    ggtitle("Planar 2D; Resistance surface")
  vdiffr::expect_doppelganger("mpg-frag-mpgplot-labeled", figure11)

  ## figure 12: centroid representation
  figure12 <- ggplot() +
    geom_segment(
      data = ggGS(d$fragMPG, "links"),
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "forestgreen", linewidth = 0.25)
    ) +
    geom_point(
      data = ggGS(d$fragMPG, "nodes"),
      aes(x = x, y = y, colour = "darkgreen", size = 0.5)
    ) +
    scale_colour_identity() +
    scale_size_identity() +
    scale_linewidth_identity() +
    ggtitle("Centroid representation of nodes")
  vdiffr::expect_doppelganger("mpg-frag-centroid-links", figure12)

  ## figure 13: perimeter representation of links
  vdiffr::expect_doppelganger(
    "mpg-frag-perimplot",
    plot(d$fragMPG, quick = "mpgPerimPlot", print = FALSE, theme = FALSE) +
      ggtitle("Perimeter representation of links")
  )

  ## figure 14: spatially-explicit representation of links
  vdiffr::expect_doppelganger(
    "mpg-frag-mpgplot",
    plot(d$fragMPG, quick = "mpgPlot", print = FALSE, theme = FALSE) +
      ggtitle("Spatially-explicit representation of links")
  )

  ## figure 15: node symbol scaled by patch area
  figure15 <- ggplot() +
    geom_segment(
      data = ggGS(d$fragMPG, "links"),
      aes(x = x1, y = y1, xend = x2, yend = y2),
      colour = "forestgreen"
    ) +
    geom_point(
      data = ggGS(d$fragMPG, "nodes"),
      aes(x = x, y = y, size = patchArea),
      colour = "darkgreen"
    ) +
    scale_size_area(max_size = 10, breaks = c(1000, 3000)) +
    ggtitle("Characteristics of nodes (weights)")
  vdiffr::expect_doppelganger("mpg-frag-node-area", figure15)

  ## figure 16: link width scaled by resistance/distance ratio
  figure16 <- ggplot() +
    geom_segment(
      data = ggGS(d$fragMPG, "links"),
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2,
        linewidth = lcpPerimWeight / (sqrt((x2 - x1)^2 + (y2 - y1)^2))
      ),
      colour = "forestgreen",
      alpha = 0.5
    ) +
    scale_linewidth(range = c(0, 3), breaks = seq(1, 6, by = 0.5)) +
    geom_point(
      data = ggGS(d$fragMPG, "nodes"),
      aes(x = x, y = y),
      size = 3,
      colour = "darkgreen"
    ) +
    ggtitle("Characteristics of links (weights)")
  vdiffr::expect_doppelganger("mpg-frag-link-weights", figure16)

  ## figure 17: link thresholding by transparency
  figure17 <- ggplot() +
    geom_raster(
      data = ggGS(d$fragMPG, "patchId"),
      aes(x = x, y = y, fill = value > 0)
    ) +
    scale_fill_manual(values = "grey") +
    geom_segment(
      data = ggGS(d$fragMPG, "links"),
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = lcpPerimWeight > 20)
    ) +
    scale_colour_manual(values = c("forestgreen", NA)) +
    geom_point(
      data = ggGS(d$fragMPG, "nodes"),
      aes(x = x, y = y),
      colour = "darkgreen"
    ) +
    ggtitle("Link thresholding by plotting")
  vdiffr::expect_doppelganger("mpg-frag-link-threshold", figure17)
})

test_that("frag MPG thresholded-network figures", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  withr::local_package("igraph")
  d <- plotTestData()

  fragTh <- threshold(d$fragMPG, doThresh = 20)

  ## figure 18: component membership after thresholding
  fragThC <- components(fragTh$th[[1]])
  fragThNodes <- data.frame(vertex_attr(fragTh$th[[1]]), component = fragThC$membership)
  singleNodes <- fragThNodes$component %in% which(fragThC$csize == 1)
  fragThNodes <- fragThNodes[!(singleNodes), ]
  fragThNodes$x <- fragThNodes$centroidX
  fragThNodes$y <- fragThNodes$centroidY
  figure18 <- ggplot() +
    geom_raster(
      data = ggGS(d$fragMPG, "patchId"),
      aes(x = x, y = y, fill = value > 0)
    ) +
    scale_fill_manual(values = "grey") +
    geom_segment(
      data = ggGS(d$fragMPG, "links"),
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = lcpPerimWeight > 20)
    ) +
    scale_colour_manual(values = c("forestgreen", NA)) +
    geom_point(
      data = fragThNodes,
      aes(x = x, y = y),
      shape = 19,
      size = 4,
      colour = "darkgreen"
    ) +
    geom_text(
      data = fragThNodes,
      aes(x = x, y = y, label = component),
      colour = "white",
      size = 2
    ) +
    ggtitle("Link thresholding to show components")
  vdiffr::expect_doppelganger("mpg-frag-components", figure18)

  ## figure 19: node symbol scaled by degree
  fragThDegree <- degree(fragTh$th[[1]])
  fragThNodesD <- data.frame(vertex_attr(fragTh$th[[1]]), degree = fragThDegree)
  fragThNodesD <- fragThNodesD[fragThNodesD$degree > 0, ]
  fragThNodesD$x <- fragThNodesD$centroidX
  fragThNodesD$y <- fragThNodesD$centroidY
  figure19 <- ggplot() +
    geom_raster(
      data = ggGS(d$fragMPG, "patchId"),
      aes(x = x, y = y, fill = value > 0)
    ) +
    scale_fill_manual(values = "grey") +
    geom_segment(
      data = ggGS(d$fragMPG, "links"),
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = lcpPerimWeight > 20)
    ) +
    scale_colour_manual(values = c("forestgreen", NA)) +
    geom_point(
      data = fragThNodesD,
      aes(x = x, y = y, size = degree),
      colour = "darkgreen"
    ) +
    ggtitle("Node importance metrics (degree)")
  vdiffr::expect_doppelganger("mpg-frag-degree", figure19)
})

test_that("frag MPG shortest path (figure 20)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  withr::local_package("igraph")
  d <- plotTestData()

  startEnd <- c(1546, 94)
  shPath <- shortest_paths(
    d$fragMPG$mpg,
    from = which(V(d$fragMPG$mpg)$patchId == startEnd[1]),
    to = which(V(d$fragMPG$mpg)$patchId == startEnd[2]),
    weights = E(d$fragMPG$mpg)$lcpPerimWeight,
    output = "both"
  )
  shPathN <- as.integer(names(shPath$vpath[[1]]))
  shPathL <- E(d$fragMPG$mpg)[shPath$epath[[1]]]$linkId
  shPathNodes <- subset(ggGS(d$fragMPG, "nodes"), patchId %in% shPathN)
  shPathLinks <- subset(ggGS(d$fragMPG, "links"), linkId %in% shPathL)
  shPathD <- distances(
    d$fragMPG$mpg,
    v = which(V(d$fragMPG$mpg)$patchId == startEnd[1]),
    to = which(V(d$fragMPG$mpg)$patchId == startEnd[2]),
    weights = E(d$fragMPG$mpg)$lcpPerimWeight
  )[1]
  figure20 <- ggplot() +
    geom_raster(
      data = ggGS(d$fragMPG, "patchId"),
      aes(x = x, y = y, fill = ifelse(value %in% shPathN, "grey70", "grey90"))
    ) +
    scale_fill_identity() +
    geom_segment(
      data = shPathLinks,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      colour = "forestgreen",
      linewidth = 1
    ) +
    geom_point(data = shPathNodes, aes(x = x, y = y), colour = "darkgreen") +
    ggtitle("Shortest-path distance between nodes") +
    annotate("text", 260, 340, label = paste0(shPathD, " resistance units"), size = 2.5)
  vdiffr::expect_doppelganger("mpg-frag-shortest-path", figure20)
})

test_that("GOC grain and corridor figures", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  withr::local_package("ggplot2")
  d <- plotTestData()

  ## figure 21: lattice GOC grain plot (seed for random colours)
  vdiffr::expect_doppelganger(
    "goc-lattice-grainplot",
    withr::with_seed(
      42,
      plot(
        grain(d$fragLatticeGOC, whichThresh = 3),
        quick = "grainPlot",
        print = FALSE,
        theme = FALSE
      )
    ) +
      ggtitle("Lattice grains of connectivity")
  )

  ## figure 22: patch GOC grain plot
  vdiffr::expect_doppelganger(
    "goc-patch-grainplot",
    withr::with_seed(
      42,
      plot(
        grain(d$fragPatchGOC, whichThresh = 4),
        quick = "grainPlot",
        print = FALSE,
        theme = FALSE
      )
    ) +
      ggtitle("Patch grains of connectivity")
  )

  ## figure 23: Voronoi polygon core-area metrics
  figure23 <- ggplot() +
    geom_raster(
      data = ggGS(d$fragPatchGrain4, "vorBound"),
      aes(x = x, y = y, fill = ifelse(value > 0, "grey", "white"))
    ) +
    scale_fill_identity() +
    geom_segment(
      data = ggGS(d$fragPatchGrain4, "links"),
      aes(x = x1, y = y1, xend = x2, yend = y2),
      colour = "forestgreen"
    ) +
    geom_point(
      data = ggGS(d$fragPatchGrain4, "nodes"),
      aes(x = x, y = y, size = totalCoreArea),
      colour = "darkgreen"
    ) +
    ggtitle("Voronoi polygon metrics (core area)")
  vdiffr::expect_doppelganger("goc-patch-coremetrics", figure23)

  ## figure 25: eight focal points over the grain (seed points + grain colours)
  pts <- withr::with_seed(
    355,
    cbind(
      sample(seq_len(terra::ncol(d$fragRes)))[1:8],
      sample(seq_len(terra::nrow(d$fragRes)))[1:8]
    )
  )
  figure25 <- withr::with_seed(
    42,
    plot(grain(d$fragPatchGOC, 4), quick = "grainPlot", print = FALSE, theme = FALSE)
  ) +
    annotate("text", x = pts[, 1], y = pts[, 2], label = 1:8, colour = "red") +
    ggtitle("Eight points for pairwise distances")
  vdiffr::expect_doppelganger("goc-patch-points", figure25)

  ## figure 24: corridor through the grain
  startEnd <- rbind(c(5, 180), c(395, 312))
  figure24 <- plot(d$fragCorridor3, print = FALSE, theme = FALSE) +
    annotate(
      "text",
      x = startEnd[1, 1],
      y = startEnd[1, 2] - 20,
      label = "START",
      colour = "red",
      size = 2
    ) +
    annotate(
      "text",
      x = startEnd[1, 1],
      y = startEnd[1, 2],
      label = "X",
      colour = "red",
      size = 2
    ) +
    annotate(
      "text",
      x = startEnd[2, 1],
      y = startEnd[2, 2] + 20,
      label = "END",
      colour = "red",
      size = 2
    ) +
    annotate(
      "text",
      x = startEnd[2, 1],
      y = startEnd[2, 2],
      label = "X",
      colour = "red",
      size = 2
    ) +
    annotate(
      "text",
      x = 250,
      y = 400,
      label = paste0(
        "Corridor length: ",
        round(d$fragCorridor3@corridorLength, 0),
        " resistance units"
      ),
      size = 2
    ) +
    ggtitle("Corridor analysis; grain of connectivity")
  vdiffr::expect_doppelganger("corridor-frag-thresh3", figure24)
})
