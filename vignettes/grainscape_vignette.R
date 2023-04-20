## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(igraph)
library(grainscape)
library(raster)
library(ggplot2)

opts_chunk$set(cache = TRUE)
opts_chunk$set(echo = TRUE)

opts_chunk$set(fig.height = 3)
opts_chunk$set(fig.width = 3)
opts_chunk$set(fig.show = "hold")

options(knitr.kable.NA = "")

## ----figure_03, fig.cap='\\label{fig:patchycost}Input raster resistance surface to create the minimum planar graph (MPG). Features with value of 1 (red) will be the patches in the network. A river (light blue) has the highest resistance in this example.'----
## Create an is-becomes matrix for reclassification
isBecomes <- cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6))
patchyCost <- reclassify(patchy, rcl = isBecomes)
patchyCost_df <- ggGS(patchyCost)
patchyCost_df$value <- as.factor(patchyCost_df$value)

## Plot this raster using ggplot2 functionality
## and the default grainscape theme
ggplot() +
  geom_raster(
    data = patchyCost_df,
    aes(x = x, y = y, fill = value)
  ) +
  scale_fill_brewer(type = "div", palette = "Paired", guide = "legend") +
  guides(fill = guide_legend(title = "Resistance")) +
  theme_grainscape() +
  theme(legend.position = "right")

## ----model_01_step_02---------------------------------------------------------
patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))

## ----figure_04, fig.cap='\\label{fig:mpgplot}A quick visualization of the minimum planar graph (MPG). Grey areas are patches (nodes) in the graph, and green lines are links showing the shortest paths between the perimeters of the patches on the resistance surface. In depth discussion of how the MPG is generated can be found elsewhere [@Fall:2007eo].'----
plot(patchyMPG, quick = "mpgPlot", theme = FALSE)

## ----table_01-----------------------------------------------------------------
## Extract tabular node information using the graphdf() function
nodeTable <- graphdf(patchyMPG)[[1]]$v

## Render table using the kable function,
## retaining the first three rows
kable(nodeTable[1:3, ], digits = 0, row.names = FALSE)

## ----table_02-----------------------------------------------------------------
## Extract tabular link information using the graphdf() function
linkTable <- graphdf(patchyMPG)[[1]]$e

## Render table using the kable function,
## retaining the first three rows
kable(linkTable[1:3, ], digits = 0, row.names = FALSE)

## ----table_03-----------------------------------------------------------------
scalarAnalysis <- threshold(patchyMPG, nThresh = 5)

## Use kable to render this as a table
kable(scalarAnalysis$summary,
  caption = paste(
    "The number of components ('nComponents') in the",
    "minimum planar graph at five automatically-selected",
    "link thresholds ('maxLink)."
  )
)

## ----figure_05, fig.cap='\\label{fig:scalesaggregation}A scalar analysis at 100 thresholds of the MPG in \\autoref{fig:mpgplot}. When the landscape is a single component at higher link thresholds all patches are completely connected. As an example, an organism able to disperse 250 resistance units would experience this landscape as six connected regions.'----
scalarAnalysis <- threshold(patchyMPG, nThresh = 100)
ggplot(scalarAnalysis$summary, aes(x = maxLink, y = nComponents)) +
  geom_line(colour = "forestgreen") +
  xlab("Link Threshold (resistance units)") +
  ylab("Number of components") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  scale_y_continuous(breaks = 1:20) +
  theme_light() +
  theme(axis.title = element_text())

## ----figure_06, fig.cap="\\label{fig:thresholdedgraph}The thresholded MPG depicted with a link length of 250 resistance units. An organism that can disperse a maximum of 250 resistance units would experience this landscape as 6 connected regions in the depicted spatial configuration. Note that the plotting has been customized to emphasize which patches are connected. This was done by plotting links with less than the threshold length from the centroids of patches", warning=FALSE----
ggplot() +
  geom_raster(
    data = ggGS(patchyMPG, "patchId"),
    aes(x = x, y = y, fill = value > 0)
  ) +
  scale_fill_manual(values = "grey") +
  geom_segment(
    data = ggGS(patchyMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = lcpPerimWeight >= 250
    )
  ) +
  scale_colour_manual(values = c("forestgreen", NA)) +
  geom_point(
    data = ggGS(patchyMPG, "nodes"), aes(x = x, y = y),
    colour = "darkgreen"
  )

## ----model_02_step_01---------------------------------------------------------
## Load the patchy raster distributed with grainscape
patchy <- raster(system.file("extdata/patchy.asc", package = "grainscape"))

## Create an is-becomes matrix for reclassification
isBecomes <- cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6))
patchyCost <- reclassify(patchy, rcl = isBecomes)

## Create the MPG model using cells = 1 as patches
patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))

## ----figure_07, fig.cap='\\label{fig:voronoitessellation}A Voronoi tessellation. This is the complement of the MPG. The patches (darkest blue) are used as generators, and regions of proximity (polygons of different colours) are found in cost or resistance units. The method was first described by @Fall:2007eo.'----
patchPlusVoronoi <- patchyMPG@voronoi
patchPlusVoronoi[patchyMPG@patchId] <- 0

ggplot() +
  geom_raster(data = ggGS(patchPlusVoronoi), aes(x = x, y = y, fill = value))

## ---- results="hide"----------------------------------------------------------
patchyGOC <- GOC(patchyMPG, nThresh = 10)

## ----figure_08, fig.cap='\\label{fig:gocthresh}A visualization of a GOC model. In this case it is the 6th scale or threshold extracted. Voronoi polygons imply regions that are functionally-connected at the given movement threshold.'----
plot(grain(patchyGOC, whichThresh = 6), quick = "grainPlot", theme = FALSE)

## ----figure_09, fig.cap="A planar network with one-dimensional point nodes extracted using `grainscape`. Nodes are represented as points and labelled with their `patchId`. Links are line segments among nodes.", warning=FALSE----
## Make a new resistance raster of 400 by 400 cells
## with a coordinate system that corresponds to cells
res <- raster(xmn = 0, xmx = 100, ymn = 0, ymx = 100, resolution = 1)

## Assign all values to 1
res[] <- 1

## Create 20 "random" points representing nodes,
## (i.e. the loci of a process of interest)
pts <- data.frame(
  x = rep(seq(10, 90, length.out = 5), 4),
  y = seq(10, 90, length.out = 4)
) +
  cbind(runif(20) * 10, runif(20) * 10)

## Represent these on a patch raster
## by duplicating the resistance raster and
## setting the relevant cells to 1
patchPts <- res
patchPts <- setValues(patchPts, 0)
patchPts[cellFromXY(patchPts, pts)] <- 1

## Extract the MPG
mpg <- MPG(res, patchPts)

## Plot the result using the quick 'network' visualization
## setting and add labels (dodging them by 3 to the upper-right)
figure09 <- plot(mpg, quick = "network", theme = FALSE) +
  geom_text(
    data = ggGS(mpg, "nodes"),
    aes(x = x + 3, y = y + 3, label = patchId)
  ) +
  ggtitle("Planar 1D; Euclidean surface")
figure09

## ----table_04-----------------------------------------------------------------
## Here we show the first five rows of the link attribute table
## extracted from the mpg object. This shows selected columns,
## using the formatting function kable()
neighbours <- graphdf(mpg)[[1]]$e[, c(1, 2, 4)]
neighbours <- neighbours[order(neighbours[, 1]), ][1:5, ]
names(neighbours) <- c("Node 1", "Node 2", "Path distance (Euclidean)")
kable(neighbours, row.names = FALSE)

## ----figure_10, fig.cap="A planar network with one-dimensional nodes extracted on a resistance surface. Nodes are represented by grey-coloured cells and labelled with their `patchId`. Links are green paths between nodes, shown in a spatially-explicit representation."----
## Add some cost values to the resistance
## surface we used in the last step
## Here we use random integers >= 2
res2 <- res
res2[] <- floor(runif(ncell(res2)) * 10 + 1)

## Extract the minimum planar graph using the
## raster made previously which represents the points only
mpg <- MPG(res2, patchPts)

## Plot the result using the quick 'mgplot' visualization
## setting and add labels (dodging them by 3 to the upper-right)
## This demonstrates the non-linear paths.
figure10 <- plot(mpg, quick = "mpgPlot", theme = FALSE) +
  geom_text(
    data = ggGS(mpg, "nodes"),
    aes(x = x + 3, y = y + 3, label = patchId)
  ) +
  ggtitle("Planar 1D; Resistance surface")
figure10

## ----table_05-----------------------------------------------------------------
## Here we show the first five rows of the link attribute table
## extracted from the mpg object. This shows selected columns, using
## the formatting function kable()
resNeighbours <- graphdf(mpg)[[1]]$e[, c(1, 2, 4)]
resNeighbours <- resNeighbours[order(resNeighbours[, 1]), ][1:5, ]
comparison <- cbind(neighbours, resNeighbours)[, -c(4, 5)]
names(comparison)[4] <- c("Path distance (Resistance)")
kable(comparison, row.names = FALSE)

## ----figure_11, fig.cap="A minimum planar graph (MPG) of a simulated resistance surface. Focal patches are grey regions and green lines indicate spatially-explicit links among patches. Several patches are labelled with their numerical `patchId`."----
## Load a land cover raster distributed with grainscape
frag <- raster(system.file("extdata/fragmented.asc", package = "grainscape"))

## Convert land cover to resistance units
## Use an "is-becomes" reclassification
isBecomes <- cbind(c(1, 2, 3, 4), c(1, 5, 10, 12))

fragRes <- reclassify(frag, rcl = isBecomes)

## Extract a network using cells = 1 on original raster
## as the focal patches or nodes
patches <- (frag == 1)
fragMPG <- MPG(fragRes, patch = patches)

## Plot the minimum planar graph with node labels for several
## focal nodes of interest
figure11 <- plot(fragMPG, quick = "mpgPlot", theme = FALSE) +
  geom_text(
    data = ggGS(fragMPG, "nodes"),
    aes(
      x = x, y = y,
      label = ifelse(patchId %in% c(7, 23, 52, 106, 158, 221),
        patchId, ""
      )
    ),
    size = 2
  ) +
  ggtitle("Planar 2D; Resistance surface")
figure11

## ----table_06-----------------------------------------------------------------
## Here we show only patch 7 and its neighbours that are labelled on
## the network using the formatting function kable().
fragNeighbours <- graphdf(fragMPG)[[1]]$e[, c(1, 2, 4)]
fragNeighbours <- fragNeighbours[order(fragNeighbours[, 1]), ][1:5, ]
names(fragNeighbours) <- c("Node 1", "Node 2", "Path distance (Resistance)")
kable(fragNeighbours, row.names = FALSE)

## ----figure_12, fig.cap="A minimum planar graph (MPG) shown with centroid node representation and linear link representation among these nodes.", results="hide"----
## Plot the minimum planar graph using centroid nodes
## A single line of code will do this as follows:
## plot(fragMPG, quick = "network")

## However the following approach gives more control,
## allowing reduction of the size of the nodes to avoid crowding
figure12 <- ggplot() +
  geom_segment(
    data = ggGS(fragMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = "forestgreen", size = 0.25
    )
  ) +
  geom_point(
    data = ggGS(fragMPG, "nodes"),
    aes(x = x, y = y, colour = "darkgreen", size = 0.5)
  ) +
  scale_colour_identity() +
  scale_size_identity() +
  ggtitle("Centroid representation of nodes")
figure12

## ----figure_13, fig.cap="A minimum planar graph (MPG) with links represented from the perimeter of the patches (*i.e.*, at the start and end points of spatially-explicit links). This rendering can simplify visualization."----
## Plot the minimum planar graph using perimeter links
## and two dimensional nodes
figure13 <- plot(fragMPG, quick = "mpgPerimPlot", theme = FALSE) +
  ggtitle("Perimeter representation of links")
figure13

## ----figure_14, fig.cap="A minimum planar graph (MPG) with the shortest links among patch perimeters rendered as spatially-explicit paths. This rendering can help demonstrate the modelling procedure, but risks overinterpretation of the actual paths involved (see text)."----
## Plot the minimum planar graph using spatially-explicit links
## and two dimensional nodes.
figure14 <- plot(fragMPG, quick = "mpgPlot", theme = FALSE) +
  ggtitle("Spatially-explicit representation of links")
figure14

## ----figure_15, fig.cap="A minimum planar graph (MPG) with centroid node representation and links between centroids, where the plotted size of the node symbol has been scaled with the area of the patch it represents."----
figure15 <- ggplot() +
  geom_segment(
    data = ggGS(fragMPG, "links"),
    aes(x = x1, y = y1, xend = x2, yend = y2),
    colour = "forestgreen"
  ) +
  geom_point(
    data = ggGS(fragMPG, "nodes"),
    aes(x = x, y = y, size = patchArea), colour = "darkgreen"
  ) +
  scale_size_area(max_size = 10, breaks = c(1000, 3000)) +
  ggtitle("Characteristics of nodes (weights)")
figure15

## ----figure_16, fig.cap="A minimum planar graph with centroid node and link representation where the width of the links has been scaled in proportion to the reduction in connectivity due to resistance. Wider links correspond to reduced connectivity."----
figure16 <- ggplot() +
  geom_segment(
    data = ggGS(fragMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      size = lcpPerimWeight / (sqrt((x2 - x1)^2 + (y2 - y1)^2))
    ),
    colour = "forestgreen", alpha = 0.5
  ) +
  scale_size(range = c(0, 3), breaks = seq(1, 6, by = 0.5)) +
  geom_point(
    data = ggGS(fragMPG, "nodes"),
    aes(x = x, y = y), size = 3, colour = "darkgreen"
  ) +
  ggtitle("Characteristics of links (weights)")
figure16

## ----figure_17, fig.cap="A link threshold representation of a minimum planar graph (MPG), where links connect the centroids of adjacent patches in the same component (cluster). Nodes that form their own component are shown as circles without links."----
figure17 <- ggplot() +
  geom_raster(
    data = ggGS(fragMPG, "patchId"),
    aes(x = x, y = y, fill = value > 0)
  ) +
  scale_fill_manual(values = "grey") +
  geom_segment(
    data = ggGS(fragMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = lcpPerimWeight > 20
    )
  ) +
  scale_colour_manual(values = c("forestgreen", NA)) +
  geom_point(
    data = ggGS(fragMPG, "nodes"),
    aes(x = x, y = y), colour = "darkgreen"
  ) +
  ggtitle("Link thresholding by plotting")
figure17

## ----figure_18, fig.cap="A link threshold representation of a minimum planar graph (MPG), where links connect the centroids of adjacent patches in the same component (cluster). Nodes that are part of a component are labelled with their component membership."----
## Use the grainscape::threshold() function to create a new network
## by thresholding links
fragTh <- threshold(fragMPG, doThresh = 20)

## Find the components in that thresholded network using
## an igraph package function
fragThC <- components(fragTh$th[[1]])

## Extract the node table and append the
## component membership information
fragThNodes <- data.frame(vertex_attr(fragTh$th[[1]]),
  component = fragThC$membership
)

## We don't want to show nodes that are in components with
## only one node, so remove them
singleNodes <- fragThNodes$component %in% which(fragThC$csize == 1)
fragThNodes <- fragThNodes[!(singleNodes), ]

## Rename some columns to improve readability
fragThNodes$x <- fragThNodes$centroidX
fragThNodes$y <- fragThNodes$centroidY

figure18 <- ggplot() +
  geom_raster(
    data = ggGS(fragMPG, "patchId"),
    aes(x = x, y = y, fill = value > 0)
  ) +
  scale_fill_manual(values = "grey") +
  geom_segment(
    data = ggGS(fragMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = lcpPerimWeight > 20
    )
  ) +
  scale_colour_manual(values = c("forestgreen", NA)) +
  geom_point(
    data = fragThNodes,
    aes(x = x, y = y), shape = 19, size = 4, colour = "darkgreen"
  ) +
  geom_text(
    data = fragThNodes, aes(x = x, y = y, label = component),
    colour = "white", size = 2
  ) +
  ggtitle("Link thresholding to show components")
figure18

## ----figure_19, fig.cap="A link threshold representation of a minimum planar graph (MPG), where nodes, plotted at patch centroids, are scaled in proportion to the degree of the node (*i.e.*, the number of links adjacent on the node). Larger circles indicate nodes with a higher degree in the MPG."----
## Assess degree on the nodes of a thresholded network
## made in the previous example (threshold = 20)
fragThDegree <- degree(fragTh$th[[1]])

## Add degree to the node table
fragThNodes <- data.frame(vertex_attr(fragTh$th[[1]]), degree = fragThDegree)

## Remove nodes with a degree of 0
fragThNodes <- fragThNodes[fragThNodes$degree > 0, ]

## Rename some columns to improve readability
fragThNodes$x <- fragThNodes$centroidX
fragThNodes$y <- fragThNodes$centroidY

figure19 <- ggplot() +
  geom_raster(
    data = ggGS(fragMPG, "patchId"),
    aes(x = x, y = y, fill = value > 0)
  ) +
  scale_fill_manual(values = "grey") +
  geom_segment(
    data = ggGS(fragMPG, "links"),
    aes(
      x = x1, y = y1, xend = x2, yend = y2,
      colour = lcpPerimWeight > 20
    )
  ) +
  scale_colour_manual(values = c("forestgreen", NA)) +
  geom_point(
    data = fragThNodes,
    aes(x = x, y = y, size = degree), colour = "darkgreen"
  ) +
  ggtitle("Node importance metrics (degree)")
figure19

## ----figure_20, fig.cap="The shortest path through a minimum planar graph (MPG) given a start and end `patchId`. Links and nodes that form part of this \"corridor\" are highlighted."----
## Declare the start and end patchIds
## These were identified by plotting the patchIds (see earlier examples)
startEnd <- c(1546, 94)

## Find the shortest path between these nodes using
## the shortest path through the resistance surface
## (i.e. weighted by 'lcpPerimWeight')
shPath <- shortest_paths(fragMPG$mpg,
  from = which(V(fragMPG$mpg)$patchId == startEnd[1]),
  to = which(V(fragMPG$mpg)$patchId == startEnd[2]),
  weights = E(fragMPG$mpg)$lcpPerimWeight,
  output = "both"
)

## Extract the nodes and links of this shortest path
shPathN <- as.integer(names(shPath$vpath[[1]]))
shPathL <- E(fragMPG$mpg)[shPath$epath[[1]]]$linkId

## Produce shortest path tables for plotting
shPathNodes <- subset(ggGS(fragMPG, "nodes"), patchId %in% shPathN)
shPathLinks <- subset(ggGS(fragMPG, "links"), linkId %in% shPathL)

## Find the distance of the shortest path
shPathD <- distances(fragMPG$mpg,
  v = which(V(fragMPG$mpg)$patchId == startEnd[1]),
  to = which(V(fragMPG$mpg)$patchId == startEnd[2]),
  weights = E(fragMPG$mpg)$lcpPerimWeight
)[1]

## Plot shortest path
figure20 <- ggplot() +
  geom_raster(
    data = ggGS(fragMPG, "patchId"),
    aes(
      x = x, y = y,
      fill = ifelse(value %in% shPathN, "grey70", "grey90")
    )
  ) +
  scale_fill_identity() +
  geom_segment(
    data = shPathLinks, aes(x = x1, y = y1, xend = x2, yend = y2),
    colour = "forestgreen", size = 1
  ) +
  geom_point(data = shPathNodes, aes(x = x, y = y), colour = "darkgreen") +
  ggtitle("Shortest-path distance between nodes") +
  annotate("text", 260, 340,
    label = paste0(shPathD, " resistance units"), size = 2.5
  )
figure20

## ----table_07-----------------------------------------------------------------
## Create a pairwise table of shortest path distances among nodes using
## the resistance surface based links
allShPathD <- distances(fragMPG$mpg, weights = E(fragMPG$mpg)$lcpPerimWeight)

## Create a table for the first 8 nodes
tableD <- allShPathD[1:8, 1:8]
tableD[upper.tri(tableD)] <- NA
dimnames(tableD)[[1]] <- paste("patchId", dimnames(tableD)[[1]], sep = " ")

kable(tableD)

## ----figure_21, fig.cap="A scaled lattice grains of connectivity (GOC) model showing the outline of Voronoi polygons in grey, and the network connections among polygons as links plotted from adjacent polygon centroids.", results="hide"----
## Extract a minimum planar graph and complementary
## Voronoi polygons from the fragmented resistance surface
## Note the use of an integer for the 'patch' parameter, which
## specifies the spacing in cells of the lattice grid
fragLatticeMPG <- MPG(fragRes, patch = 25)

## Extract grains of connectivity from this MPG at
## five link thresholds (or scales)
fragLatticeGOC <- GOC(fragLatticeMPG, nThresh = 5)

## Visualize the Voronoi polygons at the third threshold
figure21 <- plot(grain(fragLatticeGOC, whichThresh = 3),
  quick = "grainPlot", theme = FALSE
) +
  ggtitle("Lattice grains of connectivity")
figure21

## ----figure_22, fig.cap="A scaled patch grains of connectivity (GOC) model showing the outline of Voronoi polygons in grey, and the network connections among polygons as links plotted from adjacent polygon centroids.", results= "hide"----
## Use the MPG extracted in previous examples to find a
## patch grains of connectivity model, where patches
## are cells on the resistance surface equal to 1
## Do this at five thresholds
fragPatchGOC <- GOC(fragMPG, nThresh = 5)

## Plot the fourth grain
figure22 <- plot(grain(fragPatchGOC, whichThresh = 4),
  quick = "grainPlot", theme = FALSE
) +
  ggtitle("Patch grains of connectivity")
figure22

## ----figure_23, fig.cap="A scaled patch grains of connectivity (GOC) model showing the outline of Voronoi polygons in grey, and the network connections among polygons. Node symbols have been scaled in proportion to the core area of patches (*i.e.*, area excluding edge) contained within the Voronoi polygon.", results = "hide"----
## Put the fourth grain of the GOC model into its own object
fragPatchGrain4 <- grain(fragPatchGOC, whichThresh = 4)

figure23 <- ggplot() +
  geom_raster(
    data = ggGS(fragPatchGrain4, "vorBound"),
    aes(x = x, y = y, fill = ifelse(value > 0, "grey", "white"))
  ) +
  scale_fill_identity() +
  geom_segment(
    data = ggGS(fragPatchGrain4, "links"),
    aes(x = x1, y = y1, xend = x2, yend = y2), colour = "forestgreen"
  ) +
  geom_point(
    data = ggGS(fragPatchGrain4, "nodes"),
    aes(x = x, y = y, size = totalCoreArea), colour = "darkgreen"
  ) +
  ggtitle("Voronoi polygon metrics (core area)")
figure23

## ----figure_24, fig.cap="A corridor through a scaled patch grains of connectivity (GOC) model. The black nodes and links demonstrate the corridor between the polygons containing the start and end points (plotted in red as `X`). Green nodes and links show the remainder of the grains of connectivity network."----
## Set coordinates for the start and end of the corridor
startEnd <- rbind(c(5, 180), c(395, 312))

fragCorridor3 <- corridor(fragPatchGOC, whichThresh = 3, coords = startEnd)

## Use the default plotting functionality for corridor objects
figure24 <- plot(fragCorridor3, theme = FALSE) +
  annotate("text",
    x = startEnd[1, 1], y = startEnd[1, 2] - 20,
    label = "START", colour = "red", size = 2
  ) +
  annotate("text",
    x = startEnd[1, 1], y = startEnd[1, 2],
    label = "X", colour = "red", size = 2
  ) +
  annotate("text",
    x = startEnd[2, 1], y = startEnd[2, 2] + 20,
    label = "END", colour = "red", size = 2
  ) +
  annotate("text",
    x = startEnd[2, 1], y = startEnd[2, 2],
    label = "X", colour = "red", size = 2
  ) +
  annotate("text",
    x = 250, y = 400,
    label = paste0(
      "Corridor length: ",
      round(fragCorridor3@corridorLength, 0),
      " resistance units"
    ), size = 2
  ) +
  ggtitle("Corridor analysis; grain of connectivity")
figure24

## ----figure_25, fig.cap="A scaled patch grains of connectivity (GOC) model showing the outline of Voronoi polygons in grey, and the network connections among polygons as links plotted from adjacent polygon centroids. The locations of eight focal points used to calculate a pairwise distance matrix are plotted in red."----
## Create eight random points on the map
pts <- cbind(sample(seq_len(ncol(fragRes)))[1:8], sample(seq_len(nrow(fragRes)))[1:8])

## Plot these points and the grains of connectivity network
figure25 <- plot(grain(fragPatchGOC, 4), quick = "grainPlot", theme = FALSE) +
  annotate("text", x = pts[, 1], y = pts[, 2], label = 1:8, colour = "red") +
  ggtitle("Eight points for pairwise distances")
figure25

## ----table_08-----------------------------------------------------------------
## Find the pairwise distances between them at all grains
## available in the GOC object created earlier
ptsD <- grainscape::distance(fragPatchGOC, pts)

## Extract distances for the grain of interest (4)
ptsD2 <- ptsD$th[[4]]$grainD

## Prepare this distance matrix for printing
ptsD2[upper.tri(ptsD2)] <- NA
ptsD2 <- round(ptsD2, 1)
dimnames(ptsD2)[[1]] <- paste0(
  "Point ", 1:8, " (Polygon ",
  dimnames(ptsD2)[[2]], ")"
)
dimnames(ptsD2)[[2]] <- 1:8

kable(ptsD2)

## ----part_A, dependson="figure_20", include=FALSE-----------------------------
## build 1st visual table of contents using vignette figures
partA <- cowplot::plot_grid(
  figure09 + theme(plot.title = element_text(size = 8)),
  figure10 + theme(plot.title = element_text(size = 8)),
  figure11 + theme(plot.title = element_text(size = 8)),
  figure12 + theme(plot.title = element_text(size = 8)),
  figure13 + theme(plot.title = element_text(size = 8)),
  figure14 + theme(plot.title = element_text(size = 8)),
  figure15 + theme(plot.title = element_text(size = 8)),
  figure16 + theme(plot.title = element_text(size = 8)),
  figure17 + theme(plot.title = element_text(size = 8)),
  figure18 + theme(plot.title = element_text(size = 8)),
  figure19 + theme(plot.title = element_text(size = 8)),
  figure20 + theme(plot.title = element_text(size = 8)),
  nrow = 4, ncol = 3, labels = paste0("(", 9:20, ")"), label_size = 8,
  vjust = 1.75, hjust = -0.05
) +
  theme(panel.background = element_blank())

## currently writing to tempdir for CRAN checks;
## be sure to save to vignettes/figures directory when changing figures.
ggsave(file.path(tempdir(), "figure_partA.png"),
  plot = partA,
  width = 8, height = 8, dpi = 600
)

## ----part_B, dependson="figure_25", include=FALSE-----------------------------
## build 2nd visual table of contents using vignette figures
partB <- cowplot::plot_grid(
  figure21 + theme(plot.title = element_text(size = 8)),
  figure22 + theme(plot.title = element_text(size = 8)),
  figure23 + theme(plot.title = element_text(size = 8)),
  figure24 + theme(plot.title = element_text(size = 8)),
  figure25 + theme(plot.title = element_text(size = 8)),
  nrow = 4, ncol = 3, labels = paste0("(", 21:25, ")"), label_size = 8,
  vjust = 1.75, hjust = -0.05
) +
  theme(panel.background = element_blank())

## currently writing to tempdir for CRAN checks;
## be sure to save to vignettes/figures directory when changing figures.
ggsave(file.path(tempdir(), "figure_partB.png"),
  plot = partB,
  width = 8, height = 8, dpi = 600
)

