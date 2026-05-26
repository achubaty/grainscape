#' Visualize corridors between two points using a grains of connectivity (GOC)
#'
#' @description
#' Given a series of GOC models built at different scales, visualize the corridor
#' (or shortest path) between two points using one of the tessellations
#' (i.e., scales) in these models.
#'
#' @param x       A [goc-class] object created by [GOC()].
#'
#' @param whichThresh  Integer giving the index of the threshold to visualize.
#'
#' @param coords  A two-column matrix or an `sf` (POINT) object
#'                giving coordinates at the end points of the corridor.
#'
#' @param weight  The GOC graph link weight to use in calculating the distance.
#'                Please see details in [distance()].
#'
#' @param ...     Additional arguments (not used).
#'
#' @return An object of class [corridor-class].
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs:
#' Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing methods
#' for scaling resistance surfaces. Landscape Ecology 28:1269-1291.
#'
#' Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape connectivity
#' on animal distribution: a functional grain approach. Ecography 36:1004-1016.
#'
#' Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape connectivity:
#' a guide to construction, analysis, and application for conservation.
#' Biological Conservation 144:44-55.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis
#' at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom graphics plot
#' @importFrom sf st_as_sf st_coordinates st_linestring st_multilinestring
#' @importFrom sf st_sf st_sfc
#' @importFrom terra boundaries
#' @include classes.R grain.R
#' @rdname corridor
#' @seealso [GOC()], [visualize()]
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_corridor.R
#'
setGeneric("corridor", function(x, ...) {
  standardGeneric("corridor")
})

#' @export
#' @rdname corridor
setMethod("corridor",
  signature = "goc",
  definition = function(x, whichThresh, coords, weight = "meanWeight", ...) {
    dots <- list(...)
    if (!is.null(dots$doPlot)) {
      warning("Argument 'doPlot' is deprecated and will be ignored.")
    }

    ## Check whichThresh
    if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(x@th)))) { # nolint
      stop("whichThresh must index a single threshold existing in the GOC object")
    }

    ## Check coords — accept matrix or sf
    if (inherits(coords, "sf")) {
      coords <- sf::st_coordinates(coords)[, 1:2, drop = FALSE]
    }

    if (ncol(coords) != 2) {
      stop(
        "coords must be an sf object or a matrix of two columns",
        "giving X and Y coordinates"
      )
    }

    if (nrow(coords) > 2) {
      warning("using only first two sets of coordinates for corridor start and end points")
      coords <- coords[1:2, ]
    }

    ## Check weight
    if (!(weight %in% names(edge_attr(x@th[[1]]$goc)))) {
      stop("link weight attribute with this name doesn't exist in GOC object")
    }

    ## GOC Graph — build sf LINESTRING object for edges
    edges <- as_edgelist(x@th[[whichThresh]]$goc)
    edgeNums <- seq_len(nrow(edges))
    v1idx <- sapply(edges[, 1], function(z) {
      which(V(x@th[[whichThresh]]$goc)$name == z)
    })
    v2idx <- sapply(edges[, 2], function(z) {
      which(V(x@th[[whichThresh]]$goc)$name == z)
    })
    edgeWeights <- edge_attr(x@th[[whichThresh]]$goc, weight)

    edgesGOC <- sf::st_sf(
      edgeNum = edgeNums,
      weight = edgeWeights,
      geometry = sf::st_sfc(lapply(seq_len(nrow(edges)), function(i) {
        sf::st_linestring(matrix(c(
          V(x@th[[whichThresh]]$goc)$centroidX[v1idx[i]],
          V(x@th[[whichThresh]]$goc)$centroidY[v1idx[i]],
          V(x@th[[whichThresh]]$goc)$centroidX[v2idx[i]],
          V(x@th[[whichThresh]]$goc)$centroidY[v2idx[i]]
        ), ncol = 2, byrow = TRUE))
      }))
    )

    verticesGOC <- sf::st_as_sf(
      data.frame(
        x = V(x@th[[whichThresh]]$goc)$centroidX,
        y = V(x@th[[whichThresh]]$goc)$centroidY
      ),
      coords = c("x", "y")
    )

    ## Shortest path
    startEndPolygons <- point(x, coords)$pointPolygon[, whichThresh]

    if (any(is.na(startEndPolygons))) {
      stop("corridor: 'coords' correspond to cells of value 'NA'.")
    }

    pths <- shortest_paths(
      graph = x@th[[whichThresh]]$goc,
      from = which(V(x@th[[whichThresh]]$goc)$polygonId == na.omit(startEndPolygons[1])),
      to = which(V(x@th[[whichThresh]]$goc)$polygonId == na.omit(startEndPolygons[2])),
      weights = V(x@th[[whichThresh]]$goc)$meanWeight
    )

    startEndPath <- if (length(pths$vpath) > 0) {
      pths |>
        (\(x) x[[1]])() |> ## extract 1st element; was %>% `[[`(1)
        (\(x) x[[1]])() |> ## extract 1st element; was %>% `[[`(1)
        as.numeric()
    } else {
      stop("corridor: all 'coords' correspond to cells of value 'NA'.")
    }

    pathCoords <- cbind(
      V(x@th[[whichThresh]]$goc)$centroidX[startEndPath],
      V(x@th[[whichThresh]]$goc)$centroidY[startEndPath]
    )
    shortestPathEdges <- sf::st_sf(
      geometry = sf::st_sfc(sf::st_linestring(pathCoords))
    )

    shortestPathVertices <- sf::st_as_sf(
      as.data.frame(pathCoords),
      coords = c("V1", "V2")
    )

    pathDist <- distances(
      x@th[[whichThresh]]$goc,
      v = V(x@th[[whichThresh]]$goc)[na.omit(startEndPath[1])],
      weights = edge_attr(x@th[[whichThresh]]$goc, weight)
    )[na.omit(startEndPath[length(startEndPath)])]

    voronoiBound <- terra::boundaries(grain(x, whichThresh = whichThresh)@voronoi, classes = TRUE)

    result <- new("corridor",
      voronoi = voronoiBound,
      linksSP = edgesGOC,
      nodesSP = verticesGOC,
      shortestLinksSP = shortestPathEdges,
      shortestNodesSP = shortestPathVertices,
      corridorLength = pathDist
    )

    return(result)
  }
)
