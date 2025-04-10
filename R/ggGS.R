#' Prepare data in `MPG` and `grain` objects for use with \pkg{ggplot2}
#'
#' @description
#' This is an informal `fortify`-type method that prepares either
#' `RasterLayer` or `igraph` objects contained as slots within
#' `MPG` or `grain` objects for easy plotting with [ggplot2::ggplot()].
#'
#'
#' @param x       A `mpg`, `grain`, or `RasterLayer` object.
#'
#' @param type    If a `mpg` or `grain` object is supplied, this
#'                gives the name of the slot to prepare for plotting.
#'                Options are discussed below.
#'                Not required if a `RasterLayer` is supplied.
#'
#' @param ...  Additional arguments (not used).
#'
#' @return A `data.frame` suitable for plotting with [ggplot2::ggplot()].
#'
#' Where `type` is a raster the `data.frame` will have the following columns:
#'
#' \describe{
#'   \item{`value`}{the value of the raster cell}
#'   \item{`x`}{the x coordinate of the centre of the raster cell}
#'   \item{`y`}{the y coordinate of the centre of the raster cell}
#' }
#'
#' Where `type = 'nodes'` the `data.frame` will have the following columns:
#'
#' \describe{
#'   \item{`x`}{the x coordinate of the node}
#'   \item{`y`}{the y coordinate of the node}
#'   \item{`...`}{other attributes associated with the network nodes}
#' }
#'
#' Where `type = 'links'` the `data.frame` will have the following columns:
#'
#' \describe{
#'   \item{`x1`}{the x coordinate of the first node}
#'   \item{`y1`}{the y coordinate of the first node}
#'   \item{`x2`}{the x coordinate of the second node}
#'   \item{`y2`}{the y coordinate of the second node}
#'   \item{`x1p`}{the x coordinate at the perimeter of the first node}
#'   \item{`y1p`}{the y coordinate at the perimeter of the first node}
#'   \item{`x2p`}{the x coordinate at the perimeter of the second node}
#'   \item{`y2p`}{the y coordinate at the perimeter of the second node}
#'   \item{`...`}{other attributes associated with the network links}
#' }
#'
#' @note
#' **Options for `type` parameter**
#'
#'  If a `RasterLayer` is supplied `type` is optional.
#'
#'  For `mpg` `type` options are `"node"` or `"links"`.
#'  This prepares the nodes and links of the minimum planar graph network for
#'  plotting,  Also `"patchId"`, `"voronoi"`, `"lcpPerimWeight"`,
#'  `"lcpLinkId"`, `"mpgPlot"` will prepare rasters for plotting.
#'
#'  For `grain` objects `type`  options are `"nodes"` or`"links"`
#'  to prepare the nodes and links of the grains of connectivity network  for
#'  plotting. Also `"voronoi"` will prepare the grains of connectivity
#'  Voronoi polygons raster for plotting.
#'
#'  For either `mpg` or `grain` objects `type = "vorBound"`
#'  will identify the boundaries of the Voronoi polygons for plotting.
#'  This is potentially time consuming for large rasters.
#'
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom sp SpatialPixelsDataFrame
#' @importFrom raster boundaries
#' @importFrom utils type.convert
#' @include classes.R
#' @rdname ggGS
#' @seealso [MPG()], [GOC()]
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_ggGS.R
#'
setGeneric("ggGS", function(x, type = NULL, ...) {
  standardGeneric("ggGS")
})

#' @export
#' @rdname ggGS
setMethod(
  "ggGS",
  signature = "RasterLayer",
  definition = function(x, type = NULL, ...) {
    out <- as.data.frame(as(x, "SpatialPixelsDataFrame"))
    names(out) <- c("value", "x", "y")
    return(out)
  }
)

#' @export
#' @rdname ggGS
setMethod(
  "ggGS",
  signature = "list",
  definition = function(x, type, ...) {
    if (type == "nodes") {
      out <- x[[1]]$v
      names(out)[names(out) == "centroidX"] <- "x"
      names(out)[names(out) == "centroidY"] <- "y"
      return(out)
    } else if (type == "links") {
      nodes <- x[[1]]$v
      links <- x[[1]]$e
      names(nodes) <- gsub("polygonId", "patchId", names(nodes))
      first <- nodes[match(links$e1, nodes$patchId), c("centroidX", "centroidY")]
      names(first) <- c("x1", "y1")
      second <- nodes[match(links$e2, nodes$patchId), c("centroidX", "centroidY")]
      names(second) <- c("x2", "y2")
      out <- data.frame(first, second, links)

      ## Rename perimeter columns (in mpg objects only)
      names(out)[names(out) == "startPerimX"] <- "x1p"
      names(out)[names(out) == "startPerimY"] <- "y1p"
      names(out)[names(out) == "endPerimX"] <- "x2p"
      names(out)[names(out) == "endPerimY"] <- "y2p"
      return(out)
    } else {
      stop("parameter 'type' not valid for a list object")
    }
  }
)

#' @export
#' @rdname ggGS
setMethod(
  "ggGS",
  signature = "mpg",
  definition = function(x, type, ...) {
    if (is.null(type)) {
      stop("type parameter must be supplied with mpg objects")
    }
    if (type %in% slotNames(x)[-which(slotNames(x) %in% c("voronoi", "mpg"))]) {
      ggGS(slot(x, type))
    } else if (type == "voronoi") {
      out <- slot(x, type)
      out[out[] == 0] <- NA
      ggGS(out)
    } else if (type == "vorBound") {
      message("Extracting voronoi boundaries...")
      out <- x@voronoi
      out[out[] == 0] <- NA
      out <- boundaries(out, classes = TRUE)
      ggGS(out)
    } else if (type %in% c("nodes", "links")) {
      ggGS(graphdf(x), type = type)
    } else {
      stop("parameter 'type' not valid for mpg object")
    }
  }
)

#' @export
#' @rdname ggGS
setMethod(
  "ggGS",
  signature = "grain",
  definition = function(x, type, ...) {
    if (type == "voronoi") {
      out <- slot(x, type)
      out[out[] == 0] <- NA
      ggGS(out)
    } else if (type == "vorBound") {
      message("Extracting voronoi boundaries...")
      out <- x@voronoi
      out[out[] == 0] <- NA
      out <- boundaries(out, classes = TRUE)
      ggGS(out)
    } else if (type %in% c("nodes", "links")) {
      ggGS(graphdf(x), type = type)
    } else {
      stop("type parameter not valid for a grain object.")
    }
  }
)

#' @export
#' @rdname ggGS
setMethod(
  "ggGS",
  signature = "goc",
  definition = function(x, type, ...) {
    message("Use grain() first to prepare GOC objects for plotting.")
    return(invisible())
  }
)
