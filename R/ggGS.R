#' Prepare data in \code{MPG} and \code{grain} objects for use with \code{ggplot2}
#'
#' @description
#' This is an informal \code{fortify}-type method that prepares either
#' \code{RasterLayer} or \code{igraph} objects contained as slots within
#' \code{MPG} or \code{grain} objects for easy plotting with \code{\link{ggplot}}.
#'
#'
#' @param x       A \code{mpg}, \code{grain}, or \code{RasterLayer} object.
#'
#' @param type    If a \code{mpg} or \code{grain} object is supplied, this
#'                gives the name of the slot to prepare for plotting. Options
#'                are discussed below. Not required if a \code{RasterLayer}
#'                is supplied.
#'
#' @param ...  Additional arguments (not used).
#'
#' @return A \code{data.frame} suitable for plotting with \code{\link{ggplot}}.
#'
#' Where \code{type} is a raster the \code{data.frame} will have the following columns:
#'
#' \describe{
#'   \item{\code{value}}{the value of the raster cell}
#'   \item{\code{x}}{the x coordinate of the centre of the raster cell}
#'   \item{\code{y}}{the y coordinate of the centre of the raster cell}
#' }
#'
#' Where \code{type = 'nodes'} the \code{data.frame} will have the following columns:
#'
#' \describe{
#'   \item{\code{x}}{the x coordinate of the node}
#'   \item{\code{y}}{the y coordinate of the node}
#'   \item{\code{...}}{other attributes associated with the network nodes}
#' }
#'
#' Where \code{type = 'links'} the \code{data.frame} will have the following columns:
#'
#' \describe{
#'   \item{\code{x1}}{the x coordinate of the first node}
#'   \item{\code{y1}}{the y coordinate of the first node}
#'   \item{\code{x2}}{the x coordinate of the second node}
#'   \item{\code{y2}}{the y coordinate of the second node}
#'   \item{\code{x1p}}{the x coordinate at the perimeter of the first node}
#'   \item{\code{y1p}}{the y coordinate at the perimeter of the first node}
#'   \item{\code{x2p}}{the x coordinate at the perimeter of the second node}
#'   \item{\code{y2p}}{the y coordinate at the perimeter of the second node}
#'   \item{\code{...}}{other attributes associated with the network links}
#' }
#'
#' @note
#' \strong{Options for \code{type} parameter}
#'
#'  If a \code{RasterLayer} is supplied \code{type} is optional.
#'
#'  For \code{mpg} \code{type} options are \code{"node"} or \code{"links"}.
#'  This prepares the nodes and links of the minimum planar graph network for
#'  plotting,  Also \code{"patchId"}, \code{"voronoi"}, \code{"lcpPerimWeight"},
#'  \code{"lcpLinkId"}, \code{"mpgPlot"} will prepare rasters for plotting.
#'
#'  For \code{grain} objects \code{type}  options are \code{"nodes"} or\code{"links"}
#'  to prepare the nodes and links of the grains of connectivity network  for
#'  plotting. Also \code{"voronoi"} will prepare the grains of connectivity
#'  Voronoi polygons raster for plotting.
#'
#'  For either \code{mpg} or \code{grain} objects \code{type = "vorBound"}
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
#' @seealso \code{\link{MPG}}, \code{\link{GOC}}
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
})

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
})

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
})

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
})

#' @export
#' @rdname ggGS
setMethod(
  "ggGS",
  signature = "goc",
  definition = function(x, type, ...) {
    message("Use grain() first to prepare GOC objects for plotting.")
    return(invisible())
})
