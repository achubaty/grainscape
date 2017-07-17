#' @author Paul Galpern
#' @keywords internal
.gFinal <- function(g, print, theme) {
  if (theme) {
    g <- g + theme_grainscape() + coord_equal()
  }
  else {
    g <- g + theme(aspect.ratio=1)
  }
  if (!print) {
    invisible(g)
  }
  else {
    g
  }
}
#' Plot quick visualizations of \code{grainscape} objects
#'
#' @description
#' Plot quick visualizations of \code{mpg}, \code{grain}, and \code{corridor}
#' objects.
#'
#' This function is intended to get a quick look at the state of a \code{grainscape}
#' object by rendering what are likely to be the most universally useful
#' visualizations of the spatial data within these objects.
#'
#' Much more control is available using \code{\link{ggGS}} with \code{\link{ggplot}}
#' enabling the layering of different different analytical products, and the
#' visualization of node and link attributes.
#'
#' For high-resolution visualization and the greatest level of control use
#' \code{\link{export}} to export spatial objects for cartographic representation
#' in a geographic information system (GIS).
#'
#' @param x       A \code{grainscape} object (\code{corridor}, \code{grain}, or \code{mpg}).
#'
#' @param y       Ignored.
#'
#' @param quick   If \code{NULL} (the default) it will plot the most useful quick
#'                visualization for the supplied object type.  See below for a
#'                description of the available quick plots, and the defaults.
#'
#' @param print   Render the \code{ggplot} on the default graphics device.
#'                Default is \code{TRUE}.
#'
#' @param theme   Apply grainscape theme and scale aesthetics.  Default is \code{TRUE}.                     .
#'
#' @param ...     Additional arguments (not used).

#' @return        Invisably, a \code{ggplot2} object to which additional \code{ggplot}
#'                geoms adn adjustments can be applied.  Has the side effect of
#'                rendering the plot, unless \code{print = FALSE}.
#'
#' @note
#' \strong{Types of visualization available with the \code{quick} parameter}
#'
#' \code{'mpgPerimPlot'} gives a a vector rendering of the minimum planar
#' graph with vector links connecting the perimeters of the patches.  This
#' doesn't accurately represent the sinuosity of paths of the links between patches
#' but offers a good approximation that renders better at large extents.
#' Default for \code{mpg} objects. Not available for other objects.
#'
#' \code{'mpgPlot'} gives a raster-only rendering of the minimum planar graph
#' where \code{patchId} are positive integers, and \code{linkId} are negative
#' integers showing the shortest paths between patches  Only available for
#' \code{mpg} objects.
#'
#' \code{'network'} gives a vector rendering of the minimum planar graph or
#' the grains of connectivity network with nodes and links plotted at the
#' patch or polygon centroid locations. Available for \code{mpg} and \code{grain}
#' objects. Default for \code{grain} objects.
#'
#' \code{'grainPlot'} gives a raster and vector rendering of the grains of
#' connectivity network with nodes and links plotted at polygon centroid locations,
#' superimposed over the boundaries of the Voronoi polygons.  Can be time
#' consuming on large rasters due to the Voronoi boundary extraction.
#' Only available for \code{grain} objects.
#'
#' \code{'corridorPlot'} renders the output of a \code{\link{corridor}} analysis.
#' It is the only option available with \code{corridor} objects and the default.
#'
#'
#' @author Alex Chubaty and Paul Galpern
#' @docType methods
#' @export
#' @importFrom ggplot2 ggplot scale_fill_identity
#' @include classes.R
#' @rdname plot
#' @seealso \code{\link{ggGS}},
#'          \code{\link{export}},
#'          \code{\linkS4class{corridor}},
#'          \code{\linkS4class{grain}},
#'          \code{\linkS4class{mpg}}
#'
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))
#'
#' ## Produce a grains of connectivity model
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## MPG and showing simplified links among the perimeters of patches
#' plot(tinyPatchMPG)
#'
#' ## MPG showing links among the nodes of connected patches
#' plot(tinyPatchMPG, quick = "network")
#'
#' ## MPG showing the shortest paths between patches actually used to
#' ## to calculate link weight values
#' plot(tinyPatchMPG, quick = "mpgPlot")
#'
#' ## A grain of connectivity network plot with Voronoi boundaries
#' plot(grain(tinyPatchGOC, 3), quick = "grainPlot")
#'
#' ## Capture plot output for further processing with ggplot
#' g <- plot(tinyPatchMPG, print = FALSE, theme = FALSE)
#' g <- g + theme_minimal() + ggtitle("Minimum planar graph") +
#'        theme(plot.title = element_text(size=20, hjust = 0.5)) +
#'        theme(legend.position = "none") +
#'        xlab("Easting") + ylab("Northing")
#' g
#' ## To change aesthetics it is best to build the plot from scratch
#' ## using grainscape::ggGS().  See examples therein.
#' }
#' @export
#' @rdname plot
setMethod(
  "plot",
  signature = "corridor",
  definition = function(x, y, quick=NULL, print=TRUE, theme=TRUE, ...) {

    .linksToDF <- function(x) {
      out <- do.call(rbind,
                lapply(
                  lapply(slot(x, "lines"), function(x)
                    lapply(slot(x, "Lines"), function(y)
                      slot(y, "coords"))), function(z)
                      c(z[[1]][1,], z[[1]][2,])))
      dimnames(out) <- NULL
      out <- data.frame(out)
      names(out) <- c("x1", "y1", "x2", "y2")
      return(out)
    }

    .pathToDF <- function(x) {
      out <- lapply(slot(x, "lines"), function(x)
               lapply(slot(x, "Lines"), function(y)
                 slot(y, "coords")))[[1]][[1]]
      out <- do.call(rbind,
               lapply(1:(nrow(out)-1), function(x)
                 c(out[x, ], out[x+1, ])))
      dimnames(out) <- NULL
      out <- data.frame(out)
      names(out) <- c("x1", "y1", "x2", "y2")
      return(out)
    }
    linesDF <- rbind(
                 data.frame(.linksToDF(x@linksSP), cols = "forestgreen", sz = 1),
                 data.frame(.pathToDF(x@shortestLinksSP), cols = "black", sz = 2))

    pointsDF <- rbind(
                  data.frame(coordinates(x@nodesSP), cols = "forestgreen", sz = 2),
                  data.frame(coordinates(x@shortestNodesSP), cols = "black", sz = 3))
    names(pointsDF) <- c("x", "y", "cols", "sz")

    message("Extracting Voronoi boundaries...")
    g <- ggplot() +
            geom_raster(data=ggGS(x@voronoi, "vorBound"),
              aes(x=x, y=y, fill=value > 0)) +
            scale_fill_manual(values=c("white", "grey")) +
            geom_segment(data=linesDF,
              aes(x=x1, y=y1, xend=x2, yend=y2, colour=cols, size=sz)) +
            geom_point(data=pointsDF, aes(x=x, y=y, col=cols, size=sz)) +
            scale_colour_identity() +
            scale_size_identity()
    if (theme) {
      g <- g + theme_grainscape()
    }
    g <- g + theme(legend.position="none")
    .gFinal(g, print, theme=FALSE)
})

#' @export
#' @rdname plot
setMethod(
  "plot",
  signature = "grain",
  definition = function(x, y, quick=NULL, print=TRUE, theme=TRUE, ...) {

    if ((is.null(quick)) || (quick == "network")) {
      col.par = function(n) {
        sample(seq(0.3, 1, length.out=50), n, replace=TRUE)
      }
      vor <- ggGS(x, "voronoi")
      nVor <- max(vor$value, na.rm = TRUE)
      cols <- data.frame(n=1:nVor,
                         col=rainbow(nVor, s=col.par(nVor),
                                     v=col.par(nVor))[sample(nVor, replace=TRUE)],
                         stringsAsFactors=FALSE)
      vor <- cbind(vor, cols=cols[match(vor$value, cols$n), "col"])
      g <- ggplot() +
        geom_raster(data=vor, aes(x=x, y=y, fill=cols), alpha=0.5) +
        scale_fill_identity()
      g <- g +
        geom_segment(data=ggGS(x, "links"),
                     aes(x = x1, y = y1, xend = x2, yend = y2, colour = "black"))
      g <- g +
        geom_point(data=ggGS(x, "nodes"),
                   aes(x = x, y = y, colour = "black")) +
        scale_colour_identity()
      if (theme) {
        g <- g + theme_grainscape()
      }
      g <- g + theme(legend.position="none")
      .gFinal(g, print, theme=FALSE)
    }
    else if (quick == "grainPlot") {
      g <- ggplot() +
        geom_raster(data=ggGS(x, "vorBound"), aes(x=x, y=y, fill=value > 0)) +
        scale_fill_manual(values=c("white", "grey"))
      g <- g +
        geom_segment(data=ggGS(x, "links"),
                     aes(x = x1, y = y1, xend = x2, yend = y2, colour = "forestgreen"))
      g <- g +
        geom_point(data=ggGS(x, "nodes"),
                   aes(x = x, y = y, colour = "darkgreen")) +
        scale_colour_identity()
      if (theme) {
        g <- g + theme_grainscape()
      }
      g <- g + theme(legend.position="none")
      .gFinal(g, print, theme=FALSE)
    }
    else {
      stop("quick parameter not valid for a grain object")
    }
  })

#' @export
#' @rdname plot
setMethod(
  "plot",
  signature = "mpg",
  definition = function(x, y, quick=NULL, print=TRUE, theme=TRUE, ...) {

    if ((is.null(quick)) || (quick == "mpgPerimPlot")) {
      g <- ggplot() +
        geom_raster(data=ggGS(x, "patchId"), aes(x=x, y=y, fill=value > 0)) +
        scale_fill_manual(values="grey")
      g <- g +
        geom_segment(data=ggGS(x, "links"),
          aes(x = x1p, y = y1p, xend = x2p, yend = y2p, colour = "forestgreen")) +
        scale_colour_identity()

    # Plot points at the ends of links (too crowded in most use cases)
    # g <- g +
    #    geom_point(data=ggGS(x, "links"),
    #      aes(x = x1p, y = y1p, colour = "forestgreen"), size=1) +
    #    geom_point(data=ggGS(x, "links"),
    #      aes(x = x2p, y = y2p, colour = "forestgreen"), size=1)

      if (theme) {
        g <- g + theme_grainscape()
      }
      g <- g + theme(legend.position="none")
      .gFinal(g, print, theme=FALSE)
    }
    else if (quick == "network") {
      g <- ggplot() +
        geom_raster(data=ggGS(x, "patchId"), aes(x=x, y=y, fill=value > 0)) +
        scale_fill_manual(values="grey")
      g <- g +
        geom_segment(data=ggGS(x, "links"),
                     aes(x = x1, y = y1, xend = x2, yend = y2, colour = "forestgreen"))
      g <- g +
        geom_point(data=ggGS(x, "nodes"),
                   aes(x = x, y = y, colour = "darkgreen")) +
        scale_colour_identity()
      if (theme) {
        g <- g + theme_grainscape()
      }
      g <- g + theme(legend.position="none")
      .gFinal(g, print, theme=FALSE)
    }
    else if (quick == "mpgPlot") {
      g <- ggplot() +
        geom_raster(data=ggGS(x, "mpgPlot"),
          aes(x = x, y = y, fill = ifelse(value > 0, "grey", "forestgreen"))) +
        scale_fill_identity() +
        theme(legend.position="none")
      if (theme) {
        g <- g + theme_grainscape()
      }
      .gFinal(g, print, theme=FALSE)
    }
    else {
      stop("quick parameter not valid for an mpg object")
    }
  })
