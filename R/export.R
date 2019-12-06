#' @author Paul Galpern
#' @keywords internal
.createDir  <- function(xType, dirname, path, overwrite) {
  if (is.null(dirname)) {
    dirname <- paste0(xType, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  newdir <- normalizePath(file.path(path, dirname), mustWork = FALSE)

  if (dir.exists(newdir) && !overwrite) {
    stop(paste0("directory ", newdir, " already exists. Use overwrite = TRUE."))
  }

  dir.create(newdir)
  return(newdir)
}

#' @author Paul Galpern
#' @importFrom raster extension<- writeRaster
#' @keywords internal
.wRas <- function(ras, fname, dirpath, rasterFormat, overwrite) {
  extensions <- data.frame(
    format = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", "EHdr", "HFA"),
    ext = paste0(".", c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", "bil", "img")),
    stringsAsFactors = FALSE
  )
  id <- which(extensions$format == rasterFormat)
  filename <- file.path(dirpath, fname)
  extension(filename) <- extensions[id, "ext"]
  writeRaster(ras, filename = filename, format = rasterFormat, overwrite = overwrite)
}

#' @author Paul Galpern
#' @importFrom rgdal writeOGR
#' @keywords internal
.wShp  <- function(sp, fname, dirpath, overwrite) {
  writeOGR(sp, dsn = dirpath, layer = fname, driver = "ESRI Shapefile",
           overwrite_layer = overwrite)
}

#' Export spatial data from MPG and GOC models
#'
#' @description
#' This function automates the export of raster and vector spatial data from \code{mpg} and
#' \code{grain} objects. By default it places them in a new directory, unless an existing one is
#' specified with \code{overwrite = TRUE}.
#'
#' It can also be used to process \code{mpg} and \code{grain} objects to
#' produce R spatial objects that are convenient for plotting or analysis within R.
#' Use \code{R = TRUE} in which case all parameters related to file export
#' are ignored. (Default \code{R = FALSE})
#'
#' The \code{raster} package \code{\link{writeRaster}} is used for rasters,
#' and \code{\link{writeOGR}} in the \code{rgdal} package is used to
#' export ESRI compatible shape files.
#'
#' @param x             A \code{mpg} or \code{grain} object
#'
#' @param dirname       The name of a new directory to create. If \code{NULL}
#'                      a directory with a name containing the date and time will be created.
#'
#' @param path          A path to where this new directory \code{dirname} should be created.
#'                      Defaults to the working directory.
#'
#' @param rasterFormat  The format for exported rasters. See \code{\link{writeFormats}} for
#'                      options. Defaults to GeoTiff (\code{rasterFormat='GTiff'}). Use
#'                      \code{rasterFormat='raster'} to save \code{.grd} files in
#'                      native \code{raster} package format.
#'
#' @param overwrite     If directory already exists will overwrite existing files inside.
#'                      Defaults to \code{FALSE}.
#'
#' @param R             If \code{TRUE}, return the spatial objects that would be written to files.
#'                      Do not write these files and ignore \code{dirname}, \code{path},
#'                      \code{rasterFormat}, \code{overwrite} parameters.
#'                      This is useful for visualization using R plotting functions,
#'                      or spatial analysis within R. Defaults to \code{FALSE}
#'
#' @param vorBound      Specify whether to create a raster with the boundaries of
#'                      the Voronoi polygons \code{=1} and the remainder \code{=NA}.
#'                      This may be useful for visualizing relationships among
#'                      polygons in a grain of connectivity.
#'                      This can add time to the export on very large rasters.
#'                      Defaults to \code{FALSE}.
#'
#' @param ...           Additional arguments (not used).
#'
#' @return   Invisibly returns the path to the folder created.
#'
#' Side effect of exporting files representing raster and vector spatial data
#' in the object.
#'
#' Please note that for vector data export the attribute name is limited
#' to 8 characters in shape files. See the tables below for the abbreviations
#' used and their meaning.
#'
#' \strong{Exported from \code{mpg} objects:}
#'
#' \code{nodes}, \code{linksCentroid}, \code{linksPerim} are shape files giving
#' the locations of the patch centroids, links among centroids, and links
#' among perimeters of patches respectively. \code{patchId, voronoi} are
#' rasters giving the patch identifier of the patch, or of the patch that
#' the Voronoi polygon refers to. \code{lcpPerimWeight, lcpLinkId} give
#' the weight in cost surface units of the shortest paths between perimeters,
#' and the identifiers of those links respectively. \code{vorBound} gives
#' the boundaries of the Voronoi polygons (if specified).
#'
#' Description of node (vertex) and link (edge) weights in \code{mpg} objects
#' and their corresponding attribute names in the shape files created.
#'
#' \tabular{llll}{
#'   \strong{type} \tab \strong{MPG name} \tab \strong{SHP name} \tab \strong{Description}\cr
#'   node \tab \code{patchId} \tab \code{patchId} \tab Patch ID from \code{patchId} raster \cr
#'   node \tab \code{patchArea} \tab \code{patchA} \tab Area of patch \cr
#'   node \tab \code{patchEdgeArea} \tab \code{patchEA} \tab Edge area of patch \cr
#'   node \tab \code{coreArea} \tab \code{coreA} \tab Area excluding edge of patch \cr
#'   node \tab \code{centroidX} \tab \code{ctrX} \tab Centroid of the patch (X)\cr
#'   node \tab \code{centroidY} \tab \code{ctrY} \tab Centroid of the patch (Y)\cr
#'   link \tab \code{e1} \tab \code{e1} \tab Id of first patch at end of link\cr
#'   link \tab \code{e2} \tab \code{e2} \tab Id of second patch at end of link\cr
#'   link \tab \code{linkId} \tab \code{linkId} \tab Link ID from \code{lcpLinkId} raster\cr
#'   link \tab \code{lcPerimWeight} \tab \code{lcpPerWt} \tab Cost length of link from patch perimeters\cr
#'   link \tab \code{startPerimX} \tab \code{strtPerX} \tab Coordinate of link endpoint on first patch (X)\cr
#'   link \tab \code{startPerimY} \tab \code{strtPerY} \tab Coordinate of link endpoint on first patch (Y)\cr
#'   link \tab \code{endPerimX} \tab \code{endPerX} \tab Coordinate of link endpoint on second patch (X)\cr
#'   link \tab \code{endPerimY} \tab \code{endPerY} \tab Coordinate of link endpoint on second patch (Y)\cr
#' }
#'
#' \strong{Exported from \code{grain} objects:}
#'
#' \code{nodes, linksCentroid} are shape files giving the locations of the
#' Voronoi polygon centroids and links among them respectively.
#' \code{voronoi} are rasters gives the polygon identifier of each cluster of patches.
#' \code{vorBound} gives the boundaries of the Voronoi polygons (if specified).
#'
#' Description of node (vertex) and link (edge) weights in \code{grain}
#' objects  and their corresponding attribute names in the shape files created.
#'
#' \tabular{llll}{
#'   \strong{Type} \tab \strong{GOC name} \tab \strong{SHP name} \tab \strong{Description}\cr
#'   node \tab \code{polygonId} \tab \code{polyId} \tab Polygon ID from grain voronoi raster \cr
#'   node \tab \code{polygonArea} \tab \code{polyA} \tab Area of polygon from grain voronoi raster \cr
#'   node \tab \code{totalPatchArea} \tab \code{patchA} \tab Total area of all patches in polygon\cr
#'   node \tab \code{totalPatchEdgeArea} \tab \code{patchEA} \tab Total area of all patch edges in polygon\cr
#'   node \tab \code{totalCoreArea} \tab \code{coreA} \tab Total area of patches in polygon excluding edges\cr
#'   node \tab \code{centroidX} \tab \code{ctrX} \tab Centroid of the polygon (X)\cr
#'   node \tab \code{centroidY} \tab \code{ctrY} \tab Centroid of the polygon (Y)\cr
#'   link \tab \code{e1} \tab \code{e1} \tab ID of first patch at end of link\cr
#'   link \tab \code{e2} \tab \code{e2} \tab ID of second patch at end of link\cr
#'   link \tab \code{maxWeight} \tab \code{maxWt} \tab The maximum weight of all links connecting
#'                                                     patches between polygons\cr
#'   link \tab \code{linkIdMaxWeight} \tab \code{maxWt} \tab The link id of that maximum weight
#'                                                           \code{link (lcpLinkId)}\cr
#'   link \tab \code{minWeight} \tab \code{min} \tab The minimum weight of all links connecting
#'                                                   patches between polygons\cr
#'   link \tab \code{linkIdMinWeight} \tab \code{minWt} \tab The link id of that minimum weight
#'                                                           \code{link (lcpLinkId)}\cr
#'   link \tab \code{medianWeight} \tab \code{medWt} \tab The median weight of all links connecting
#'                                                        patches between polygons\cr
#'   link \tab \code{meanWeight} \tab \code{meanWT} \tab The minimum weight of all links connecting
#'                                                       patches between polygons\cr
#'   link \tab \code{numlinksWeight} \tab \code{numEWt} \tab The number of links connecting patches
#'                                                           between polygons\cr
#'   link \tab \code{eucCentroidWeight} \tab \code{eucCtrWt} \tab The Euclidean distance between
#'                                                                centroids of polygons\cr
#' }
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom raster boundaries projection projection<- writeRaster
#' @importFrom rgdal writeOGR
#' @importFrom sp coordinates<- CRS Line Lines proj4string proj4string<-
#' @importFrom sp SpatialLines SpatialLinesDataFrame
#' @include classes.R
#' @rdname export
#' @seealso \code{\link{MPG}}, \code{\link{GOC}}, \code{\link{grain}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_export.R
#'
setGeneric(
  "export",
  function(x, dirname = NULL, path = ".", rasterFormat = "GTiff",
           overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    standardGeneric("export")
})

#' @export
#' @rdname export
setMethod(
  "export",
  signature = "mpg",
  definition = function(x, dirname = NULL, path = ".", rasterFormat = "GTiff",
                        overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    if (!R) {
      dirpath <- .createDir("mpg", dirname, path, overwrite)
    }

    ## Prepare links
    linksDF <- graphdf(x)[[1]]$e
    names(linksDF) <- c("e1", "e2", "linkId", "lcpPerWt", "strtPerX", "strtPerY",
                        "endPerX", "endPerY")
    nodesDF <- graphdf(x)[[1]]$v[, -1]
    names(nodesDF) <- c("patchId", "patchA", "patchEA", "coreA", "ctrX", "ctrY")

    firstCentr <- nodesDF[match(linksDF$e1, nodesDF$patchId), c("ctrX", "ctrY")]
    names(firstCentr) <- c("strtCtrX", "strtCtrY")
    secondCentr <- nodesDF[match(linksDF$e2, nodesDF$patchId), c("ctrX", "ctrY")]
    names(secondCentr) <- c("endCtrX", "endCtrY")
    linksCentr <- cbind(linksDF, firstCentr, secondCentr)
    row.names(linksCentr) <- linksDF$linkId
    linksCentrSP <- linksCentr[, c("strtCtrX", "strtCtrY", "endCtrX", "endCtrY", "linkId")] %>%
      apply(., 1, function(x) {
        Lines(Line(matrix(x[1:4], 2, 2, byrow = TRUE)), ID = as.character(x[5]))
      }) %>%
      SpatialLines(.) %>%
      SpatialLinesDataFrame(., data = linksCentr)
    projection(linksCentrSP) <- CRS(projection(x@patchId))

    firstPerim <- linksDF[, c("strtPerX", "strtPerY")]
    secondPerim <- linksDF[, c("endPerX", "endPerY")]
    linksPerim <- cbind(linksDF, firstPerim, secondPerim)
    row.names(linksPerim) <- linksDF$linkId
    linksPerimSP <- linksPerim[, c("strtPerX", "strtPerY", "endPerX", "endPerY", "linkId")] %>%
      apply(., 1, function(x) {
        Lines(Line(matrix(x[1:4], 2, 2, byrow = TRUE)), ID = as.character(x[5]))
      }) %>%
      SpatialLines(.) %>%
      SpatialLinesDataFrame(., data = linksPerim[, -which(duplicated(names(linksPerim)))])
    proj4string(linksPerimSP) <- CRS(projection(x@patchId))

    ## Create voronoi boundaries
    if (vorBound) {
      message("Extracting voronoi boundaries...")
      vorB <- boundaries(x@voronoi, class = TRUE, asNA = TRUE)
    } else {
      vorB <- "Not created. Use vorBound=TRUE."
    }

    ## Prepare nodes
    nodesSP <- nodesDF
    coordinates(nodesSP) <- ~ ctrX + ctrY
    proj4string(nodesSP) <- CRS(projection(x@patchId))

    if (!R) {
      ## Write shapefiles
      .wShp(nodesSP, "nodes", dirpath, overwrite)
      .wShp(linksCentrSP, "linksCentroid", dirpath, overwrite)
      .wShp(linksPerimSP, "linksPerim", dirpath, overwrite)

      ## Write rasters
      .wRas(x@patchId, "patchId", dirpath, rasterFormat, overwrite)
      .wRas(x@voronoi, "voronoi", dirpath, rasterFormat, overwrite)
      .wRas(x@lcpPerimWeight, "lcpPerimWeight", dirpath, rasterFormat, overwrite)
      .wRas(x@lcpLinkId, "lcpLinkId", dirpath, rasterFormat, overwrite)
      if (vorBound) .wRas(vorB, "vorBound", dirpath, rasterFormat, overwrite)

      message("Exported to:", normalizePath(dirpath))
      invisible(normalizePath(dirpath))
    } else {
      returnSpatial <- list(
        nodes = nodesSP,
        linksCentroid = linksCentrSP,
        linksPerim = linksPerimSP,
        patchId = x@patchId,
        voronoi = x@voronoi,
        lcpPerimWeight = x@lcpPerimWeight,
        lcpLinkId = x@lcpLinkId,
        vorBound = vorB
      )

      return(returnSpatial)
    }
})

#' @export
#' @rdname export
setMethod(
  "export",
  signature = "grain",
  definition = function(x, dirname = NULL, path = ".", rasterFormat = "GTiff",
                        overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    if (!R) {
      dirpath <- .createDir("grain", dirname, path, overwrite)
    }

    ## Prepare links
    linksDF <- graphdf(x@th)[[1]]$e[, -10]
    names(linksDF) <- c("e1", "e2", "maxWt", "lidMaxWt", "minWt", "lidMinWt",
                        "medWt", "meanWt", "numEWt", "eucCtrWt")
    nodesDF <- graphdf(x@th)[[1]]$v[, -c(1, 9)]
    names(nodesDF) <- c("polyId", "ctrX", "ctrY", "polyA", "patchA", "patchEA", "coreA")

    firstCentr <- nodesDF[match(linksDF$e1, nodesDF$polyId), c("ctrX", "ctrY")]
    names(firstCentr) <- c("strtCtrX", "strtCtrY")
    secondCentr <- nodesDF[match(linksDF$e2, nodesDF$polyId), c("ctrX", "ctrY")]
    names(secondCentr) <- c("endCtrX", "endCtrY")
    linksCentr <- cbind(linksDF, firstCentr, secondCentr)
    row.names(linksCentr) <- 1:nrow(linksCentr)
    linksCentr$plinkId <- 1:nrow(linksCentr)
    linksCentrSP <- linksCentr[, c("strtCtrX", "strtCtrY", "endCtrX", "endCtrY", "plinkId")] %>%
      apply(., 1, function(x) {
        Lines(Line(matrix(x[1:4], 2, 2, byrow = TRUE)), ID = as.character(x[5]))
      }) %>%
      SpatialLines() %>%
      SpatialLinesDataFrame(., data = linksCentr)
    projection(linksCentrSP) <- CRS(projection(x@voronoi))

    ## Create voronoi boundaries
    if (vorBound) {
      message("Extracting voronoi boundaries...")
      vorB <- boundaries(x@voronoi, class = TRUE, asNA = TRUE)
    } else {
      vorB <- "Not created. Use vorBound=TRUE."
    }

    ## Prepare nodes
    nodesSP <- nodesDF
    coordinates(nodesSP) <- ~ ctrX + ctrY
    proj4string(nodesSP) <- CRS(projection(x@voronoi))

    if (!R) {
      ## Write shapefiles
      .wShp(nodesSP, "nodes", dirpath, overwrite)
      .wShp(linksCentrSP, "linksCentroid", dirpath, overwrite)

      ## Write rasters
      .wRas(x@voronoi, "voronoi", dirpath, rasterFormat, overwrite)
      if (vorBound) .wRas(vorB, "vorBound", dirpath, rasterFormat, overwrite)

      message("Exported to:", normalizePath(dirpath))
      invisible(normalizePath(dirpath))
    } else {
      returnSpatial <- list(
        nodes = nodesSP,
        linksCentroid = linksCentrSP,
        voronoi = x@voronoi,
        vorBound = vorB
      )

      return(returnSpatial)
    }
})

#' @export
#' @rdname export
setMethod(
  "export",
  signature = "goc",
  definition = function(x, dirname = NULL, path = ".",
                        overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    message("Use grain() to extract a single grain of connectivity to export.")
    return(invisible())
})
