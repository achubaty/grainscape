#' Export spatial data from MPG and GOC models
#'
#' @description
#' This function automates the export of raster and vector spatial data from
#' \code{mpg} and \code{grain} objects.  By default it places them in a new directory,
#' unless an existing one is specified with \code{overwrite=TRUE}.
#'
#' It can also be used to process \code{mpg} and \code{grain} objects to
#' produce R spatial objects that are convenient for plotting or analysis within R.
#' Use \code{R=TRUE} in which case all parameters related to file export
#' are ignored.  (Default \code{R=FALSE})
#'
#' The \code{raster} package \code{\link{writeRaster}} is used for rasters,
#' and \code{\link{writeOGR}} in the \code{rgdal} package is used to
#' export ESRI compatible shape files.
#'
#' @param x             A \code{mpg} or \code{grain} object
#'
#' @param dirname       The name of a new directory to create.  If \code{NULL}
#'                      a directory with a name containing the date and time will be created.
#'
#' @param path          A path to where this new directory \code{dirname} should be created.
#'                      Defaults to the working directory.
#'
#' @param rasterFormat  The format for exported rasters.  See \code{\link{writeFormats}} for
#'                      options.  Defaults to GeoTiff (\code{rasterFormat='GTiff'}).  Use
#'                      \code{rasterFormat='raster'} to save \code{.grd} files in
#'                      native \code{raster} package format.
#'
#' @param overwrite     If directory already exists will overwrite existing files inside.
#'                      Defaults to \code{FALSE}.
#'
#' @param R             If \code{TRUE}, return the spatial objects that would be written to files.  Do not
#'                      write these files and ignore \code{dirname, path, rasterFormat, overwrite}
#'                      parameters.  This is useful for visualization using R plotting functions, or
#'                      spatial analysis within R. Defaults to \code{FALSE}
#'
#' @param vorBound      Specify whether to create a raster with the boundaries of the Voronoi
#'                      polygons \code{=1} and the remainder \code{=NA}.  This may be useful
#'                      for visualizing relationships among polygons in a grain of connectivity.
#'                      This can add time to the export on very large rasters.
#'                      Defaults to \code{FALSE}.
#'
#' @param ...           Additional arguments (not used).
#'
#' @return   Invisibly returns the path to the folder created.
#'
#'   Side effect of exporting files representing raster and vector spatial data
#'   in the object.
#'
#'   Please note that for vector data export the attribute name is limited
#'   to 8 characters in shape files.  See the tables below for the abbreviations
#'   used and their meaning.
#'
#'   \strong{Exported from \code{mpg} objects:}
#'
#'   \code{nodes, linksCentroid, linksPerim} are shape files giving the
#'   locations of the patch centroids, links among centroids, and links
#'   among perimeters of patches respectively.  \code{patchId, voronoi} are
#'   rasters giving the patch identifier of the patch, or of the patch that
#'   the Voronoi polygon refers to.  \code{lcpPerimWeight, lcpLinkId} give
#'   the weight in cost surface units of the shortest paths between perimeters,
#'   and the identifiers of those links respectively.  \code{vorBound} gives
#'   the boundaries of the Voronoi polygons (if specified).
#'
#'   Description of node (vertex) and link (edge) weights in \code{mpg}
#'   objects  and their corresponding attribute names in the
#'   shape files created.
#'
#' \tabular{llll}{
#'   \strong{type} \tab \strong{MPG name} \tab \strong{SHP name} \tab \strong{Description}\cr
#'   node \tab patchId \tab patchId \tab Patch id from patchId raster \cr
#'   node \tab patchArea \tab patchA \tab Area of patch \cr
#'   node \tab patchEdgeArea \tab patchEA \tab Edge area of patch \cr
#'   node \tab coreArea \tab coreA \tab Area excluding edge of patch \cr
#'   node \tab centroidX \tab ctrX \tab Centroid of the patch (X)\cr
#'   node \tab centroidY \tab ctrY \tab Centroid of the patch (Y)\cr
#'   link \tab e1 \tab e1 \tab Id of first patch at end of link\cr
#'   link \tab e2 \tab e2 \tab Id of second patch at end of link\cr
#'   link \tab linkId \tab linkId \tab Link id from lcpLinkId raster\cr
#'   link \tab lcPerimWeight \tab lcpPerWt \tab Cost length of link from patch perimeters\cr
#'   link \tab startPerimX \tab strtPerX \tab Coordinate of link endpoint on first patch (X)\cr
#'   link \tab startPerimY \tab strtPerY \tab Coordinate of link endpoint on first patch (Y)\cr
#'   link \tab endPerimX \tab endPerX \tab Coordinate of link endpoint on second patch (X)\cr
#'   link \tab endPerimY \tab endPerY \tab Coordinate of link endpoint on second patch (Y)\cr
#'}
#'
#'   \strong{Exported from \code{grain} objects:}
#'
#'   \code{nodes, linksCentroid} are shape files giving the locations of the
#'   Voronoi polygon centroids and links among them respectively. \code{voronoi}
#'   are rasters gives the polygon identifier of each cluster of patches.
#'   \code{vorBound} gives the boundaries of the Voronoi polygons (if specified).
#'
#'   Description of node (vertex) and link (edge) weights in \code{grain}
#'   objects  and their corresponding attribute names in the
#'   shape files created.
#'
#' \tabular{llll}{
#'   \strong{type} \tab \strong{GOC name} \tab \strong{SHP name} \tab \strong{Description}\cr
#'   node \tab polygonId \tab polyId \tab Polygon id from grain voronoi raster \cr
#'   node \tab polygonArea \tab polyA \tab Area of polygon from grain voronoi raster \cr
#'   node \tab totalPatchArea \tab patchA \tab Total area of all patches in polygon\cr
#'   node \tab totalPatchEdgeArea \tab patchEA \tab Total area of all patch edges in polygon\cr
#'   node \tab totalCoreArea \tab coreA \tab Total area of patches in poylgon excluding edges\cr
#'   node \tab centroidX \tab ctrX \tab Centroid of the polygon (X)\cr
#'   node \tab centroidY \tab ctrY \tab Centroid of the polygon (Y)\cr
#'   link \tab e1 \tab e1 \tab Id of first patch at end of link\cr
#'   link \tab e2 \tab e2 \tab Id of second patch at end of link\cr
#'   link \tab maxWeight \tab maxWt \tab The maximum weight of all links connecting patches between polygons\cr
#'   link \tab linkIdMaxWeight \tab maxWt \tab The link id of that maximum weight link (lcpLinkId)\cr
#'   link \tab minWeight \tab min \tab The minimum weight of all links connecting patches between polygons\cr
#'   link \tab linkIdMinWeight \tab minWt \tab The link id of that minimum weight link (lcpLinkId)\cr
#'   link \tab medianWeight \tab medWt \tab The median weight of all links connecting patches between polygons\cr
#'   link \tab meanWeight \tab meanWT \tab The minimum weight of all links connecting patches between polygons\cr
#'   link \tab numlinksWeight \tab numEWt \tab The number of links connecting patches between polygons\cr
#'   link \tab eucCentroidWeight \tab eucCtrWt \tab The Euclidean distance between centroids of polygons\cr
#' }
#'
#' @author Paul Galpern and Alex Chubaty
#' @docType methods
#' @export
#' @importFrom raster writeRaster
#' @importFrom rgdal writeOGR
#' @include classes.R
#' @rdname export
#' @seealso \code{\link{MPG}}, \code{\link{GOC}}, \code{\link{grain}}
#' @examples
#' \dontrun{
#' library(raster)
#'
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Export rasters and vectors of the MPG to the current working directory
#' export(tinyPatchMPG)
#'
#' ## Export raster and vectors of a grain to a specified directory
#' export(grain(tinyPatchGOC, 2), dirname = 'tiny_goc_thresh2')
#' }
#'
setGeneric("export", function(x, dirname=NULL, path=".", rasterFormat="GTiff",
                              overwrite=FALSE, R=FALSE, vorBound=FALSE, ...) {
  standardGeneric("export")
})

.createDir  <- function(xType, dirname, path, overwrite) {

  if (is.null(dirname)) {
    dirname <- paste0(xType, "_", format(Sys.time(), "%d%h%y%_%H%M%S"))
  }

  newdir <- paste0(gsub("\\\\", "/", paste0(gsub("\\\\$|/$", "", path), "/")), dirname)

  if ((dir.exists(newdir)) && (!overwrite)) {
    stop(paste0("grainscape: directory ", newdir, " already exists. Use overwrite=TRUE."), call.=FALSE)
  }

  dir.create(newdir)
  return(newdir)
}

.wRas <- function(ras, fname, dirpath, rasterFormat, overwrite) {
  extensions <- data.frame(format=c("raster", "ascii", "SAGA", "IDRISI",
                                    "CDF", "GTiff", "ENVI", "EHdr", "HFA"),
                           ext=c("grd", "asc", "sdat", "rst",
                                 "nc", "tif", "envi", "bil", "img"))
  writeRaster(ras, filename=paste0(dirpath, "/", fname, ".",
                extensions[extensions$format==rasterFormat, "ext"]),
              format=rasterFormat, overwrite=overwrite)
}

.wShp  <- function(sp, fname, dirpath, overwrite) {
  writeOGR(sp, dsn=dirpath, layer=fname, driver="ESRI Shapefile",
           overwrite_layer=overwrite)
}

#' @export
#' @rdname export
setMethod(
  "export",
  signature = "mpg",
  definition = function(x, dirname=NULL, path=".", rasterFormat="GTiff",
                        overwrite=FALSE, R=FALSE, vorBound=FALSE, ...) {


    if (!R) {
      dirpath <- .createDir("mpg", dirname, path, overwrite)
    }

    ## Prepare links
    linksDF <- graphdf(x)[[1]]$e
    names(linksDF) <- c("e1", "e2", "linkId", "lcpPerWt", "strtPerX", "strtPerY", "endPerX", "endPerY" )
    nodesDF <- graphdf(x)[[1]]$v[, -1]
    names(nodesDF) <- c("patchId", "patchA", "patchEA", "coreA", "ctrX", "ctrY")

    firstCentr <- nodesDF[match(linksDF$e1, nodesDF$patchId), c("ctrX", "ctrY")]
    names(firstCentr) <- c("strtCtrX", "strtCtrY")
    secondCentr <- nodesDF[match(linksDF$e2, nodesDF$patchId), c("ctrX", "ctrY")]
    names(secondCentr) <- c("endCtrX", "endCtrY")
    linksCentr <- cbind(linksDF, firstCentr, secondCentr)
    row.names(linksCentr) <- linksDF$linkId
    linksCentrSP <- SpatialLinesDataFrame(
                      SpatialLines(
                        apply(linksCentr[, c("strtCtrX", "strtCtrY", "endCtrX", "endCtrY", "linkId")], 1,
                        function(x)
                          Lines(Line(matrix(x[1:4], 2, 2, byrow=TRUE)),
                            ID=as.character(x[5])))), data=linksCentr)
    projection(linksCentrSP) <- CRS(projection(x@patchId))

    firstPerim <- linksDF[, c("strtPerX", "strtPerY")]
    secondPerim <- linksDF[, c("endPerX", "endPerY")]
    linksPerim <- cbind(linksDF, firstPerim, secondPerim)
    row.names(linksPerim) <- linksDF$linkId
    linksPerimSP <- SpatialLinesDataFrame(
                      SpatialLines(
                        apply(linksPerim[, c("strtPerX", "strtPerY", "endPerX", "endPerY", "linkId")], 1,
                        function(x)
                          Lines(Line(matrix(x[1:4], 2, 2, byrow=TRUE)),
                            ID=as.character(x[5])))),
                      data=linksPerim[,-which(duplicated(names(linksPerim)))])
    proj4string(linksPerimSP) <- CRS(projection(x@patchId))

    ## Create voronoi boundaries
    if (vorBound) {
      cat("Extracting voronoi boundaries\n")
      vorB <- boundaries(x@voronoi, class=TRUE, asNA=TRUE)
    }
    else {
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

      cat("Exported to:", normalizePath(dirpath), "\n")
      invisible(normalizePath(dirpath))
    }
    else {
      returnSpatial <- list(
        nodes=nodesSP,
        linksCentroid=linksCentrSP,
        linksPerim=linksPerimSP,
        patchId=x@patchId,
        voronoi=x@voronoi,
        lcpPerimWeight=x@lcpPerimWeight,
        lcpLinkId=x@lcpLinkId,
        vorBound=vorB
      )

      return(returnSpatial)
    }

})

#' @export
#' @rdname export
setMethod(
  "export",
  signature = "grain",
  definition = function(x, dirname=NULL, path=".", rasterFormat="GTiff",
                        overwrite=FALSE, R=FALSE, vorBound=FALSE, ...) {

    if (!R) {
      dirpath <- .createDir("grain", dirname, path, overwrite)
    }

    ## Prepare links
    linksDF <- graphdf(x@th)[[1]]$e[, -10]
    names(linksDF) <- c("e1", "e2", "maxWt", "lidMaxWt", "minWt", "lidMinWt", "medWt", "meanWt", "numEWt", "eucCtrWt" )
    nodesDF <- graphdf(x@th)[[1]]$v[, -c(1, 9)]
    names(nodesDF) <- c("polyId", "ctrX", "ctrY", "polyA", "patchA", "patchEA", "coreA")

    firstCentr <- nodesDF[match(linksDF$e1, nodesDF$polyId), c("ctrX", "ctrY")]
    names(firstCentr) <- c("strtCtrX", "strtCtrY")
    secondCentr <- nodesDF[match(linksDF$e2, nodesDF$polyId), c("ctrX", "ctrY")]
    names(secondCentr) <- c("endCtrX", "endCtrY")
    linksCentr <- cbind(linksDF, firstCentr, secondCentr)
    row.names(linksCentr) <- 1:nrow(linksCentr)
    linksCentr$plinkId <- 1:nrow(linksCentr)
    linksCentrSP <- SpatialLinesDataFrame(
      SpatialLines(
        apply(linksCentr[, c("strtCtrX", "strtCtrY", "endCtrX", "endCtrY", "plinkId")], 1,
              function(x)
                Lines(Line(matrix(x[1:4], 2, 2, byrow=TRUE)),
                      ID=as.character(x[5])))), data=linksCentr)
    projection(linksCentrSP) <- CRS(projection(x@voronoi))

    ## Create voronoi boundaries
    if (vorBound) {
      cat("Extracting voronoi boundaries\n")
      vorB <- boundaries(x@voronoi, class=TRUE, asNA=TRUE)
    }
    else {
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

      cat("Exported to:", normalizePath(dirpath), "\n")
      invisible(normalizePath(dirpath))
    }
    else {
      returnSpatial <- list(
        nodes=nodesSP,
        linksCentroid=linksCentrSP,
        voronoi=x@voronoi,
        vorBound=vorB
      )

      return(returnSpatial)
    }
  })

setMethod(
  "export",
  signature = "goc",
  definition = function(x, dirname=NULL, path=".",
                        overwrite=FALSE, R=FALSE, vorBound=FALSE, ...) {

    cat("Use grain() to extract a single grain of connectivity to export.\n")
  })

