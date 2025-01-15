#' Abbreviate names
#'
#' Manually specify field names for writing to shapefile instead of relying on
#' `sf:::abbreviate_shapefile_names()` (which uses `abbreviate()`) during save.
#' Names must contain fewer than 10 characters.
#'
#' @author Alex Chubaty
#' @keywords internal
.abbrev <- function(x) {
  names_linksDF <- c(
    "e1", "e2", "linkId", "lcpPerimWeight", "startPerimX", "startPerimY", "endPerimX", "endPerimY"
  )
  names_nodesDF <- c(
    "patchId", "patchArea", "patchEdgeArea", "coreArea", "centroidX", "centroidY"
  )

  names_gs <- c(names_linksDF, names_nodesDF) |>
    unique() |>
    sort()

  ## abbreviate other (e.g., user-added) fields
  names_usr_ids <- which(!(x %in% names_gs))
  names_usr <- x[names_usr_ids]
  if (length(names_usr) > 0) {
    for (i in names_usr_ids) {
      x <- gsub(x[i], abbreviate(x[i], minlength = 7L), x)
    }
  }

  ## abbreviate **our** fields (`names_gs`)
  ## `e1`, `e2`, `linkId`, `patchId` are already short, so don't abbrev
  x <- gsub("centroid", "ctr", x)
  x <- gsub("coreArea", "coreA", x)
  x <- gsub("endPerim", "endPer", x)
  x <- gsub("lcpPerimWeight", "lcpPerWt", x)
  x <- gsub("patchArea", "patchA", x)
  x <- gsub("patchEdgeArea", "patchEA", x)
  x <- gsub("startPerim", "strtPer", x)

  return(x)
}

#' @author Paul Galpern
#' @keywords internal
.createDir <- function(xType, dirname, path, overwrite) {
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
#' @importFrom sf st_as_sf st_write
#' @keywords internal
.wShp <- function(sp, fname, dirpath, overwrite) {
  st_write(st_as_sf(sp),
    dsn = dirpath, layer = fname, driver = "ESRI Shapefile",
    delete_layer = overwrite
  )
}

#' Export spatial data from MPG and GOC models
#'
#' @description
#' This function automates the export of raster and vector spatial data from `mpg` and
#' `grain` objects. By default it places them in a new directory, unless an existing one is
#' specified with `overwrite = TRUE`.
#'
#' It can also be used to process `mpg` and `grain` objects to
#' produce R spatial objects that are convenient for plotting or analysis within R.
#' Use `R = TRUE` in which case all parameters related to file export
#' are ignored. (Default `R = FALSE`)
#'
#' The [raster::writeRaster()] function is used for rasters,
#' and [sf::st_write()] is used to export ESRI compatible shape files.
#'
#' @param x             A `mpg` or `grain` object
#'
#' @param dirname       The name of a new directory to create. If `NULL`
#'                      a directory with a name containing the date and time will be created.
#'
#' @param path          A path to where this new directory `dirname` should be created.
#'                      Defaults to the working directory.
#'
#' @param rasterFormat  The format for exported rasters. See [raster::writeFormats()] for options.
#'                      Defaults to GeoTiff (`rasterFormat='GTiff'`).
#'                      Use `rasterFormat='raster'` to save `.grd` files in
#'                      native \pkg{raster} package format.
#'
#' @param overwrite     If directory already exists will overwrite existing files inside.
#'                      Defaults to `FALSE`.
#'
#' @param R             If `TRUE`, return the spatial objects that would be written to files.
#'                      Do not write these files and ignore `dirname`, `path`,
#'                      `rasterFormat`, `overwrite` parameters.
#'                      This is useful for visualization using R plotting functions,
#'                      or spatial analysis within R. Defaults to `FALSE`
#'
#' @param vorBound      Specify whether to create a raster with the boundaries of
#'                      the Voronoi polygons `=1` and the remainder `=NA`.
#'                      This may be useful for visualizing relationships among
#'                      polygons in a grain of connectivity.
#'                      This can add time to the export on very large rasters.
#'                      Defaults to `FALSE`.
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
#' **Exported from `mpg` objects:**
#'
#' `nodes`, `linksCentroid`, `linksPerim` are shape files giving
#' the locations of the patch centroids, links among centroids, and links
#' among perimeters of patches respectively. `patchId, voronoi` are
#' rasters giving the patch identifier of the patch, or of the patch that
#' the Voronoi polygon refers to. `lcpPerimWeight, lcpLinkId` give
#' the weight in cost surface units of the shortest paths between perimeters,
#' and the identifiers of those links respectively. `vorBound` gives
#' the boundaries of the Voronoi polygons (if specified).
#'
#' Description of node (vertex) and link (edge) weights in `mpg` objects
#' and their corresponding attribute names in the shape files created.
#'
#' \tabular{llll}{
#'   **type** \tab **MPG name** \tab **SHP name** \tab **Description**\cr
#'   node \tab `patchId` \tab `patchId` \tab Patch ID from `patchId` raster \cr
#'   node \tab `patchArea` \tab `patchA` \tab Area of patch \cr
#'   node \tab `patchEdgeArea` \tab `patchEA` \tab Edge area of patch \cr
#'   node \tab `coreArea` \tab `coreA` \tab Area excluding edge of patch \cr
#'   node \tab `centroidX` \tab `ctrX` \tab Centroid of the patch (X)\cr
#'   node \tab `centroidY` \tab `ctrY` \tab Centroid of the patch (Y)\cr
#'   link \tab `e1` \tab `e1` \tab Id of first patch at end of link\cr
#'   link \tab `e2` \tab `e2` \tab Id of second patch at end of link\cr
#'   link \tab `linkId` \tab `linkId` \tab Link ID from `lcpLinkId` raster\cr
#'   link \tab `lcPerimWeight` \tab `lcpPerWt` \tab Cost length of link from patch perimeters\cr
#'   link \tab `startPerimX` \tab `strtPerX` \tab Coordinate of link endpoint on first patch (X)\cr
#'   link \tab `startPerimY` \tab `strtPerY` \tab Coordinate of link endpoint on first patch (Y)\cr
#'   link \tab `endPerimX` \tab `endPerX` \tab Coordinate of link endpoint on second patch (X)\cr
#'   link \tab `endPerimY` \tab `endPerY` \tab Coordinate of link endpoint on second patch (Y)\cr
#' }
#'
#' **Exported from `grain` objects:**
#'
#' `nodes, linksCentroid` are shape files giving the locations of the
#' Voronoi polygon centroids and links among them respectively.
#' `voronoi` are rasters gives the polygon identifier of each cluster of patches.
#' `vorBound` gives the boundaries of the Voronoi polygons (if specified).
#'
#' Description of node (vertex) and link (edge) weights in `grain`
#' objects  and their corresponding attribute names in the shape files created.
#'
#' \tabular{llll}{
#'   **Type** \tab **GOC name** \tab **SHP name** \tab **Description**\cr
#'   node \tab `polygonId` \tab `polyId` \tab Polygon ID from grain voronoi raster \cr
#'   node \tab `polygonArea` \tab `polyA` \tab Area of polygon from grain voronoi raster \cr
#'   node \tab `totalPatchArea` \tab `patchA` \tab Total area of all patches in polygon\cr
#'   node \tab `totalPatchEdgeArea` \tab `patchEA` \tab Total area of all patch edges in polygon\cr
#'   node \tab `totalCoreArea` \tab `coreA` \tab Total area of patches in polygon excluding edges\cr
#'   node \tab `centroidX` \tab `ctrX` \tab Centroid of the polygon (X)\cr
#'   node \tab `centroidY` \tab `ctrY` \tab Centroid of the polygon (Y)\cr
#'   link \tab `e1` \tab `e1` \tab ID of first patch at end of link\cr
#'   link \tab `e2` \tab `e2` \tab ID of second patch at end of link\cr
#'   link \tab `maxWeight` \tab `maxWt` \tab The maximum weight of all links connecting
#'                                                     patches between polygons\cr
#'   link \tab `linkIdMaxWeight` \tab `maxWt` \tab The link id of that maximum weight
#'                                                           `link (lcpLinkId)`\cr
#'   link \tab `minWeight` \tab `min` \tab The minimum weight of all links connecting
#'                                                   patches between polygons\cr
#'   link \tab `linkIdMinWeight` \tab `minWt` \tab The link id of that minimum weight
#'                                                           `link (lcpLinkId)`\cr
#'   link \tab `medianWeight` \tab `medWt` \tab The median weight of all links connecting
#'                                                        patches between polygons\cr
#'   link \tab `meanWeight` \tab `meanWT` \tab The minimum weight of all links connecting
#'                                                       patches between polygons\cr
#'   link \tab `numlinksWeight` \tab `numEWt` \tab The number of links connecting patches
#'                                                           between polygons\cr
#'   link \tab `eucCentroidWeight` \tab `eucCtrWt` \tab The Euclidean distance between
#'                                                                centroids of polygons\cr
#' }
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom raster boundaries projection projection<- writeRaster
#' @importFrom sf st_write
#' @importFrom sp coordinates<- CRS Line Lines proj4string proj4string<-
#' @importFrom sp SpatialLines SpatialLinesDataFrame
#' @include classes.R
#' @rdname export
#' @seealso [MPG()], [GOC()], [grain()]
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
  }
)

#' @export
#' @rdname export
setMethod("export",
  signature = "mpg",
  definition = function(x, dirname = NULL, path = ".", rasterFormat = "GTiff",
                        overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    if (!R) {
      dirpath <- .createDir("mpg", dirname, path, overwrite)
    }

    ## Prepare links
    linksDF <- graphdf(x)[[1]]$e
    names(linksDF) <- names(linksDF) |> .abbrev()
    nodesDF <- graphdf(x)[[1]]$v[, -1]
    names(nodesDF) <- names(nodesDF) |> .abbrev()

    firstCentr <- nodesDF[match(linksDF$e1, nodesDF$patchId), c("ctrX", "ctrY")]
    names(firstCentr) <- c("strtCtrX", "strtCtrY")
    secondCentr <- nodesDF[match(linksDF$e2, nodesDF$patchId), c("ctrX", "ctrY")]
    names(secondCentr) <- c("endCtrX", "endCtrY")
    linksCentr <- cbind(linksDF, firstCentr, secondCentr)
    row.names(linksCentr) <- linksDF$linkId
    linksCentrSP <- linksCentr[, c("strtCtrX", "strtCtrY", "endCtrX", "endCtrY", "linkId")] |>
      apply(1, function(x) {
        Lines(Line(matrix(x[1:4], 2, 2, byrow = TRUE)), ID = as.character(x[5]))
      }) |>
      SpatialLines() |>
      SpatialLinesDataFrame(data = linksCentr)
    projection(linksCentrSP) <- CRS(projection(x@patchId))

    firstPerim <- linksDF[, c("strtPerX", "strtPerY")]
    secondPerim <- linksDF[, c("endPerX", "endPerY")]
    linksPerim <- cbind(linksDF, firstPerim, secondPerim)
    row.names(linksPerim) <- linksDF$linkId
    linksPerimSP <- linksPerim[, c("strtPerX", "strtPerY", "endPerX", "endPerY", "linkId")] |>
      apply(1, function(x) {
        Lines(Line(matrix(x[1:4], 2, 2, byrow = TRUE)), ID = as.character(x[5]))
      }) |>
      SpatialLines() |>
      SpatialLinesDataFrame(data = linksPerim[, -which(duplicated(names(linksPerim)))])
    proj4string(linksPerimSP) <- CRS(projection(x@patchId))

    ## Create voronoi boundaries
    if (vorBound) {
      message("Extracting voronoi boundaries...")
      vorB <- raster::boundaries(x@voronoi, classes = TRUE, asNA = TRUE)
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
  }
)

#' @export
#' @rdname export
setMethod("export",
  signature = "grain",
  definition = function(x, dirname = NULL, path = ".", rasterFormat = "GTiff",
                        overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    if (!R) {
      dirpath <- .createDir("grain", dirname, path, overwrite)
    }

    ## Prepare links
    linksDF <- graphdf(x@th)[[1]]$e[, -10]
    names(linksDF) <- c(
      "e1", "e2", "maxWt", "lidMaxWt", "minWt", "lidMinWt",
      "medWt", "meanWt", "numEWt", "eucCtrWt"
    )
    nodesDF <- graphdf(x@th)[[1]]$v[, -c(1, 9)]
    names(nodesDF) <- c("polyId", "ctrX", "ctrY", "polyA", "patchA", "patchEA", "coreA")

    firstCentr <- nodesDF[match(linksDF$e1, nodesDF$polyId), c("ctrX", "ctrY")]
    names(firstCentr) <- c("strtCtrX", "strtCtrY")
    secondCentr <- nodesDF[match(linksDF$e2, nodesDF$polyId), c("ctrX", "ctrY")]
    names(secondCentr) <- c("endCtrX", "endCtrY")
    linksCentr <- cbind(linksDF, firstCentr, secondCentr)
    row.names(linksCentr) <- seq_len(nrow(linksCentr))
    linksCentr$plinkId <- seq_len(nrow(linksCentr))
    linksCentrSP <- linksCentr[, c("strtCtrX", "strtCtrY", "endCtrX", "endCtrY", "plinkId")] |>
      apply(1, function(x) {
        Lines(Line(matrix(x[1:4], 2, 2, byrow = TRUE)), ID = as.character(x[5]))
      }) |>
      SpatialLines() |>
      SpatialLinesDataFrame(data = linksCentr)
    projection(linksCentrSP) <- CRS(projection(x@voronoi))

    ## Create voronoi boundaries
    if (vorBound) {
      message("Extracting voronoi boundaries...")
      vorB <- raster::boundaries(x@voronoi, classes = TRUE, asNA = TRUE)
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
  }
)

#' @export
#' @rdname export
setMethod("export",
  signature = "goc",
  definition = function(x, dirname = NULL, path = ".",
                        overwrite = FALSE, R = FALSE, vorBound = FALSE, ...) { # nolint
    message("Use grain() to extract a single grain of connectivity to export.")
    return(invisible())
  }
)
