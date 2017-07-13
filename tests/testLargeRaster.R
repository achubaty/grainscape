## This is a test of running a very large raster
## on GRAINSCAPE
library(grainscape)
library(raster)

## Grab a very large land cover raster 
## for experimental purposes

url <- 'ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip'
file <- file.path(tempdir(), basename(url))

download.file(url, destfile = file)

unzip(zipfile = file, files = "LCC2005_V1_4a.tif", exdir = dirname(file))

f <- file.path(dirname(file), 'LCC2005_V1_4a.tif')
lc <- raster(f)

