\name{gsMPGstitch}
\alias{gsMPGstitch}
\title{
Extract a minimum planar graph (MPG) model in strips from a landscape resistance surface (using SELES)
}
\description{
This function is used to extract a minimum planar graph (MPG) in strips, and can be used as an alternative
to \code{\link{gsMPG}}.  It can save time where there are a large number of patches by dividing
the landscape into a number of strips and stitching them back together.  It can also extract MPGs for each strip in parallel (with the \code{\link{parallel} package}).
Using this function is necessary when there are a very large number of patches (usually in excess of 2000), as SELES
and consequently \code{\link{gsMPG}} will fail on such landscapes.

Stripping and extracting separate MPGs  may sometimes result in small artefactual differences in graph topology
and Voronoi tessellations.  Researchers may wish to quantify the impact of stripping by using a
small subregion of their landscape, and comparing the results from \code{gsMPG(x)} and
\code{gsMPGstrip(x, numStrips=2, ...)}.

Note that \code{gsMPGstitch} cannot be used to generate MPGs for use in lattice-based grains of
connectivity modelling.  \code{\link{gsMPG}} must be used for this purpose.

}
\usage{
gsMPGstitch(cost, patchid, numStrips, percentOverlap,
disttype="Cost", cpu=1, outputFolder=NULL, filterPatch=NULL,
spreadFactor=0, selesPath = system.file("SELES", package = "grainscape"))
}
\arguments{
  \item{cost}{
  A raster of class \code{RasterLayer} giving a landscape resistance surface, where the values of
  each raster cell are proportional to the resistance to movement, dispersal, or gene flow
  for an organism in the landscape feature they represent.  Missing values \code{NA} are acceptable (but see below).
  Negative values are not.  To extract an MPG with Euclidean links (i.e. and not least-cost path links) set
  \code{cost[] <- 1}.
  
}
  \item{patchid}{
  PLEASE WRITE.  Note that it is different from gsMPG which requires a binary patch raster as input.
}
  \item{numStrips}{
  PLEASE WRITE
}

  \item{percentOverlap}{
  PLEASE WRITE
}

  \item{disttype}{
  PLEASE WRITE
}

  \item{cpu}{
  For parallel computation of each MPG strip.  If \code{cpu=1} serial computation is used.  If \code{cpu > 1}
  a cluster of up to this many processors will be initiated using the \code{parallel} package.  Using this feature
  is recommended if processors are available.  Typically one less processor than the available number of
  virtual cores works well (e.g. 7 on an i7 chip).
  }

  \item{outputFolder}{
  Optional.  If not supplied this function creates files for use by \code{SELES} in a temporary folder placed in
  the R working directory that is deleted following successful execution.  Another location may be specified instead.  If supplied
  the location is not deleted after the analysis completes.  This can be useful for debugging purposes.
}
  \item{filterPatch}{
  Optional.  Remove patches from the analysis that are smaller than a given number of cells.  
}
  \item{spreadFactor}{
  Optional.  Fine-grained control over the accuracy of Voronoi polygons.  To reduce accuracy and increase speed,
  set this as \code{spreadFactor=10} or \code{spreadFactor=100}.  
}
  \item{selesPath}{
  Optional.  The location of the \code{SELES} installation.  By default this is the folder in the package installation.
}
}
\details{
  Use this function to create a minimum planar graph (MPG) that can be further analyzed using \code{\link{igraph}} routines.\cr\cr
  It is also the first step in grains of connectivity (GOC) modelling.\cr
  
}
\value{
A \code{gsMPG} object, consisting of a list of objects.\cr\cr
The main elements:\cr
\code{$mpg} is the minimum planar graph as class \code{igraph}\cr
\code{$patchId} is the input \code{patch} raster with patch cells assigned to their id (\code{RasterLayer})\cr
\code{$voronoi} is the Voronoi tessellation of the patches and resistance surface (\code{RasterLayer})\cr
\code{$lcpPerimWeight} gives the paths of the links between patches and their accumulated costs (\code{RasterLayer})\cr
\code{$lcpLinkId} gives the paths of the links between patches and their id (\code{RasterLayer})\cr
\code{$lcpPerimType} gives the paths of the links between patches and their type (\code{RasterLayer}; see notes)\cr
\code{$mpgPlot} provides a quick way of visualizing the mpg (\code{RasterLayer})\cr\cr

The \code{$mpg} has useful vertex and edge attributes.  Vertex attributes give attributes of patches including patch area, the area of patch edges,  the
  core area of each patch, and the coordinates of the patch centroid.   All areal measurements are given as raster cell counts.
  Edge attributes give attributes of the graph links including
  link weights giving accumulated resistance/least-cost path distance, Euclidean distance, and the start and end coordinates of
  each link.

}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.\cr
}
\author{
Bronwyn Rayfield (\email{bronwyn.rayfield@mail.mcgill.ca}), Paul Galpern (\email{pgalpern@gmail.com}), Andrew Fall
}

\note{
SELES has been compiled for Windows.  Therefore use of \code{gsMPG} is limited to Windows-based platforms.\cr

MORE DETAILS IN HERE

Four types of links are identified in the MPG (1=Nearest neighbour; 2=Minimum spanning tree; 3=Gabriel; 4=Delaunay;)\cr

Areal measurements are given as raster cell counts.  If the raster projection is one where cell sizes are approximately
constant in area (e.g. UTM), or the raster covers a relatively small geographic extent (e.g. < 1000 km in dimension)
areal measurements will often be adequate.  Reprojection of rasters should be considered to minimize
these effects in other cases (see \code{\link{projectRaster}}).
}

\seealso{
\code{\link{gsGOC}, \link{gsThreshold}}
}
\examples{

\dontrun{

## Create a resistance surface
frag <- raster(system.file("extdata/fragmented.asc", package="grainscape"))
fragCost <- reclassify(frag, rcl=cbind(c(1,2,3,4), c(1,10,8,3)))

## Unlike with gsMPG() it is necessary to create a patchid raster
## before-hand.  This can be done with raster::clump()
fragPatchId <- raster::clump(fragCost==1, directions=8, gaps=FALSE)
fragPatchId[is.na(fragPatchId[])] <- 0

## Create an MPG graph by stitching together 3 strips
## Do this in parallel with two processor cores
fragMPGstitch <- gsMPGstitch(cost=fragCost, patchid=fragPatchId,
    numStrips=3, percentOverlap=50, outputFolder=NULL, cpu=2)

}
}