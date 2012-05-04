\name{gsMPG}
\alias{gsMPG}
\title{
Extract a minimum planar graph (MPG) model from a landscape resistance surface (using SELES)
}
\description{
This function is used to extract a minimum planar graph (MPG) and it is also the
first step in grains of connectivity (GOC) modelling.  Both patch-based and lattice MPGs can be extracted.
The function calls \code{SELES}, a Windows-based executable distributed with the package, to extract the graph.
}
\usage{
gsMPG(cost, patch, sa = NULL, outputFolder = NULL, filterPatch = NULL,
    spreadFactor = 0, selesPath = system.file("SELES", package = "grainscape"))
}
\arguments{
  \item{cost}{
  A raster of class \code{RasterLayer} giving a landscape resistance surface, where the values of
  each raster cell are proportional to the resistance to movement, dispersal, or gene flow
  for an organism in the landscape feature they represent.  Missing values \code{NA} are acceptable (but see below).
  Negative values are not.  To extract an MPG with Euclidean links (i.e. and not least-cost path links) set
  \code{cost[] <- 1}.
  
}
  \item{patch}{
  A raster of class \code{RasterLayer} for a patch-based analysis OR an integer for a lattice analysis.  If a raster
  is given it must be of the same extent, origin and projection as \code{cost} and be binary, without missing values,
  where patches=1 and non-patches=0.  For lattice analyses, an integer gives the spacing in raster cells between focal points in the lattice.
}
  \item{sa}{
  Optional.  A raster of class \code{RasterLayer} of the same extent, origin and projection as \code{cost}
  indicating the study area (i.e. cells on the landscape to include in the analysis).
  If not supplied \code{sa} is the full extent of \code{cost}.  To mask out areas of the landscape to exclude
  from analysis (e.g. at the edges of a map), supply a binary raster where included cells=1 and excluded cells=0.
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
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology.  In press.\cr
}
\author{
Paul Galpern (\email{pgalpern@gmail.com}), Andrew Fall
}

\note{
SELES has been compiled for Windows.  Therefore use of \code{gsMPG} is limited to Windows-based platforms.\cr

Researchers should consider whether the use of a patch-based
MPG or a lattice MPG model is appropriate based on the patch-dependency of the organism under study.  Patch-based models make
most sense when animals are restricted to, or dependent on, a resource patch.  Lattice models can be used as a generalized
and functional approach to scaling resistance surfaces.\cr

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

## Load raster landscape
tiny <- raster(system.file("extdata/tiny.asc", package="grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- reclass(tiny, rcl=cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1)

## Explore the graph structure and link attributes
print(tinyPatchMPG$mpg, edge=TRUE)

## Explore the patch/node attributes
print(tinyPatchMPG$mpg, vertex=TRUE)

## Find the mean patch area (see igraph manual for use of V() and E())
mean(V(tinyPatchMPG$mpg)$patchArea)

## Quick visualization of the MPG
plot(tinyPatchMPG$mpgPlot, col=c("grey", "black"), legend=FALSE)

## Visualize the minimum spanning tree of the MPG
plot(tinyPatchMPG$patchId, col="black", legend=FALSE)
plot(tinyPatchMPG$lcpPerimType %in% c(1,2), add=TRUE, legend=FALSE, col=c(NA, "grey"))

## Additional graph extraction scenarios
## Produce a lattice MPG where focal points are spaced 10 cells apart
tinyLatticeMPG <- gsMPG(cost=tinyCost, patch=10)

## Produce a patch-based MPG with a study area consisting of half of the map
tinySa <- tinyCost
tinySa[] <- 1
tinySa[1:5000] <- 0
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1, sa=tinySa)

}
}