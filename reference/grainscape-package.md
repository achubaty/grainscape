# Landscape Connectivity, Habitat, and Protected Area Networks

Given a landscape resistance surface, creates minimum planar graph and
grains of connectivity models that can be used to calculate effective
distances for landscape connectivity at multiple scales.

## Details

Landscape connectivity modelling to understand the movement and
dispersal of organisms has been done using raster resistance surfaces
and landscape graph methods. Grains of connectivity (GOC) models combine
elements of both approaches to produce a continuous and scalable tool
that can be applied in a variety of study systems. The purpose of this
package is to implement grains of connectivity analyses. Routines accept
raster-based resistance surfaces as input and return raster, vector and
graph-based data structures to represent connectivity at multiple
scales. Effective distances describing connectivity between geographic
locations can be determined at multiple scales. Analyses of this sort
can contribute to corridor identification, landscape genetics, as well
as other connectivity assessments. Minimum planar graph (MPG; Fall *et
al.*, 2007) models of resource patches on landscapes can also be
generated using the software.

MPG calculations and generalization of the Voronoi tessellation used in
GOC models is based on the routines in SELES software (Fall and Fall,
2001). Routines depend on the terra (Hijmans, 2023), sf (Pebesma, 2018),
and igraph (Csardi and Nepusz, 2006) packages.

A paper describing the use of this package for landscape connectivity
modelling is available at
[doi:10.1111/2041-210X.13350](https://doi.org/10.1111/2041-210X.13350) .

A detailed tutorial is available as a vignette (see
`browseVignettes('grainscape')`).

## References

Csardi, G. and T. Nepusz. (2006). The igraph software package for
complex network research. InterJournal Complex Systems 1695.
<https://igraph.org>.

Fall, A. and J. Fall. (2001). A domain-specific language for models of
landscape dynamics. Ecological Modelling 141:1-18.

Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs:
Principles and applications for habitat connectivity. Ecosystems
10:448:461.

Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing
methods for scaling resistance surfaces. Landscape Ecology 28:1269-1291.

Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape
connectivity on animal distribution: a functional grain approach.
Ecography 36:1004-1016.

Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape
connectivity: A guide to construction, analysis and application for
conservation. Biological Conservation 144:44-55.

Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity:
analysis at multiple spatial scales in landscape genetics. Molecular
Ecology 21:3996-4009.

Hijmans, R.J. (2023). terra: Spatial Data Analysis. R package,
<https://CRAN.R-project.org/package=terra>.

Pebesma, E.J. (2018). Simple features for R: Standardized support for
spatial vector data. The R Journal 10:1, 439-446,
[doi:10.32614/RJ-2018-009](https://doi.org/10.32614/RJ-2018-009) .

## See also

Useful links:

- <https://www.alexchubaty.com/grainscape/>

- <https://github.com/achubaty/grainscape>

- Report bugs at <https://github.com/achubaty/grainscape/issues>

## Author

**Maintainer**: Alex M Chubaty <achubaty@for-cast.ca>
([ORCID](https://orcid.org/0000-0001-7146-8135))

Authors:

- Alex M Chubaty <achubaty@for-cast.ca>
  ([ORCID](https://orcid.org/0000-0001-7146-8135))

- Paul Galpern <pgalpern@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-0099-3981)) \[copyright holder\]

- Sam Doctolero <sam.doctolero@gmail.com>
