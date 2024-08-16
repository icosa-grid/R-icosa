
# icosa <img src="man/figures/logo.png" align="right" />

##### Triangular and Penta-Hexagonal Grids Based on Tessellated Icosahedra

[![](https://img.shields.io/badge/devel%20version-0.11.1-green.svg)](https://github.com/adamkocsis/icosa)
[![](https://www.r-pkg.org/badges/version/icosa?color=blue)](https://cran.r-project.org/package=icosa)
[![](http://cranlogs.r-pkg.org/badges/grand-total/icosa?color=yellow)](https://cran.r-project.org/package=icosa)
[![CRAN
checks](https://badges.cranchecks.info/summary/icosa.svg)](https://cran.r-project.org/web/checks/check_results_icosa.html)
[![](https://img.shields.io/badge/doi-10.5281/zenodo.7798915-blue.svg)](https://doi.org/10.5281/zenodo.7798915)

*icosa* (originally εἴκοσι for twenty) refers to the icosahedron, the
platonic solid with the highest number of faces. The ‘icosa’ extension
is an implementation of icosahedral grids in three dimensions. The
spherical-triangular tessellation can be set to create grids with custom
resolutions. Both the primary triangular and their inverted
penta-hexagonal grids can be calculated. Additional functions are
provided that allow plotting of the grids and associated data, the
interaction of the grids with other raster and vector objects, and
treating the grids as a graphs.

These grids are often called icospheres, and have an important use in
material design, architecture, computer graphics and chemistry.

------------------------------------------------------------------------

### Similar packages

Implementations of similar grids are available in R with the [H3
library](https://h3geo.org/)
([h3-js](https://obrl-soil.github.io/h3jsr/)) and the
[ddgridR](https://github.com/r-barnes/dggridR) libraries. These other
implementations are based on hierarchical organizations of grid cells,
which allows very high-resolution. In contrast, the icosa package was
optimized for coarser resolutions, providing a more gradual change of
cells sizes, 3D-based operations (e.g. grid rotation), and to be used in
the R environment for spatially isotropic analysis - especially when it
comes to latitudinal patterns. It also allows access to not only
hexagonal, but triangular grids as well.

------------------------------------------------------------------------

### Plans

#### Near

- Finishing vignettes in **Tutorials**
- Paper about the package (long overdue)
- Moving internal vector-representation from `sp` to `sf`

#### Distant

- Customization of tesselation that allows tweaking of cell sizes and
  shapes - depending on needs
- Writing a C++ library from the core functionality and porting it to
  Python and Julia
