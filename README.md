# icosa
R package for global triangular and penta-gexagonal grids based on tessellated icosahedra (v0.9.82)

## News

I initilized the developmental repository of the `icosa` package, which has been under continuous testing in the past year. I have not been attending to this project,because I was too occupied with research and the divDyn package. 

## About

This repository contains the under development versions of the package as well as those that were uploaded to the CRAN.


## Installation

1. The latest stable version of the package (v0.9.81) is openly available from the CRAN servers. Just type in
```
install.packages("icosa")
```

and the package will be installed. In case you would like to experiment with the under development versions, please take a look at the points below

2. If you have a compiler installed, you can install the latest version from the source tarball, with
```
install.packages(
  "https://github.com/adamkocsis/icosa/raw/master/_archive/source/icosa.0.9.82.tar.gz", 
  repos=NULL, type="source")
```
3. I will also provide binaries for Windows. If you want to, you can try them. 
`install.packages("https://github.com/adamkocsis/icosa/raw/master/_bin/Win_x64_x86/icosa_0.9.82.zip", repos=NULL)`



# Change log

## [0.9.82] - 2018-09-27
Rebuilt from previous version .

### Changed
- vignette strcutre update
- vignette now uses z1 OpenStreetMap land polygons instead of z3
- the package no longer requires the 'rgdal' package, which is only used for projection changes

### Fixed
- added compatibility for up-to-date Rcpp versions
- explicit self-assignment compilation warning with clang 7.0.0

### Known issues
- warnings produced by sp::Polygons function - will be corrected shortly


## [0.9.81] - 2017-04-18
### Fixed
- memory deallocation issues
- SpPolygons(): zenith/nadir face issue
- unnecessary 'rgdal' namespace import

## [0.9.80] (first Beta) - 2017-04-17
### Added
- the locate() function was updated to version 6.0, now incorporating the 'randomborders' argument
- the occupied() function was rebuilt on the locate() function
- the centers() shorthand was added to the quick extraction of the facecenters
- major work on the help files
- the value replacement method of the gridlayer was extended to host lat/long indicators
- added logical subsetting to the facelayer
- bugfix for the x86 application of SpPolygons()
 
## [0.8.61] - 2017-04-09
### Added
- igraph is added as related content 
- the function gridgraph() is implmented for 'trigrid' and for 'facelayer' classes
- fixed bug in OccupiedFaces() for SpatialPolygons
- the @graph slot has been added to the 'trigrid' class to host 'igraph' class graph representations
- package namespace is cleaned up to include only importing namespaces, only the rgl package is a dependency
- the newgraph() function is added to the package
- added an Rcpp function to interpolate 3d lines
- the gridgraph() function is added for the 'hexagrid' class, and the class constructor is upgraded to include it
- the vicinity() function replaced the neighbours() function
- the class of the values is now printed to the console, when the show method of the facelayer is called
- the tesselation and class of the source grid is now added to the gridlayer class, along with a function that checks the linked grid's compatibility to
the facelayer
- the translate() function is added to the package for fast reposition of translated grids to the origin
- the chullsphere() and surfacecentroid() functions were added to the packages
- resampling methods were added for the 'hexagrid'
- igraph representation was added to the vignette


## [0.8.16] - 2017-02-17
### Added
- the shapes() function is added to the package. It will calculate a value that is proportional to the irregularities of the triangular faces or subfaces.
- character values in facelayers will be plotted with random colors
- rgdal dependecy of spTransorm() was properly resolved
- the missing belts slot of the hexagrid class was filled 
- fixed missing invalid input message for PolToCar() function.
- grid resolution is now displayed with the show() method.
- the group generics Ops, Math, and Summary were implemented for the facelayer.
- basic functions of the 'stats' packages were implemented
- latitude and longitude-wise selection was implemented to the facelayer subsetting method
- resampling of 'facelayer' objects to based on 'trigrid' classes was applied in the program: downscaling
- added 2d plotting scheme for 'facelayers', where the values of the facelayer are exclusively colours

### Deleted
- the deprecated argument "border" is no longer available for the user interface of locate()

## [0.8.0] (Alpha) - 2016-11-25
### Added
- Most gridding features are functional and should be usable, I consider the package to be ready for alpha testing.

### Notes
Pre-alpha versions were not registered.
