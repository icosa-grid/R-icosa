
#' Global Triangular and Hexa-Pentagonal Grids Based on Tessellated Icosahedra
#' 
#' The \bold{icosa} package provides tools to aggregate and analyze geographic data
#' using grids based on tessellated icosahedra. The procedures can be set to provide a grid with a
#'    custom resolution. Both the primary triangular and their inverted penta-
#'   hexagonal grids are available for implementation. Additional functions
#'    are provided to position points (latitude-longitude data) on the grids,
#'    to allow 2D and 3D plotting, use raster data and shapefiles.
#' 
#' This is the 0.9 (Beta) version. Notes about found bugs and suggestions are more than welcome!
#'
#' @author Adam T. Kocsis (adam.kocsis@outlook.com)
#' @docType package
#' @examples
#' # Create a triangular grid
#' tri <- trigrid(c(2,2))
#' @name icosa
#' @useDynLib icosa
#' @importFrom Rcpp sourceCpp
#' @importFrom rgl plot3d
#' @importFrom sp coordinates
#' @importFrom rgl lines3d
#' @importFrom raster subset
#' @importFrom raster rotate
#' @importFrom raster values
NULL

#the objects ---------
