# The main testing
library(tinytest)
library(parallel)
library(icosa)

if(rgplates:::getOS()=="linux") wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/icosa")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/icosa")

# path of generic test functions
genericloc <- "icosa/tests/_methods/generic/"

# working directory
setwd(wd)

# make a cluster of 8
## cl <- parallel::makeCluster(4, outfile="")
## parallel::clusterCall(cl, source, "icosa/tests/source.R")


################################################################################
# Actual tests - based on functions and input data
# arcs
	point_arcs <- run_test_file("icosa/tests/arcs/test_arcs.R")

# arcdistmat
	point_arcdistmat <- run_test_file("icosa/tests/arcdistmat/test_arcdistmat.R")

# conversions: CarToPol and PolToCar
	point_conversions <- run_test_file("icosa/tests/conversions/test_conversions.R")

# constructor argumentation
# grids
#	grid <- run_test_dir("icosa/tests/grids")
	grid_args <- run_test_file("icosa/tests/grids/test_grid_args.R")
 	grid_faces <- run_test_file("icosa/tests/grids/test_faces.R")
 	grid_guide_lookup <- run_test_file("icosa/tests/grids/test_guide_lookup.R")
 	grid_rotate <- run_test_file("icosa/tests/grids/test_rotate.R")
 	grid_spacing <- run_test_file("icosa/tests/grids/test_spacing.R")
 	grid_surfacearea <- run_test_file("icosa/tests/grids/test_surfacearea.R")

# face2nb
	face2nb_faces <- run_test_file("icosa/tests/face2nb/test_face2nb_faces.R")

# facelayer constructor
	facelayer_logical <-  run_test_file("icosa/tests/facelayer/test_facelayer_logical.R")
 	facelayer_integer <-  run_test_file("icosa/tests/facelayer/test_facelayer_integer.R")
 	facelayer_numeric <-  run_test_file("icosa/tests/facelayer/test_facelayer_numeric.R")
 	facelayer_character <-  run_test_file("icosa/tests/facelayer/test_facelayer_character.R")

# grapply/gridensity
	grapply_rasters <- run_test_file("icosa/tests/grapply/test_grapply_rasters.R")
 	grapply_trigrid <- run_test_file("icosa/tests/grapply/test_grapply_trigrid.R")

## gridlabs
 	gridlabs <- run_test_file("icosa/tests/gridlabs/test_gridlabs.R")

# patches
	patches_faces <- run_test_file("icosa/tests/patches/test_patches.R")

# holes
	holes_faces <- run_test_file("icosa/tests/holes/test_holes.R")

# locate
	locate_points <- run_test_file("icosa/tests/locate/test_locate_points.R")

# plot
 	grid_plot <- run_test_file("icosa/tests/plot/test_grid_plot.R")
	grid_data_plots <- run_test_file("icosa/tests/plot/test_grid_data_plot.R") # grid via sf + data

# namespace
	namespace_rgl <- run_test_file("icosa/tests/namespace/test_rgl-icosa-silentAttach.R")
	namespace_icosa <- run_test_file("icosa/tests/namespace/test_icosa-rgl-silentAttach.R")
	namespace_plot3d <- run_test_file("icosa/tests/namespace/test_rgl-icosa-plot3d.R")

# newsf
# newsp

## # occupied
	occupied_raster <- run_test_file("icosa/tests/occupied/test_occupied_raster.R")
	occupied_natural_earth <- run_test_file("icosa/tests/occupied/test_ne_occupied_faces.R")
	occupied_paleo_coastlines <- run_test_file("icosa/tests/occupied/test_occupied_paleomap_paleocoastlines.R")
	occupied_paleo_static <- run_test_file("icosa/tests/occupied/test_occupied_paleomap_static_polygons.R")

##  # subset
 	facelayer_subset <- run_test_file("icosa/tests/bracket_subset/test_extents.R") # takes long

# resample
	resample_from_raster_to_trigrid <- run_test_file("icosa/tests/resample/test_resample_raster.R") # to trigrid and hexagrid

## # rotate
## 	point_rotate_essential <- run_test_file("icosa/tests/rotate/test_point_rotate_essential.R")
## 	point_rotate <- run_test_file("icosa/tests/rotate/test_point_rotate.R")
## 	grid_rotate <- run_test_file("icosa/tests/rotate/test_grid_rotate.R")

## # vertexradius
## 	vertexradius <- run_test_file("icosa/tests/vertexradius/test_vertexradius.R")

## # saveOBJ
## 	saveOBJ <- run_test_file("icosa/tests/saveOBJ/test_saveOBJ.R")

## # surfacecentroid
## 	surfaceCentroid <- run_test_file("icosa/tests/surfacecentroid/test_surfacecentroid.R")

# Finish
# stopCluster(cl)
