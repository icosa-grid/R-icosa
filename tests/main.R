# The main testing

library(tinytest)
library(parallel)
library(icosa)

if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/icosa")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/icosa")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/icosa")

setwd(wd)

# make a cluster of 8
cl <- parallel::makeCluster(4, outfile="")
parallel::clusterCall(cl, source, "icosa/tests/source.R")

# tests
# occupied paleomap
paleomap <- run_test_dir("icosa/tests/paleomap")


# spherical tests
point_rot <- run_test_file("icosa/tests/_spherical/point_rotation.R")

# Finish
stopCluster(cl)
