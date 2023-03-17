library(icosa)
library(tinytest)

library(sf)

ne <- st_read(file.path(system.file(package="icosa"),"extdata/ne_110m_land.shx" ))

# SpatialPolygons method
sf <- ne
spdf <- as(ne,"Spatial")
sp<- methods::as(spdf,"SpatialPolygons")

# triangular coarse
gr <- trigrid(4)
sol <- 191 # correct solution based on visual check!
run_test_file("tests/generic/occupied/test_occupied_vector.R")

# 
gr <- hexagrid(c(8,10), sp=TRUE)
sol <- 20652
run_test_file("tests/generic/occupied/test_occupied_vector.R")
