library(terra)
library(icosa)
library(tinytest)


r <- rast()

set.seed(10)
values(r) <- NA
values(r)[sample(1:ncell(r), 100)] <- TRUE


# actual tests
gr <- trigrid(4)
sol <- 68
run_test_file("tests/generic/occupied/test_occupied_raster.R")

gr <- hexagrid(c(8,10), sp=TRUE)
sol <- 396
run_test_file("tests/generic/occupied/test_occupied_raster.R")


# randomized. rpsphere -> rasterize, locate
