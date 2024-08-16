library(terra)
library(icosa)
library(tinytest)

setwd(file.path(Sys.getenv("Software"), "/icosa/icosa"))

r <- rast()

set.seed(10)
values(r) <- NA
values(r)[sample(1:ncell(r), 100)] <- TRUE


# actual tests
gr <- trigrid(4)
run_test_file("tests/generic/occupied/test_occupied_rast.R")

#gr <- hexagrid(c(8,10))
# run_test_file("tests/generic/occupied/test_occupied_rast.R")
# # A different testing is required here!


# randomized. rpsphere -> rasterize, locate
