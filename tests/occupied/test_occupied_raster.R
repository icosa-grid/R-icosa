suppressPackageStartupMessages(library(terra))
library(icosa)
library(tinytest)

# diagnose
diag <- FALSE

setwd(file.path(Sys.getenv("Software"), "/icosa"))

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# create an empti raster
r <- rast()

# generate a bunc of random stuff
set.seed(10)
values(r) <- NA
values(r)[sample(1:ncell(r), 100)] <- TRUE


# actual tests
expect_silent(gr <- trigrid(4))
source(file.path(genericloc,"occupied/test_occupied_rast.R"), local=TRUE, print.eval=diag)

#gr <- hexagrid(c(8,10))
# run_test_file("tests/generic/occupied/test_occupied_rast.R")
# # A different testing is required here!


# randomized. rpsphere -> rasterize, locate
