library(terra)
library(icosa)
library(tinytest)

# diagnose
diag <- FALSE
#diag <- TRUE

setwd(file.path(Sys.getenv("Software"), "/icosa"))

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# generate a bunch of stuff
r <- rast()
res(r) <- 10
values(r) <- 1:ncell(r)

# actual tests
expect_silent(gr <- trigrid(4))
sol <- 103840
source(file.path(genericloc,"resample/test_resample_from_raster_to_trigrid.R"), local=TRUE, print.eval=diag)
source(file.path(genericloc,"resample/test_resample_from_raster_to_facelayer.R"), local=TRUE, print.eval=diag)

expect_silent(gr <- hexagrid(c(8,10)))
sol <- 20768649
source(file.path(genericloc,"resample/test_resample_from_raster_to_trigrid.R"), local=TRUE, print.eval=diag)
source(file.path(genericloc,"resample/test_resample_from_raster_to_facelayer.R"), local=TRUE, print.eval=diag)
