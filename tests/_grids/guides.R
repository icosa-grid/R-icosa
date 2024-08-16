library(tinytest)

setwd(file.path(Sys.getenv("Software"), "/icosa/icosa"))

# can icosa grid be created without attaching the package?
expect_message(one <- icosa:::hexagrid(deg=9))

library(icosa)
# the lookup function
lookup <- icosa:::gridLookUp

# hexagrids
data(hexguide)
guide <- hexguide
type<-"hexagrid"

run_test_file("tests/generic/grids/lookups.R")

# trigrids
data(triguide)
guide <- triguide
type<-"trigrid"

run_test_file("tests/generic/grids/lookups.R")
