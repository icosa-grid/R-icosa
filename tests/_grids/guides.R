library(icosa)
library(tinytest)

setwd(file.path(Sys.getenv("WorkSpace"), "/2014-09-08_icosa/icosa"))


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
