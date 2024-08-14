library(tinytest)
library(icosa)

setwd(file.path(Sys.getenv("Software"), "/icosa/icosa"))

# testing name access

# trigrids
data(hexguide)
guide <- hexguide[1:15, ]
run_test_file("tests/generic/grids/faces.R")

# trigrids
data(triguide)
guide <-triguide[1:15, ]
run_test_file("tests/generic/grids/faces.R")
