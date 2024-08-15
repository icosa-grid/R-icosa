library(tinytest)
library(icosa)

setwd(file.path(Sys.getenv("Software"), "/icosa/icosa"))

# Cartesian coordinates
set.seed(1)
expect_silent(ps <- rpsphere(50))
run_test_file("tests/generic/arcs/arcdistances.R")


# Cartesian coordinates
set.seed(1)
expect_silent(ps <- rpsphere(50, output="polar"))
run_test_file("tests/generic/arcs/arcdistances.R")
