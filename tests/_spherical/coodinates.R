library(icosa)
library(tinytest)

setwd(file.path(Sys.getenv("WorkSpace"), "/2014-09-08_icosa/icosa"))

set.seed(0)
xy <- rpsphere(300, output="polar")
xyz <- rpsphere(300, output="cartesian")

# simple conversion tests
run_test_file("tests/generic/conversions/test_CarToPol_methods.R")
run_test_file("tests/generic/conversions/test_PolToCar_methods.R")
	
# location 
# trigrid 
gr <- trigrid(c(4,4))
run_test_file("tests/generic/locate/test_locate_dispatch.R")

gr <- hexagrid(c(4,4))
run_test_file("tests/generic/locate/test_locate_dispatch.R")

# surfacecentroids

# basic testing
run_test_file("tests/generic/surfacecentroid/test_surfacecentroids.R")


