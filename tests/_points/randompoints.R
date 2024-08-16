library(tinytest)
library(icosa)

setwd(file.path(Sys.getenv("Software"), "/icosa/icosa"))

################################################################################
# arcdistances()
# Cartesian coordinates
set.seed(1)
expect_silent(ps <- rpsphere(50))
run_test_file("tests/generic/arcs/arcdistances.R")


# Cartesian coordinates
set.seed(1)
expect_silent(ps <- rpsphere(50, output="polar"))
run_test_file("tests/generic/arcs/arcdistances.R")

################################################################################
# arcs()
# single point
expect_silent(ps <- rpsphere(1, output="polar"))
run_test_file("tests/generic/arcs/arcs.R")

# two points
expect_silent(ps <- rpsphere(2, output="polar"))
run_test_file("tests/generic/arcs/arcs.R")

set.seed(1)
np<- ceiling(sort( runif(100, 1, 100)))

for(i in 1:length(np)){
	# plotting them
	expect_silent(ps <- rpsphere(np[i], output="polar"))
	run_test_file("tests/generic/arcs/arcs.R")
}
