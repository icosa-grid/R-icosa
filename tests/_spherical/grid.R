library(icosa)
library(tinytest)

# an example grik
gr <- hexagrid(5)
run_test_file("tests/generic/plot/test_grid_plot.R")
run_test_file("test/generic/gridlabs/test_gridlabs.R")

