library(terra)
library(icosa)
library(tinytest)

setwd("/mnt/sky/Dropbox/WorkSpace/2014-09-08_icosa/icosa")

r <- rast()
res(r) <- 10
values(r) <- 1:ncell(r)

# actual tests
gr <- trigrid(4)
sol <- 103840 
run_test_file("tests/generic/resample/test_resample.R", reporter=SummaryReporter)

gr <- hexagrid(c(8,10))
sol <- 20768649
run_test_file("tests/generic/resample/test_resample.R", reporter=SummaryReporter)
