library(icosa)
library(testthat)

setwd("/mnt/sky/Dropbox/WorkSpace/2014-09-08_icosa/icosa")

gr <- hexagrid(4)
test_dir("tests/generic/rotate/", reporter=SummaryReporter)

gr <- trigrid(4)
test_dir("tests/generic/rotate/", reporter=SummaryReporter)
