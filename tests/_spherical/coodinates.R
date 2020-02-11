library(testthat)
library(icosa)
library(restools)

if(getOS()=="windows") setwd("D:/Dropbox/WorkSpace/2014-09-08_icosa/icosa")

set.seed(0)
xy <- rpsphere(30, output="polar")
xyz <- rpsphere(30, output="cartesian")

# simple conversion tests
test_dir("tests/generic/conversions/", reporter=SummaryReporter)
	
# location 
# trigrid 
gr <- trigrid(c(4,4))
test_dir("tests/generic/locate/", reporter=SummaryReporter)

gr <- hexagrid(c(4,4))
test_dir("tests/generic/locate/", reporter=SummaryReporter)

