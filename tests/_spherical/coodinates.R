library(testthat)
library(icosa)
library(restools)

if(getOS()=="windows") setwd("D:/Dropbox/WorkSpace/2014-09-08_icosa/icosa")

set.seed(0)
xy <- rpsphere(30, output="polar")
xyz <- rpsphere(30, output="cartesian")


test_dir("tests/generic/conversions/", reporter=SummaryReporter)
	