# Testing the location of points on grids
# 2025-08-02

library(tinytest)
library(icosa)

# working dir
 wd <- file.path(Sys.getenv("Dropbox"), "Software/icosa")

setwd(wd)

# location of generic functions
genericloc <- "icosa/tests/_methods/generic/"

# diagnose
diag <- FALSE
#diag <- TRUE

# basic testing

# create a couple of coordinates (original)
set.seed(0)
expect_silent(xy <- rpsphere(300, output="polar"))

# trigrid
expect_silent(gr <- trigrid(c(4,4)))
source(file.path(genericloc, "locate/locate_points.R"), local=TRUE, print.eval=diag)

expect_silent(gr <- hexagrid(c(4,4)))
source(file.path(genericloc, "locate/locate_points.R"), local=TRUE, print.eval=diag)
