# Testing rotation wrappers (matrix, df method) around basic CPP rotation
#
# 2025-08-07
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

# one trigrid
gr <- trigrid(4)
source(file.path(genericloc,"rotate/grid_rotate.R"), local=TRUE, print.eval=diag)

# one hexagrid
gr <- hexagrid(4)
source(file.path(genericloc,"rotate/grid_rotate.R"), local=TRUE, print.eval=diag)
