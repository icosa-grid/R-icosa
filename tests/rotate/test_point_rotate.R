# Testing rotation wrappers (matrix, df method) around basic CPP rotation
#
# 2025-08-02
library(tinytest)
library(icosa)
# working dir
 wd <- file.path(Sys.getenv("Dropbox"), "Software/icosa")

setwd(wd)

# location of generic functions
genericloc <- "icosa/tests/_methods/generic/"

# theold rotation
source("icosa/tests/_methods/old/old_rotation.R")

# diagnose
diag <- FALSE
# diag <- TRUE

# generate random points (matrix)
set.seed(5)
expect_silent(origp <- rpsphere(100,output="polar"))
source(file.path(genericloc,"rotate/points_rotate_all.R"), local=TRUE, print.eval=diag)

# single point (matrix)
set.seed(5)
expect_silent(origp <- rpsphere(100,output="polar"))
origp <- origp[1, ,drop=FALSE]
source(file.path(genericloc,"rotate/points_rotate_all.R"), local=TRUE, print.eval=diag)


# points with rownames (matrix)
set.seed(5)
expect_silent(origp <- rpsphere(100,output="polar"))
rownames(origp) <- paste0("r", 1:nrow(origp))
source(file.path(genericloc,"rotate/points_rotate_all.R"), local=TRUE, print.eval=diag)


# points with NAs (matrix)
origp[c(1, 4, nrow(origp)), ] <- NA
source(file.path(genericloc,"rotate/points_rotate_all.R"), local=TRUE, print.eval=diag)
