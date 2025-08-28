# Testing Polar to Cartesian conversions and vice versa

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
expect_silent(xyz <- rpsphere(300))
source(file.path(genericloc, "conversions/test_CarToPol_start.R"), local=TRUE, print.eval=diag)

# single row
xyz<- xyz[1, , drop=FALSE]
source(file.path(genericloc, "conversions/test_CarToPol_start.R"), local=TRUE, print.eval=diag)

# numeric vector

# matrix with missing values
set.seed(0)
expect_silent(xyz <- rpsphere(300))
xyz[c(1, 10, nrow(xyz)),] <- NA
source(file.path(genericloc, "conversions/test_CarToPol_start.R"), local=TRUE, print.eval=diag)

# create a couple of coordinates (with names)
rownames(xyz) <- paste0("a", 1:nrow(xyz))
source(file.path(genericloc, "conversions/test_CarToPol_start.R"), local=TRUE, print.eval=diag)
