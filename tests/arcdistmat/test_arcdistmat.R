library(tinytest)
library(icosa)

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

diag <- FALSE

################################################################################
# Cartesian coordinates
set.seed(1)
expect_silent(ps <- rpsphere(50))
source(file.path(genericloc, "arcdistmat/arcdistmat.R"), local=TRUE, print.eval=diag)


# Cartesian coordinates
set.seed(2)
expect_silent(ps <- rpsphere(50, output="polar"))
source(file.path(genericloc, "arcdistmat/arcdistmat.R"), local=TRUE, print.eval=diag)
