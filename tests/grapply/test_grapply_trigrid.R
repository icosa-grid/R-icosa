library(icosa)
library(tinytest)

# diagnose
diag <- FALSE
#diag <- TRUE

setwd(file.path(Sys.getenv("Software"), "/icosa"))

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# grid (should work the same with trigrids and hexagrids
expect_silent(gr <- hexagrid(4))

# output structure
out <- hexagrid(8)


################################################################################
# density testing
# randomly generated points
set.seed(1)
x <- rpsphere(100, output="polar")

# the tests
# density-type functionality
source(file.path(genericloc,"grapply/grapply_trigrid_density.R"), local=TRUE, print.eval=diag)

# old function still works
source(file.path(genericloc,"gridensity/gridensity_trigrid.R"), local=TRUE, print.eval=diag)

# minimalist argumentation
source(file.path(genericloc,"grapply/grapply_trigrid_density_noOut.R"), local=TRUE, print.eval=diag)

# single point
x <- rpsphere(1, output="polar")

# density-type functionality
source(file.path(genericloc,"grapply/grapply_trigrid_density.R"), local=TRUE, print.eval=diag)

# old function still works
source(file.path(genericloc,"gridensity/gridensity_trigrid.R"), local=TRUE, print.eval=diag)


################################################################################
# associated variable testing
set.seed(1)
x <- rpsphere(100, output="polar")
x <- as.data.frame(x)

# variable is the point's latitude
x$var <- x$lat

# the tests
source(file.path(genericloc,"grapply/grapply_trigrid_variable.R"), local=TRUE, print.eval=diag)


