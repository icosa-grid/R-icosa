library(icosa)
library(tinytest)
library(rgl) # optional
testRGL <- TRUE

# diagnose
diag <- FALSE
#diag <- TRUE

setwd(file.path(Sys.getenv("Software"), "/icosa"))

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

################################################################################
# trigrid
expect_silent(gr <- trigrid(4))
# the tests
source(file.path(genericloc,"saveOBJ/saveOBJ.R"), local=TRUE, print.eval=diag)

# hexagrid
expect_silent(gr <- hexagrid(4))
# the tests
source(file.path(genericloc,"saveOBJ/saveOBJ.R"), local=TRUE, print.eval=diag)
