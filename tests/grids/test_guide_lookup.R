library(tinytest)
library(icosa)

diag <- FALSE
# diag <- TRUE

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# can icosa grid be created without attaching the package?
expect_message(one <- icosa:::hexagrid(deg=9))

library(icosa)
# the lookup function
lookup <- icosa:::gridLookUp

# hexagrids
data(hexguide)
guide <- hexguide
type<-"hexagrid"

source(file.path(genericloc,"grids/lookups.R"), local=TRUE, print.eval=diag)

# trigrids
data(triguide)
guide <- triguide
type<-"trigrid"

source(file.path(genericloc,"grids/lookups.R"), local=TRUE, print.eval=diag)
