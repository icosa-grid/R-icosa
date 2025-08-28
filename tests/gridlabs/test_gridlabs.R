library(tinytest)
library(icosa)

diag <- FALSE
# diag <- TRUE

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# an example grik
gr <- trigrid(5)
source(file.path(genericloc, "gridlabs/gridlabs.R"), local=TRUE, print.eval=diag)

# an example grik
gr <- hexagrid(5)
source(file.path(genericloc, "gridlabs/gridlabs.R"), local=TRUE, print.eval=diag)
