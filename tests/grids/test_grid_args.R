library(tinytest)
library(icosa)

diag <- FALSE
# diag <- TRUE

# working direcory
 wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# trigrids

# grab
lookup <- icosa:::gridLookUp


# same tests apply to the trigrid an hexagrid classes
# trigrids
gr <- "trigrid"
source(file.path(genericloc,"grids/constructor_args.R"), local=TRUE, print.eval=diag)

# hexagrids
gr <- "hexagrid"
source(file.path(genericloc,"grids/constructor_args.R"), local=TRUE, print.eval=diag)
