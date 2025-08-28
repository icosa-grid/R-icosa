library(icosa)
library(tinytest)

# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")

# get the
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

diag <- FALSE
# diag <- TRUE

# Assuming that the graph is alright, this function should produce a grid-instance
# independent results.
expect_silent(gr <- hexagrid(2))

# single face
shape <- "F1"
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)

# wrong face input
shape <- "WRONG"
expect_error(patches(gr, shape))

# two connected faces
shape <- c("F1", "F2")
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(out, c(1,1))

# two unconnected faces
shape <- c("F1", "F20")
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(out, c(1,2))

# two unconnected faces + wrong
shape <- c("F1", "F20", "WRONG")
expect_error(patches(shape, gr))

# three from the same pathc
shape <- c("F1", "F2", "F3")
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(out, c(1,1,1))

# two from one, n=3
shape <- c("F1", "F2", "F20")
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(out, c(1,1,2))

################################################################################
# Example
shape <- paste0("F", c(3,6,7,9, 10, 16, 22, 26))
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(out, c(1,2,2,1,2,2,1,3))

################################################################################
# cast input to different facelayers

# character
fl <- facelayer(gr)
fl[shape] <- letters[1:length(shape)]
source(file.path(genericloc, "patches/test_patches_facelayer.R"), local=TRUE, print.eval=diag)

# numeric (floating points)
fl <- facelayer(gr)
fl[shape] <- 1:length(shape)/7
source(file.path(genericloc, "patches/test_patches_facelayer.R"), local=TRUE, print.eval=diag)

# integers
fl <- facelayer(gr)
fl[shape] <- 1:length(shape)
source(file.path(genericloc, "patches/test_patches_facelayer.R"), local=TRUE, print.eval=diag)

# logical
fl <- facelayer(gr, FALSE)
fl[shape] <- TRUE
source(file.path(genericloc, "patches/test_patches_facelayer.R"), local=TRUE, print.eval=diag)


################################################################################
# A test after terra is attached.
suppressPackageStartupMessages(library(terra))

# should work just the same
source(file.path(genericloc, "patches/test_patch_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(out, c(1,2,2,1,2,2,1,3))
