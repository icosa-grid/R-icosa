library(icosa)
library(tinytest)

# get the
setwd(wd)

# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")

diag <- FALSE
# diag <- TRUE

# location of generic testing methods
genericloc <- "icosa/tests/_methods/generic/"

# Assuming that the graph is alright, this function should produce grid-instance
# independent results.
expect_silent(gr <- hexagrid(2))

################################################################################
# No Holes

# single face
shape <- "F1"
expect_silent(out <- holes(gr, shape))
expect_null(out <- holes(gr, shape))

# wrong face input
shape <- "WRONG"
expect_error(holes(gr, shape))

# two connected faces
shape <- c("F1", "F2")
expect_silent(out <- holes(gr, shape))
expect_null(out <- holes(gr, shape))

# two unconnected faces
shape <- c("F1", "F20")
expect_silent(out <- holes(gr, shape))
expect_null(out <- holes(gr, shape))

# two unconnected faces + wrong
shape <- c("F1", "F20", "WRONG")
expect_error(patches(gr, shape))

# three from the same pathc
shape <- c("F1", "F2", "F3")
expect_silent(out <- holes(gr, shape))
expect_null(out <- holes(gr, shape))

# two from one, n=3
shape <- c("F1", "F2", "F20")
expect_silent(out <- holes(gr, shape))
expect_null(out <- holes(gr, shape))

################################################################################
# Example - known outuput
shape <- paste0("F", c(4, 5, 11, 13, 15, 21, 24, 26, 32, 33, 34, 35, 36))
source(file.path(genericloc, "holes/test_holes_output.R"), local=TRUE, print.eval=diag)
expect_equivalent(sort(names(out)), sort(c("F12", "F14", "F23", "F25", "F22")))
expect_equal(length(unique(out)), 2L)

################################################################################
# cast input to different facelayers

# character
fl <- facelayer(gr)
fl[shape] <- letters[1:length(shape)]
source(file.path(genericloc, "holes/test_holes_facelayer.R"), local=TRUE, print.eval=diag)

# numeric (floating points)
fl <- facelayer(gr)
fl[shape] <- 1:length(shape)/7
source(file.path(genericloc, "holes/test_holes_facelayer.R"), local=TRUE, print.eval=diag)

# integers
fl <- facelayer(gr)
fl[shape] <- 1:length(shape)
source(file.path(genericloc, "holes/test_holes_facelayer.R"), local=TRUE, print.eval=diag)

# logical
fl <- facelayer(gr, FALSE)
fl[shape] <- TRUE
source(file.path(genericloc, "holes/test_holes_facelayer.R"), local=TRUE, print.eval=diag)
