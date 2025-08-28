library(icosa)
library(tinytest)

# diagnose
diag <- FALSE
#diag <- TRUE

setwd(file.path(Sys.getenv("Software"), "/icosa"))

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# base grid (does not matter whether trigrid or hexagrid)
expect_silent(gr <- trigrid(4))
expect_silent(faceNames <- faces(gr))

################################################################################
# 0. empty facelayer
vals <- NULL
expect_silent(fl <- facelayer(gr))
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))
expect_equivalent(flVals, rep(NA, length(gridFaces)))
expect_true(inherits(flVals, "logical"))

################################################################################
# 1. logical initialization
################################################################################
# A. single value
vals <- TRUE
expect_silent(fl <- facelayer(gr,vals))

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))
expect_equivalent(flVals, rep(vals, length(gridFaces)))
expect_true(inherits(flVals, "logical"))

################################################################################
# B. multiple values (incomplete - unnamed)
vals <- c(TRUE, FALSE)
expect_error(fl <- facelayer(gr,vals)) # undefined positions

################################################################################
# C. multiple values (incomplete - correctly named)
vals <- c(TRUE, FALSE)
names(vals) <- c("F3", "F5")
expect_silent(fl <- facelayer(gr,vals))

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)


# manually define the output
reconstructed <- rep(NA, length(faceNames))
names(reconstructed) <- faceNames
reconstructed[names(vals)] <- vals
# and compare
expect_silent(flVals <- values(fl))
expect_true(inherits(flVals, "logical"))
expect_equivalent(flVals, reconstructed)

################################################################################
# D. multiple values (incomplete - incorrectly named)
vals <- c(TRUE, FALSE)
names(vals) <- c("F1", "a")
expect_error(fl <- facelayer(gr,vals)) # undefined positions

################################################################################
# E. multiple values (incomplete, explicit NA)
vals <- c(TRUE, FALSE, NA)
names(vals) <- c("F3", "F5", "F7")
expect_silent(fl <- facelayer(gr,vals))

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))

# manually define the output
reconstructed <- rep(NA, length(faceNames))
names(reconstructed) <- faceNames
reconstructed[names(vals)] <- vals
# and compare
expect_true(inherits(flVals, "logical"))
expect_equivalent(flVals, reconstructed)

################################################################################
# F. multiple values (complete, unnnamed)
vals <- rep(c(TRUE, FALSE), length(faceNames)/2)
vals <- vals[1:length(faceNames)]
expect_silent(fl <- facelayer(gr,vals)) # undefined positions

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))
expect_true(inherits(flVals, "logical"))
expect_equivalent(flVals, vals)

