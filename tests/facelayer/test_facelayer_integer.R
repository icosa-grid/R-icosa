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
# 1. logical initialization
################################################################################
# A. single value
vals <- 1L
expect_silent(fl <- facelayer(gr,vals))

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))
expect_true(inherits(flVals, "integer"))
expect_equivalent(flVals, rep(vals, length(gridFaces)))

################################################################################
# B. multiple values (incomplete - unnamed)
vals <- c(4L, 6L)
expect_error(fl <- facelayer(gr,vals)) # undefined positions

################################################################################
# C. multiple values (incomplete - correctly named)
vals <- c(5L, 0L)
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
expect_true(inherits(flVals, "integer"))
expect_equivalent(flVals, reconstructed)

################################################################################
# D. multiple values (incomplete - incorrectly named)
vals <- c(5L, 0L)
names(vals) <- c("F1", "a")
expect_error(fl <- facelayer(gr,vals)) # undefined positions

################################################################################
# E. multiple values (incomplete, explicit NA)
vals <- c(6L, 1L, NA)
names(vals) <- c("F3", "F5", "F7")
expect_silent(fl <- facelayer(gr,vals))

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))
expect_true(inherits(flVals, "integer"))

# manually define the output
reconstructed <- rep(NA, length(faceNames))
names(reconstructed) <- faceNames
reconstructed[names(vals)] <- vals
# and compare
expect_equivalent(flVals, reconstructed)

################################################################################
# F. multiple values (complete, unnnamed)
vals <- rep(c(5L, 7L), length(faceNames)/2)
vals <- vals[1:length(faceNames)]
expect_silent(fl <- facelayer(gr,vals)) # undefined positions

# conssitency check
source(file.path(genericloc,"facelayer/facelayer_structure.R"), local=TRUE, print.eval=diag)

# the definition of values
expect_silent(flVals <- values(fl))
expect_true(inherits(flVals, "integer"))
expect_equivalent(flVals, vals)

