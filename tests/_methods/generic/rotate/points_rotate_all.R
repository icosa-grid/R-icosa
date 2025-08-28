# Tests applicable to all matrices and data.frames
# parameter: orgip (the original point cloud)

################################################################################
# 0. completely random rotation
################################################################################

# completely random rotation
set.seed(1)
out <- NULL
expect_silent(rotp <- rotate(origp))

# inherits same class
expect_true(inherits(rotp, class(origp)))

# same number of points
expect_equal(nrow(rotp), nrow(origp))

# same attributes
expect_equal(rownames(rotp), rownames(origp))

# check these
expect_true(all(rotp[,1]>=-180 &  rotp[,1]<= 180, na.rm=TRUE))
expect_true(all(rotp[,2]>=-90 &  rotp[,2]<= 90, na.rm=TRUE))
expect_equal(ncol(rotp), 2L)

# distance matrix is identical to that based on original points

# do this only if there are no missing values
if(!any(is.na(rotp[,1]))){
	expect_silent(newDM <- arcdistmat(rotp))
	expect_silent(oldDM <- arcdistmat(origp))
	expect_equal(newDM, oldDM)
}

# save original rotation
rotpOrig <- rotp

# check reproducibility of the random rotation with seed control
set.seed(1)
expect_silent(rotp <- rotate(origp))
expect_equal(rotp, rotpOrig)

# check different output coords
set.seed(1)
out <- "cartesian"
expect_silent(rotp <- rotate(origp, output="cartesian"))
expect_equal(ncol(rotp), 3L)

# check whether it is still the same
expect_silent(repol <- CarToPol(rotp)[,1:2, drop=FALSE])
expect_equal(repol, rotpOrig)

# entire rows are missing
anyNA<- apply(rotp, 1, function(x) any(is.na(x)))
allNA<- apply(rotp, 1, function(x) all(is.na(x)))
expect_equal(anyNA, allNA)

# missing values at identical position
expect_equal(which(is.na(rotp[,1, drop=FALSE]) ), which(is.na(origp[,1, drop=FALSE]) ))


################################################################################
# 1. if rotation vector is given (3d angles)
################################################################################

# if rotation vector is given
set.seed(5)
angles <- rnorm(3)

expect_silent(rotp <- rotate(origp, angles=angles))

# rotation can be redone exactly the same way using the old function
expect_silent(origCart <- PolToCar(origp))
oldRes <- apply(origCart, 1, rotateOnePoint, angles=angles, origin=c(0,0,0))
oldResPol <- CarToPol(t(oldRes))[,1:2, drop=FALSE]

# exact match expected
expect_equal(rotp, oldResPol )


################################################################################
# 2. longitudinal rotation (latitude invariant)
################################################################################

# the longitudinal
longAngle <- 30

# angles need to take precendence!
expect_silent(rotLong <- rotate(origp, angles=angles, long=longAngle))

# latitude invariance
expect_equal(rotLong[,2], origp[,2])

# the differences in longitude
diffLong <- rotLong[,1] - origp[,1]

# dateline wrapping
diffLong[which(diffLong<0)] <-  diffLong[which(diffLong<0)]+360

# not exactly the same but practically yes
expect_true(sum(longAngle-diffLong, na.rm=TRUE) < 1e-8)


################################################################################
# 3. longitudinal rotation and latitudinal (centroid difference given)
################################################################################

# the longitudinal
longAngle <- 30
latAngle <- 20

# angles need to take precendence!
expect_silent(rotBoth <- rotate(origp,long=longAngle, lat=latAngle))

# calculate the centroid
origCentroid <- surfacecentroid(origp)
newCentroid <- surfacecentroid(rotBoth)

# the difference between centroids
centroidDiff <- newCentroid-origCentroid

expect_true(centroidDiff[1]-longAngle < 1e-8)
expect_true(centroidDiff[2]-latAngle < 1e-8)


################################################################################
# 4. Data.frame and matrix methods identical
################################################################################
origDF <- as.data.frame(origp)

# the same rotation
expect_silent(rotBothDF <- rotate(origDF, long=longAngle, lat=latAngle))

# equivalent with matrix method
expect_equivalent(rotBoth[,1], rotBothDF[,1])
expect_equivalent(rotBoth[,2], rotBothDF[,2])

# same rownames as original
expect_equal(rownames(origDF), rownames(rotBothDF))
