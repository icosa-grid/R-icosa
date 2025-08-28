# testing the rotation function
# 2025-01-25
library(tinytest)
library(icosa)


# get the function
# tests are written for the original R function
# this will be kept as a wrapper for testing
rotateMultiplePoints <- icosa:::rotateMultiplePoints

if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/icosa")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/icosa")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/icosa")

setwd(wd)

# get rotateOnePoint() - old R function
source("icosa/tests/_methods/old/old_rotation.R")

# all the angles in the circle
circleAngles <- seq(0, 2*pi, length.out=16)
################################################################################
# 1. Single point rotations
################################################################################
# I. X coordinate
# x coordinate
radius <- 5
pX <- c(radius,0,0)

# the origin
origin <- c(0,0,0)

# 0. Identity rotation
# the rotation vector
rotVec <- c(0,0,0)
expect_silent(xRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equal(pX, xRot)


# period identity
rotVec <- c(2*pi, 2*pi,2*pi)
expect_silent(xRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equal(pX, xRot)


# 1. X-rotation - should be identical no matter the angle
rotVec <- c(pi, 0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equal(pX, xRot)

# go through the different angles

for(i in 1:length(circleAngles)){
	rotVec <- c(circleAngles[i],0,0)
	expect_silent(xRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
	expect_equal(pX, xRot)
}


# 2. Y-rotation
# 90 degrees
deg <- 90
rotVec <- c(0, deg / 180 * pi, 0 )
expect_silent(yRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(yRot, c(0, 0, radius))

# 180 degrees
deg <- 180
rotVec <- c(0, deg / 180 * pi, 0 )
expect_silent(yRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(yRot, c(-radius, 0, 0))

# 270 degrees
deg <- 270
rotVec <- c(0, deg / 180 * pi, 0 )
expect_silent(yRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(yRot, c(0, 0, -radius))

# general case
for(i in 1:length(circleAngles)){
	# the current angle in radians
	rad <- circleAngles[i]

	# the rotation vector
	rotVec <- c(0, rad, 0 )

	# the actual rotation
	expect_silent(yRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))

	# test
	expect_equivalent(yRot, c(cos(rad)*radius, 0, sin(rad)*radius))

}

# 3. Z-rotation
# 90 degrees
deg <- 90
rotVec <- c(0, 0, deg / 180 * pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(zRot, c(0, radius, 0))

# 180 degrees
deg <- 180
rotVec <- c(0, 0, deg / 180 * pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(zRot, c(-radius, 0, 0))

# 270 degrees
deg <- 270
rotVec <- c(0, 0, deg / 180 * pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(zRot, c(0, -radius, 0))

# general case
for(i in 1:length(circleAngles)){
	# the current angle in radians
	rad <- circleAngles[i]

	# the rotation vector
	rotVec <- c(0, 0, rad)

	# the actual rotation
	expect_silent(zRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))

	# test
	expect_equivalent(zRot, c(cos(rad)*radius, sin(rad)*radius, 0))

}

# X + Y rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(rad, rad, 0)
expect_silent(xyRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(xyRot, c(cos(rad)*radius, 0, cos(rad)*radius) )

# Y + Z rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(0, rad, rad)
expect_silent(yzRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(yzRot, c(radius/2, radius/2,cos(rad)*radius))

# X + Z rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(rad, 0, rad)
expect_silent(xzRot <- rotateMultiplePoints(coords=pX, angles=rotVec, origin=origin))
expect_equivalent(xzRot, c(cos(rad)*radius,cos(rad)*radius, 0))


################################################################################
# I. Y coordinate
# y coordinate
radius <- 5
pY <- c(0, radius,0)

# the origin
origin <- c(0,0,0)

# 0. Identity rotation
# the rotation vector
rotVec <- c(0,0,0)
expect_silent(yRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equal(pY, yRot)


# period identity
rotVec <- c(2*pi, 2*pi,2*pi)
expect_silent(yRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equal(pY, yRot)


# 1. Y-rotation - should be identical no matter the angle
rotVec <- c(0, pi, 0 )
expect_silent(yRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equal(pY, yRot)

# go through the different angles
for(i in 1:length(circleAngles)){
	rotVec <- c(0,circleAngles[i],0)
	expect_silent(yRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
	expect_equal(pY, yRot)
}


# 2. X-rotation
# 90 degrees
deg <- 90
rotVec <- c(deg / 180 * pi,0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(xRot, c(0, 0, radius))

# 180 degrees
deg <- 180
rotVec <- c(deg / 180 * pi, 0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(xRot, c(0,-radius, 0))

# 270 degrees
deg <- 270
rotVec <- c( deg / 180 * pi,0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(xRot, c(0, 0, -radius))

# general case
for(i in 1:length(circleAngles)){
	# the current angle in radians
	rad <- circleAngles[i]

	# the rotation vector
	rotVec <- c(rad, 0, 0)

	# the actual rotation
	expect_silent(xRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))

	# test
	expect_equivalent(xRot, c(0, cos(rad)*radius, sin(rad)*radius))

}

# 3. Z-rotation
# 90 degrees
deg <- 90
rotVec <- c(0, 0, deg / 180 * pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(zRot, c(-radius, 0, 0))

# 180 degrees
deg <- 180
rotVec <- c(0, 0, deg / 180 * pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(zRot, c(0, -radius, 0))

# 270 degrees
deg <- 270
rotVec <- c(0, 0, deg / 180 * pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(zRot, c(radius, 0,  0))

# general case
for(i in 1:length(circleAngles)){
	# the current angle in radians
	rad <- circleAngles[i]

	# the rotation vector
	rotVec <- c(0, 0, rad)

	# the actual rotation
	expect_silent(zRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))

	# test
	expect_equivalent(zRot, c(-sin(rad)*radius, cos(rad)*radius, 0))

}

# X + Y rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(rad, rad, 0)
expect_silent(xyRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(xyRot, c(-radius/2, cos(rad)*radius, radius/2) )

# Y + Z rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(0, rad, rad)
expect_silent(yzRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(yzRot, c(-cos(rad)*radius, cos(rad)*radius, 0))

# X + Z rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(rad, 0, rad)
expect_silent(xzRot <- rotateMultiplePoints(coords=pY, angles=rotVec, origin=origin))
expect_equivalent(xzRot, c(-radius/2, radius/2, cos(rad)*radius))

################################################################################
# III. Z coordinate
# y coordinate
radius <- 5
pZ <- c(0, 0, radius)

# the origin
origin <- c(0,0,0)

# 0. Identity rotation
# the rotation vector
rotVec <- c(0,0,0)
expect_silent(zRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equal(pZ, zRot)


# period identity
rotVec <- c(2*pi, 2*pi,2*pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equal(pZ, zRot)


# 1. Z-rotation - should be identical no matter the angle
rotVec <- c(0, 0, pi)
expect_silent(zRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equal(pZ, zRot)

# go through the different angles
for(i in 1:length(circleAngles)){
	rotVec <- c(0,0, circleAngles[i])
	expect_silent(zRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
	expect_equal(pZ, zRot)
}


# 2. X-rotation
# 90 degrees
deg <- 90
rotVec <- c(deg / 180 * pi,0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(xRot, c(0, -radius, 0))

# 180 degrees
deg <- 180
rotVec <- c(deg / 180 * pi, 0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(xRot, c(0,0, -radius))

# 270 degrees
deg <- 270
rotVec <- c( deg / 180 * pi,0, 0 )
expect_silent(xRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(xRot, c(0, radius, 0))

# general case
for(i in 1:length(circleAngles)){
	# the current angle in radians
	rad <- circleAngles[i]

	# the rotation vector
	rotVec <- c(rad, 0, 0)

	# the actual rotation
	expect_silent(xRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))

	# test
	expect_equivalent(xRot, c(0, -sin(rad)*radius, cos(rad)*radius))

}

# 3. Y-rotation
# 90 degrees
deg <- 90
rotVec <- c(0, deg / 180 * pi, 0)
expect_silent(yRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(yRot, c(-radius, 0, 0))

# 180 degrees
deg <- 180
rotVec <- c(0, deg / 180 * pi, 0)
expect_silent(yRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(yRot, c(0,0, -radius))

# 270 degrees
deg <- 270
rotVec <- c(0, deg / 180 * pi, 0)
expect_silent(yRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(yRot, c(radius, 0,  0))

# general case
for(i in 1:length(circleAngles)){
	# the current angle in radians
	rad <- circleAngles[i]

	# the rotation vector
	rotVec <- c(0, rad, 0)

	# the actual rotation
	expect_silent(yRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))

	# test
	expect_equivalent(yRot, c(-sin(rad)*radius,0, cos(rad)*radius))

}

# X + Y rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(rad, rad, 0)
expect_silent(xyRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(xyRot, c(-radius/2, -cos(rad)*radius, radius/2) )

# Y + Z rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(0, rad, rad)
expect_silent(yzRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(yzRot, c(-radius/2,-radius/2, cos(rad)*radius ))

# X + Z rotation
deg <- 45
rad <- deg/180*pi
rotVec <- c(rad, 0, rad)
expect_silent(xzRot <- rotateMultiplePoints(coords=pZ, angles=rotVec, origin=origin))
expect_equivalent(xzRot, c(radius/2, -radius/2, cos(rad)*radius))



################################################################################
# 2. Multiple point rotations -> basis for the matrix-method
################################################################################
# 2 A. initial
########################################----------------------------------------
set.seed(1)
randPoints <- rpsphere(10)
rotVec <- c(0.2, 0.4, 0.5)


# 1. rotate all of them
expect_silent(cppMultiple <- rotateMultiplePoints(randPoints, angles=rotVec, origin=origin))

# 2. Ensure repetition results in the same result (deep memcopy test)
for(i in 1:100){
	expect_silent(cppMultipleRe <- rotateMultiplePoints(randPoints, angles=rotVec, origin=origin))
	expect_equal(cppMultiple, cppMultipleRe)
}


# 3. calculate rotations one-by-one (compare vector vs matrix method)
expect_silent(oneByOne <-t(apply(randPoints, 1, rotateMultiplePoints, angles=rotVec, origin=origin)))
colnames(oneByOne) <- colnames(cppMultiple)
rownames(oneByOne) <- rownames(cppMultiple)

# comparison
expect_equal(oneByOne, cppMultiple)


# 4. calculate rotations one-by-one (pure R function)- OLD
oneByOneOld <-t(apply(randPoints, 1, rotateOnePoint, angles=rotVec, origin=origin))
colnames(oneByOneOld) <- colnames(cppMultiple)

# comparison
expect_equal(oneByOneOld, cppMultiple)

# store for comparison
cppPrevious <- cppMultiple


########################################----------------------------------------
# 2B. repetition with different input
set.seed(2)
randPoints <- rpsphere(10)

expect_silent(cppMultiple <- rotateMultiplePoints(randPoints, angles=rotVec, origin=origin))

# should be different from the previous one
expect_true(!identical(cppMultiple, cppPrevious))

# 2. Ensure repetition results in the same result (deep memcopy test)
for(i in 1:100){
	expect_silent(cppMultipleRe <- rotateMultiplePoints(randPoints, angles=rotVec, origin=origin))
	expect_equal(cppMultiple, cppMultipleRe)
}


# 3. calculate rotations one-by-one (compare vector vs matrix method)
expect_silent(oneByOne <-t(apply(randPoints, 1, rotateMultiplePoints, angles=rotVec, origin=origin)))
colnames(oneByOne) <- colnames(cppMultiple)
rownames(oneByOne) <- rownames(cppMultiple)

# comparison
expect_equal(oneByOne, cppMultiple)


# 4. calculate rotations one-by-one (pure R function)- OLD
oneByOneOld <-t(apply(randPoints, 1, rotateOnePoint, angles=rotVec, origin=origin))
colnames(oneByOneOld) <- colnames(cppMultiple)

# comparison
expect_equal(oneByOneOld, cppMultiple)


# speed comparison: about 200 times faster
## randPoints <- rpsphere(1000000)
## system.time(
## 	cppMultiple <- rotateMultiplePoints(randPoints, angles=rotVec, origin=origin)
## )
## system.time(
## 	oneByOneOld <-t(apply(randPoints, 1, rotateOnePoint, angles=rotVec, origin=origin))
## )
