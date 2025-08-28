# parameters:
# gr: icosahedral grid



################################################################################
# 1a. random rotation
################################################################################

set.seed(1)
expect_silent(ro <- rotate(gr))
expect_true(inherits(ro, class(gr)))
expect_equal(faces(ro), faces(gr))

# reproducible
set.seed(1)
expect_silent(ro2 <- rotate(gr))
expect_equal(ro, ro2)

# explicitly given random is the same
set.seed(1)
expect_silent(ro3 <- rotate(gr, angles="random"))
expect_equal(ro3, ro2)

########################################----------------------------------------
# sf- handling
# make a copy
gr2 <- gr
suppressWarnings(gr2 <- newsf(gr2)) # this might give a warning - known bug for sf

# there is a message here:
set.seed(1)
expect_message(roSF <- rotate(gr2, angles="random"))
# this is different so make it the same
roSF@sf <- ro2@sf
expect_equal(roSF, ro2)

# message can be turned off
set.seed(1)
expect_silent(roSF <- rotate(gr2, angles="random", projnote=FALSE))
# this is different so make it the same
roSF@sf <- ro2@sf
expect_equal(roSF, ro2)

########################################----------------------------------------
# sp- handling
# make a copy
gr2 <- gr
suppressWarnings(gr2 <- newsp(gr2))

# there is a message here:
set.seed(1)
expect_message(roSP <- rotate(gr2, angles="random"))
# this is different so make it the same
roSP@sp <- ro2@sp
expect_equal(roSP, ro2)

# message can be turned off
set.seed(1)
expect_silent(roSP <- rotate(gr2, angles="random", projnote=FALSE))
# this is different so make it the same
roSP@sp <- ro2@sp
expect_equal(roSP, ro2)

################################################################################
# 1. Given angles
################################################################################

# one angle given
expect_error(rotate(gr, pi))

# rotate by pi
pivec <- c(pi, pi, pi)
expect_silent(ro180 <- rotate(gr, pivec))
expect_silent(orient <- orientation(ro180))
expect_equivalent(orient, pivec/pi*180)

# arbitrary angle
set.seed(1)
ang <- rnorm(3)

# basic rotation should work
expect_silent(ro <- rotate(gr, ang))

# all vertices should be rotate appropriately
verts <- gr@vertices
expect_silent(rotVerts <- rotate(verts, ang, output="Cartesian", origin=gr@center))
expect_equal(rotVerts, ro@vertices)

# all face centers should be rotate appropriately
cents <- gr@faceCenters
expect_silent(rotCents <- rotate(cents, ang, output="Cartesian", origin=gr@center))
expect_equal(rotCents, ro@faceCenters)

# internals too!
inVerts <- gr@skeleton$v
expect_silent(rotInVerts <- rotate(inVerts, ang, output="Cartesian", origin=gr@center))
expect_equal(rotInVerts, ro@skeleton$v)


# plotting vertices (only hexagrid)
if(!is.null(gr@skeleton$plotV)){
	inVerts <- gr@skeleton$plotV
	expect_silent(rotInVerts <- rotate(inVerts, ang, output="Cartesian", origin=gr@center))
	expect_equal(rotInVerts, ro@skeleton$plotV)
}

################################################################################
# 2. Secondary rotation
################################################################################

# expect
set.seed(2)
ang2 <- rnorm(3)
expect_silent(roSec <- rotate(ro, ang2))

# all vertices should be rotate appropriately
verts <- ro@vertices
expect_silent(rotVerts <- rotate(verts, ang2, output="Cartesian", origin=ro@center))
expect_equal(rotVerts, roSec@vertices)

# all face centers should be rotate appropriately
cents <- ro@faceCenters
expect_silent(rotCents <- rotate(cents, ang2, output="Cartesian", origin=ro@center))
expect_equal(rotCents, roSec@faceCenters)

# internals too!
inVerts <- ro@skeleton$v
expect_silent(rotInVerts <- rotate(inVerts, ang2, output="Cartesian", origin=ro@center))
expect_equal(rotInVerts, roSec@skeleton$v)

# plotting vertices (only hexagrid)
if(!is.null(ro@skeleton$plotV)){
	inVerts <- ro@skeleton$plotV
	expect_silent(rotInVerts <- rotate(inVerts, ang2, output="Cartesian", origin=ro@center))
	expect_equal(rotInVerts, roSec@skeleton$plotV)
}

################################################################################
# Test different origins!
################################################################################

################################################################################
# Known issues
################################################################################

# Orientation is meaningless upon secondary rotation. The recording of orientation needs
# to be replaced with a history, without which the rotations cannot be reversed or
# reproduced.
