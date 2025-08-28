# input: xyz 3 column matrix
#
# Basic matrix-method"
	expect_silent(out <- CarToPol(xyz))
	expect_equal(class(xyz), class(out))
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out), nrow(xyz))

	# attributes match
	expect_equal(colnames(out), c("long", "lat", "rho"))
	expect_equal(rownames(out), rownames(xyz))

	# no radius option
	expect_silent(outNoRad <- CarToPol(xyz, norad=TRUE))
	expect_equal(outNoRad, out[,1:2, drop=FALSE])

# NA positions identical
	naOrig <- is.na(xyz[,1]) | is.na(xyz[,2]) | is.na(xyz[,3])
	naNew  <- is.na(out[,1]) | is.na(out[,2]) | is.na(out[,3])
	expect_equivalent(naOrig, naNew)

# reconverting
	rad <- unique(out[!is.na(out[,3]),3])[1] # there is some numeric variance
	expect_silent(rexyz <- PolToCar(out[, 1:2, drop=FALSE], radius=rad))
	expect_equal(rexyz, xyz)

################################################################################
# "data.frame"-method"
	xyzDF <- as.data.frame(xyz)
	expect_silent(outDF <- CarToPol(xyzDF))
	expect_equal(class(xyzDF), class(outDF))
	expect_equal(ncol(outDF), 3)
	expect_equal(nrow(outDF), nrow(xyzDF))

	# attributes match
	expect_equal(colnames(outDF), c("long", "lat", "rho"))
	expect_equal(rownames(outDF), rownames(xyzDF))

# reconverting
	expect_silent(rexyzDF <- PolToCar(outDF[, 1:2, drop=FALSE], radius=rad))
	expect_equal(rexyzDF, xyzDF)
