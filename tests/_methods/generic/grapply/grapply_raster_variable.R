# params:
# x: point data.frame, x has variable 'var'
# gr: grid
# out: raster

# the number of iteration trials
trials <- 7
################################################################################

# custom function
LatFromVar <- function(x, absolute=FALSE){
	if(absolute) x$var <- abs(x$var)
	tapply(
		INDEX=x$cell,
		X=x$var,
		mean, na.rm=TRUE)
}


# basic config - with missing values
set.seed(1)
expect_silent(o <- grapply(x, y=gr, out=out, iter=trials, FUN=LatFromVar, counter=FALSE))

# output class
expect_true(inherits(o, "SpatRaster"))

# no missing values
expect_true(sum(is.na(values(o)))> 0)

#matching params as out
expect_equal(res(out), res(o))
expect_equal(crs(out), crs(o))
expect_equal(dim(out), dim(o))

################################################################################
# including partial rotation successes
set.seed(1)
expect_silent(oInc <- grapply(x, y=gr, out=out, iter=trials, FUN=LatFromVar, counter=FALSE, APP.args=list(na.rm=TRUE)))

# the output class
expect_true(inherits(oInc, "SpatRaster"))

# in the overlapping part, these should be the same
with <- values(o)[,1]
without <- values(oInc)[,1]
expect_equal(with[!is.na(with)], without[!is.na(with)])

################################################################################
# Configuring the iterated function
set.seed(1)
expect_silent(oAbs <- grapply(x, y=gr, out=out, iter=trials, FUN=LatFromVar, counter=FALSE, FUN.args=list(absolute=TRUE)))

# dims should be the same as o
expect_equal(res(oAbs), res(o))
expect_equal(crs(oAbs), crs(o))
expect_equal(dim(oAbs), dim(o))

# Patterns of missing values should be the same
orig <- values(o)
absol <- values(oAbs)
expect_equal(which(is.na(orig)), which(is.na(absol)))
