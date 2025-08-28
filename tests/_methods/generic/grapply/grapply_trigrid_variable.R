# params:
# x: point data.frame, x has variable 'var'
# gr: grid
# out: trigrid class

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
expect_true(inherits(o, "numeric"))

# no missing values
expect_true(sum(is.na(o))> 0)

#matching params as out
expect_true(all(names(o)%in%faces(out)))
expect_equal(length(o), length(faces(out)))

################################################################################
# including partial rotation successes
set.seed(1)
expect_silent(oInc <- grapply(x, y=gr, out=out, iter=trials, FUN=LatFromVar, counter=FALSE, APP.args=list(na.rm=TRUE)))

# the output class
expect_true(inherits(oInc, "numeric"))

# in the overlapping part, these should be the same
with <- o
without <- oInc
expect_equal(with[!is.na(with)], without[!is.na(with)])

################################################################################
# Configuring the iterated function
set.seed(1)
expect_silent(oAbs <- grapply(x, y=gr, out=out, iter=trials, FUN=LatFromVar, counter=FALSE, FUN.args=list(absolute=TRUE)))

# dims should be the same as o
expect_equal(length(o), length(oAbs))
expect_equal(names(o), names(oAbs))

# Patterns of missing values should be the same
orig <- o
absol <-oAbs
expect_equal(which(is.na(orig)), which(is.na(absol)))
