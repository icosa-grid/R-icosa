# params:
# x: point matrix
# gr: grid
# out: raster

# the number of iteration trials
trials <- 7
################################################################################
set.seed(1)
# basic config - matrix method
expect_silent(o <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))

# output class
expect_true(inherits(o, "SpatRaster"))

# no missing values
expect_equal(sum(is.na(values(o))), 0)

#matching params as out
expect_equal(res(out), res(o))
expect_equal(crs(out), crs(o))
expect_equal(dim(out), dim(o))

# default missing
set.seed(1)
expect_silent(oMiss <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE))
expect_true(sum(is.na(values(oMiss)))>0)


################################################################################
# basic config - data.frame method
xDF <- as.data.frame(x)
set.seed(1)
expect_silent(oDF <- grapply(xDF, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
expect_identical(values(o), values(oDF))

# coordinates set manuyll
colnames(xDF) <- c("LONG", "LAT")
expect_error(grapply(xDF, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
set.seed(1)
expect_silent(oDF2 <- grapply(xDF, y=gr, out=out, iter=trials, coords=c("LONG", "LAT"), FUN=function(x) table(x$cell), counter=FALSE, miss=0))
expect_equal(values(oDF), values(oDF2))


################################################################################
# APP=NULL
set.seed(1)
expect_silent(oNULL <- grapply(x, y=gr, out=out, iter=trials,
	FUN=function(x) table(x$cell), counter=FALSE, miss=0, APP=NULL))

# manual meaning
expect_true(inherits(oNULL, "SpatRaster"))

# expected properties
expect_equal(res(out), res(oNULL))
expect_equal(crs(out), crs(oNULL))
expect_equal(dim(out)[1:2], dim(oNULL)[1:2])
expect_equal(trials, dim(oNULL)[3]) # number of layers

# the should not be any missing values
expect_equal(sum(is.na(values(oNULL))), 0)

# manual apping - reproducibility with seed control
oMeaned<- app(oNULL, fun=mean)

# they match
expect_identical(values(oMeaned), values(o))

################################################################################
# APP args - arguments passed to the APP function
set.seed(1)
expect_silent(oMiss2 <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE,
	APP.args = list(na.rm=TRUE)
))
without <- values(oMiss2)
with <- values(oMiss)

# with should have more values
expect_true(sum(is.na(with))>sum(is.na(without)))

# these two need to have the same values in overlapping parts
expect_equal(with[!is.na(with)], without[!is.na(with)])

################################################################################
# Adding missing values everywhere
if(nrow(x)>1){
	xNA <- rbind(c(NA, NA), x[1:4,], c(NA, NA), x[5:nrow(x), ], c(NA, NA))
}else{
	xNA <- rbind(c(NA, NA), x, c(NA, NA), c(NA, NA))
}

# should provide identical results to the original
set.seed(1)
expect_silent(oNA <- grapply(xNA, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
expect_equal(values(oNA), values(o))
