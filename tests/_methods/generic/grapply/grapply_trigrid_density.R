# params:
# x: point matrix
# gr: grid
# out: trigrid class

# the number of iteration trials
trials <- 7
################################################################################
set.seed(1)
# basic config - matrix method
expect_silent(o <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))

# output class
expect_true(inherits(o, "numeric"))

# no missing values
expect_equal(sum(is.na(o)), 0)

#matching params as out
expect_true(all(names(o)%in%faces(out)))
expect_equal(length(o), length(faces(out)))

# default missing
set.seed(1)
expect_silent(oMiss <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE))
expect_true(sum(is.na(oMiss))>0)


################################################################################
# basic config - data.frame method
xDF <- as.data.frame(x)
set.seed(1)
expect_silent(oDF <- grapply(xDF, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
expect_identical(o, oDF)

# coordinates set manually
colnames(xDF) <- c("LONG", "LAT")
expect_error(grapply(xDF, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
set.seed(1)
expect_silent(oDF2 <- grapply(xDF, y=gr, out=out, iter=trials, coords=c("LONG", "LAT"), FUN=function(x) table(x$cell), counter=FALSE, miss=0))
expect_equal(oDF, oDF2)


################################################################################
# APP=NULL
set.seed(1)
expect_silent(oNULL <- grapply(x, y=gr, out=out, iter=trials,
	FUN=function(x) table(x$cell), counter=FALSE, miss=0, APP=NULL))

# manual meaning
expect_true(inherits(oNULL, "matrix"))

# expected properties
expect_equal(dim(oNULL), c(trials, length(faces(out))))
expect_equal(colnames(oNULL), names(o))

# the should not be any missing values
expect_equal(sum(is.na(oNULL)), 0)

# manual apping - reproducibility with seed control
oMeaned<- apply(oNULL, 2, FUN=mean)

# they match
expect_identical(oMeaned, o)

################################################################################
# APP args - arguments passed to the APP function
set.seed(1)
expect_silent(oMiss2 <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE,
	APP.args = list(na.rm=TRUE)
))
without <- oMiss2
with <- oMiss

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
expect_equal(oNA, o)
