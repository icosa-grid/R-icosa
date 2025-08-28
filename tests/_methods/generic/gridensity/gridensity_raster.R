# params:
# x: point matrix
# gr: grid
# out: raster

# the number of iteration trials
trials <- 7
################################################################################

# basic config - matrix method
# The new grapply implementation
set.seed(1)
expect_silent(o <- grapply(x, y=gr, out=out, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))

set.seed(1)
expect_warning(gd <- gridensity(x, y=gr, out=out, trials=trials))

# output should be exactly the same as that of grapply (that is what this is now)
expect_equal(res(gd), res(o))
expect_equal(crs(gd), crs(o))
expect_equal(dim(gd), dim(o))
expect_equal(values(gd), values(o))
