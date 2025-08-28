# params:
# x: point matrix
# gr: grid
# out: reused from gr

# the number of iteration trials
trials <- 7
################################################################################
set.seed(1)
# basic config - matrix method
expect_silent(o <- grapply(x, y=gr, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
set.seed(1)
expect_silent(oNormal <- grapply(x, y=gr, out=gr, iter=trials, FUN=function(x) table(x$cell), counter=FALSE, miss=0))
expect_equal(o, oNormal)
