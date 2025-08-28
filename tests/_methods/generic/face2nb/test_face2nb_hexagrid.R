# This generic test compares the neighbor-list objects that
# are built from the icosa grids to those that are built using
# spdep's poly2nb function. The resulting weight matrices are compared.

# depends on
# @param one: hexagrid-class
# @param faceNB: neigborhood


# the number of entries is the same as the number of faces
expect_equal(as.numeric(length(one)), length(faceNB))

# the spacing should be the number of entries
expect_silent(space <- spacing(one))

# number of spacings *2 is the number of links in the neibhorhood structure
expect_equal(length(unlist(faceNB)), length(space)*2)

# list values are the same
suppressMessages(suppressWarnings(poly <- poly2nb(one@sf)))
expect_equivalent(poly, faceNB)

# the autocorrelation structre
ico <- spdep::nb2listw(faceNB, style="W", zero.policy=TRUE)
pol <- spdep::nb2listw(poly, style="W", zero.policy=TRUE)

# some attributes differ!
expect_equivalent(ico, pol)
