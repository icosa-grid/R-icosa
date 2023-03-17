# gr: the grid
# sp: spatial object in spatialpolygons
# spdf: spatial object in spatialpolygonsdataframe
# sf: sf version


# SpatialPolygons
expect_silent(fl <- occupied(gr,sp, out="facelayer"))
expect_true(inherits(fl, "facelayer"))

expect_equal(sum(fl), sol)

expect_silent(lo <- occupied(gr,sp))
expect_true(inherits(lo, "logical"))

expect_equal(sum(lo), sol)


# SpatialPolygonsDataFrame
expect_silent(fl <- occupied(gr,spdf, out="facelayer"))
expect_true(inherits(fl, "facelayer"))

expect_equal(sum(fl), sol)

expect_silent(lo <- occupied(gr,spdf))
expect_true(inherits(lo, "logical"))

expect_equal(sum(lo), sol)


# sf type
expect_silent(fl <- occupied(gr,sf, out="facelayer"))
expect_true(inherits(fl, "facelayer"))

expect_equal(sum(fl), sol)

expect_silent(lo <- occupied(gr,sf))
expect_true(inherits(lo, "logical"))

expect_equal(sum(lo), sol)
