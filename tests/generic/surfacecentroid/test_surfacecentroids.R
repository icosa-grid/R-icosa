# input: xy

# method dispatch checks
expect_silent(whole <- surfacecentroid(xy))
expect_true(whole[1]>=-180 & whole[1] <= 180)
expect_true(whole[2]>=-90 & whole[2] <= 90)

# the sp method
expect_silent(whole <- surfacecentroid(sp::SpatialPoints(xy)))

# the data.frame method
expect_silent(whole <- surfacecentroid(as.data.frame(xy)))

