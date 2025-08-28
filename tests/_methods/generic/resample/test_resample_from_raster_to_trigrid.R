# Resampling from SpatRaster to trigrid
# provide sol which is the sum of the result
# r: raster
expect_silent(re <- resample(r, gr))

# class output - default: to numeric
expect_equal(class(re), "numeric")

# comprehenisive output
expect_silent(faceNames <- faces(gr))
expect_true(all(names(re)%in%faceNames))
expect_true(all(faceNames%in%names(re)))
expect_equal(sum(re), sol)


# force array output
expect_silent(reArray <- resample(r, gr, output="array"))
expect_equal(class(reArray), "array")
expect_true(all(names(reArray)%in%faceNames))
expect_true(all(faceNames%in%names(reArray)))
expect_equal(sum(reArray), sol)


# Facelayers are not supported for this (will be in newer version, where I will add a pointer here)
# wrong output
expect_error(reFL <- resample(r, gr, output="facelayer"))


