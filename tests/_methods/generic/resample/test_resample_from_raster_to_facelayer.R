# Resampling from SpatRaster to facelayer

# default ouptut: named numeric
expect_silent(re <- resample(r, gr))
# Define a facelayer from the input
expect_silent(fl<-facelayer(gr))
# facelayer method, defaults to the same as trigrid method
expect_silent(reFL <- resample(r, fl))
expect_identical(re, reFL)

# specificed array output
expect_silent(reArray <- resample(r, gr, output="array"))
# facelayer method, defaults to the same as trigrid method
expect_silent(reFLarray <- resample(r, fl, output="array"))
expect_identical(reArray, reFLarray)
