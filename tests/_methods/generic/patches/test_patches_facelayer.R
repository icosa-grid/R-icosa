# params:
# gr: trigrid
# fl: the facelayer to test
# shape: the names of assessed faces
# output from facelayer
# naked method (no facelayer wrapper)
expect_silent(out <- patches(gr, shape))
expect_silent(flout <- patches(fl))
expect_equal(out, flout)
