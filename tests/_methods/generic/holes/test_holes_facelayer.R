# params:
# gr: trigrid
# fl: the facelayer to test
# shape: the names of assessed faces
# output from facelayer
# naked method (no facelayer wrapper)
expect_silent(out <- holes(gr, shape))
expect_silent(flout <- holes(fl))
expect_equal(out, flout)
