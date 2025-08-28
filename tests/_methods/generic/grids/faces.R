# test for a suite of grids in the tessellation guide

# should be the same as this
expect_equal(rownames(one@faces),faces(one))

# replacement should not be allowed!
expect_error(faces(one) <- paste("B", 1:nrow(one@faces)))
