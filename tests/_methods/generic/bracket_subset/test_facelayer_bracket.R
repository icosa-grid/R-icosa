
# structure really correct
expect_equal(length(values(out)),length(names(out)))

# linked grid is identical
expect_equal(out@grid,fl@grid)

# all names in output facelayer are in original
expect_true(all(names(out)%in%names(fl)))

# subsetting actually happened
expect_true(length(values(out))<=length(faces(gr)))
