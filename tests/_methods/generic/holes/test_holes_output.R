# run function
expect_silent(out <- holes(gr, shape))

# output correctness
expect_equal(class(out), "numeric")
expect_true(all(names(out)%in% faces(gr) ))
