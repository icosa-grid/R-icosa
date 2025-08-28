# run function 
expect_silent(out <- patches(gr, shape))

# output correctness
expect_equal(length(shape), length(out))
expect_true(all(names(out)%in% faces(gr) ))
expect_equal(sort(names(out)), sort(shape))
expect_equal(class(out), "numeric")
