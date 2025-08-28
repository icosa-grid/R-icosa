# input: gr
expect_silent(surface <- surfacearea(gr))

# types
expect_true(inherits(surface, "numeric"))

# one for every face
expect_equivalent(length(gr), length(surface))

# names are there
faceNames <- names(surface)
expect_true(all(faces(gr)%in%faceNames))
expect_true(all(faceNames%in%faces(gr)))

# the sum is the surface of the sphere to the precision of 1km^2
expect_true(abs(sum(surface)- 4*pi*gr@r^2)<1)

