# Test when the raster is finer resolution than the trigrid
expect_silent(oc <- occupied(gr,r))
expect_true(inherits(oc, "logical"))
rastCoords <- xyFromCell(r, 1:ncell(r))[!is.na(values(r)),]

# this should be the same as the result of locate
expect_silent(located <- locate(gr, rastCoords))
expect_equal(sort(unique(located)), sort(names(oc[oc])))


# facelayer output
expect_silent(fl <- occupied(gr,r, out="facelayer"))
expect_true(inherits(fl, "facelayer"))
expect_equal(sort(names(fl)[values(fl)]), sort(unique(located)))
