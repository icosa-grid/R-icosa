# standardized testing methods for facelayer construction consistency

# parameters:
# - gr: associated grid
# - vals: values added to the facelayer


# basic
expect_true(inherits(fl, "facelayer"))

# the grid is findable in a lower, undefined frame
expect_identical(get(fl@grid), gr)
expect_silent(flVals <- values(fl))

# all names are in the
# the face names of the grid
expect_silent(gridFaces <- faces(gr))
expect_silent(flNames <- names(fl))
expect_equal(gridFaces, flNames)

# names return the same as names attributes of values
#
expect_equal(names(flVals), flNames)

