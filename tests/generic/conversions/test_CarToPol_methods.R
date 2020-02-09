context("CarToPol method dispatch")

# input structure 
test_that("Basic matrix-method", {
	expect_silent(out <<- CarToPol(xyz))
	expect_equal(class(xyz), class(out))
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out), nrow(xyz))

})

test_that("Numeric-method", {
	this <- xyz[1,]
	expect_silent(out <<- CarToPol(this))
	expect_equal(class(this), class(out))
	expect_equal(length(out), 3)
})

test_that("2column data.frame-method", {
	this <- as.data.frame(xyz)
	expect_silent(out <<- CarToPol(this))
	expect_equal(class(this), class(out))
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out), nrow(this))

})

