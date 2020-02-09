context("PolToCar method dispatch")

# input structure 
test_that("Basic matrix-method", {
	expect_silent(out <<- PolToCar(xy))
	expect_equal(class(xy), class(out))
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out), nrow(xy))

})

test_that("Numeric-method", {
	this <- xy[1,]
	expect_silent(out <<- PolToCar(this))
	expect_equal(class(this), class(out))
	expect_equal(length(out), 3)
})

test_that("2column data.frame-method", {
	this <- as.data.frame(xy)
	expect_silent(out <<- PolToCar(this))
	expect_equal(class(this), class(out))
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out), nrow(this))

})

test_that("multi data.frame-method", {
	this <- as.data.frame(xy)
	this <- cbind(this, one="garbage", two="dummy")

	expect_error(out <<- PolToCar(this))

	expect_error(out <<- PolToCar(this, lat="one"))
	expect_error(out <<- PolToCar(this, lat="two"))
	
	# wrong columns
	expect_error(out <<- PolToCar(this, long="oneish", lat="two"))
	expect_error(out <<- PolToCar(this, long="one", lat="twoish"))

	expect_silent(out <<- PolToCar(this, long="long", lat="lat"))

	expect_equal(class(this), class(out))
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out), nrow(this))

})
