context("locate() method dispatch for longlat")


test_that("trigrid-basic matrix method",{
	expect_silent(cells <<- locate(gr, xy))
})

test_that("trigrid-numeric method", {
	this<- xy[1,]
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells[1], thisRes)
})

test_that("trigrid-data-frame method", {
	this<- as.data.frame(xy)
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)
})


test_that("trigrid-SpatialPoints-method, no projection", {
	this <- sp::SpatialPoints(xy)
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)
})


test_that("trigrid-SpatialPoints-method,  projection", {
	this <- sp::SpatialPoints(xy)
	this@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
	this <- sp::spTransform(this, sp::CRS("+proj=moll"))
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)
})




