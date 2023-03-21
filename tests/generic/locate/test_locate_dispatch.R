message("locate() method dispatch for longlat")


# trigrid-basic matrix method
	expect_silent(cells <<- locate(gr, xy))

# trigrid-numeric method
	this<- xy[1,]
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells[1], thisRes)

# trigrid-data-frame method
	this<- as.data.frame(xy)
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)

# trigrid-sf method, no projection
	this <- sf::st_as_sf(this, coords=c("long", "lat"))
	expect_silent(thisRes <- locate(gr, this))
	expect_equal(cells, thisRes)

# trigrid-sf method, no with Projection
	this <- sf::st_as_sf(this, coords=c("long", "lat"))
	sf::st_crs(this) <- "WGS84"
	expect_silent(thisRes <- locate(gr, this))
	expect_equal(cells, thisRes)

# trigrid-SpatialPoints-method, no projection
	this <- sp::SpatialPoints(xy)
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)


# trigrid-SpatialPoints-method,  projection
	this <- sp::SpatialPoints(xy)
	this@proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
	this <- sp::spTransform(this, sp::CRS("+proj=moll"))
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)


# trigrid-SpatialPoints-method, no projection
	this <- sp::SpatialPoints(xy)
 	this <- sp::SpatialPointsDataFrame(this, data=data.frame(1:length(this)))
	expect_silent(thisRes <<- locate(gr, this))
	expect_equal(cells, thisRes)





