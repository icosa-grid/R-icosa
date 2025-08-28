
# args:
# gr: "hexagrid" or "trigrid"
if(gr=="hexagrid"){
	construct <- hexagrid
	data(hexguide)
	guide <- hexguide
}
if(gr=="trigrid"){
	construct <- trigrid
	data(triguide)
	guide <- triguide
}

################################################################################
# Tessellation vector

# explicit tessellation vector
tess <- 4
expect_silent(tri <- construct(tessellation=tess))
expect_equal(tri@tessellation, tess)

# positional  determination
tess <- 4
expect_silent(tri <- construct(tess))
expect_equal(tri@tessellation, tess)

# missing value
tess <- NA
expect_error(tri <- construct(tessellation=tess))

# NULL
tess <- NULL
expect_error(tri <- construct(tessellation=tess))

################################################################################
# Edge length
el <- 8
expect_silent(tri <- construct(deg=el, verbose=FALSE))
elTess <- lookup(el, gr=gr, arg="deg", verbose=FALSE)
expect_identical(tri@tessellation, elTess)

# manually calculated edge length
expect_silent(meEL <-  mean(edgelength(tri, output="deg")))

# which is it?
index <- which(guide$meanEdgeLength_deg==round(meEL, 3))

# is it really the closest?
if(1 < index & index < nrow(guide)){
	compareWith <- c(index-1, index, index+1)
	absDiff <- abs(guide$meanEdgeLength_deg[compareWith] - el)
	# the second should be the smallest
	expect_equal(2L, which.min(absDiff))
}

# missing value
el <- NA
expect_error(tri <- construct(deg=el, verbose=FALSE))

################################################################################
# Spacing

# both spacing and edge length is given
expect_error(construct(deg=8, spacing=3))

# the space
space <- 8
expect_silent(tri <- construct(spacing=space, verbose=FALSE))
spaceTess <- lookup(space, gr=gr, arg="spacing", verbose=FALSE)
expect_identical(tri@tessellation, spaceTess)

# manually calculated edge length
expect_silent(mSpace <-  mean(spacing(tri, degree=TRUE)))

# which is it?
index <- which(guide$meanSpacing_deg==round(mSpace, 3))

# is it really the closest?
if(1 < index & index < nrow(guide)){
	compareWith <- c(index-1, index, index+1)
	absDiff <- abs(guide$meanSpacing_deg[compareWith] - space)
	# the second should be the smallest
	expect_equal(2L, which.min(absDiff))
}

# missing value
space <- NA
expect_error(tri <- construct(spacing=space, verbose=FALSE))
