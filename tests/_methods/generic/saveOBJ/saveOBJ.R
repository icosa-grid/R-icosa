# input: gr
temp <- tempdir()
filename <- "grid.obj"
path <- file.path(temp, filename)

# make sure to remove this before checking
if(filename %in% list.files(temp)) file.remove(path)
# write out the file
expect_silent(saveOBJ(gr, file=path))
expect_true(filename %in% list.files(temp))

# automatically overwriting
expect_silent(saveOBJ(gr, file=path))

# can be read in with readOBJ
if(testRGL){
	expect_silent(mesh <- rgl::readOBJ(path))
	file.remove(path)
}
