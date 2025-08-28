gr <- newsf(gr)
plot(gr)
expect_silent(gridlabs(gr))

plot(gr)
expect_silent(gridlabs(gr, type="v"))

plot(gr)
expect_silent(gridlabs(gr, type="e"))

# projetion change
proj <- "ESRI:54009"
plot(gr, crs=proj)
expect_silent(gridlabs(gr, crs=proj))

# argument passing
proj <- "ESRI:54009"
plot(gr, crs=proj)
expect_silent(gridlabs(gr, crs=proj, col="red"))
