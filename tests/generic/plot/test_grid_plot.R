# no sf or sp slot
gr2 <- gr

# new @sp no @sf
expect_error(plot(gr2))

# Using the @sf
gr2 <- gr
expect_silent(gr2 <- newsf(gr2))

# the basic plot with no CRS
expect_silent(plot(gr2))
expect_silent(plot(gr2, crs="ESRI:54009"))
expect_silent(plot(gr2, crs=sf::st_crs("ESRI:54009")))

# Using the @SP
gr2 <- gr
expect_silent(gr2 <- newsp(gr2))

# the basic plot with no CRS
expect_silent(plot(gr2))
expect_silent(plot(gr2, crs="ESRI:54009"))
expect_silent(plot(gr2, crs=sf::st_crs("ESRI:54009")))

# if both present, the method should still work fine
expect_silent(gr2 <- newsf(gr2))
expect_silent(gr2 <- newsp(gr2))
expect_silent(plot(gr2))

