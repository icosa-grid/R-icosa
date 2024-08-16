plot(NULL, NULL, xlim=c(-180, 180), ylim=c(-90,90))
points(ps)
expect_silent(arcs(ps))

# broken return
expect_silent(ret <- arcs(ps, plot=FALSE))

# single return
breaks <- 100
expect_silent(nogaps <- arcs(ps, plot=FALSE, breaks=breaks, breakAtDateline=FALSE))
expect_equal(nrow(nogaps), (nrow(ps)-1)*breaks+nrow(ps))
#
