# 1. Complete distance matrices
expect_silent(dmat <- arcdistmat(points1=ps))
expect_equal(nrow(dmat), nrow(ps))
expect_equal(ncol(dmat), nrow(ps))
expect_equal(diag(dmat), rep(0, nrow(ps)))

# invert the matrix
inverted <- t(dmat)
expect_equal(inverted, dmat)

# 2. asymmetric with one
first <- ps[1,, drop=FALSE ]
others <- ps[2:nrow(ps),, drop=FALSE ]

# one vs many
expect_silent(set1 <- arcdistmat(first, others))
expect_equal(dim(set1), c(1, nrow(others)))
expect_equal(as.numeric(set1),as.numeric(dmat[2:nrow(dmat), 1]) )

# many vs one
expect_silent(set2 <- arcdistmat(others, first))
expect_equal(dim(set2), c(nrow(others),1))
expect_equal(as.numeric(set2),as.numeric(dmat[2:nrow(dmat)], 1) )

# 3. many vs many
firsts <- ps[1:3,, drop=FALSE ]
otherses<- ps[4:nrow(ps),, drop=FALSE ]

expect_silent(set3 <- arcdistmat(firsts, otherses))
expect_equal(dim(set3), c(nrow(firsts), nrow(otherses)))
expect_equal(as.numeric(set3[1,]),as.numeric(dmat[4:nrow(dmat),1]) )
expect_equal(as.numeric(set3[2,]),as.numeric(dmat[4:nrow(dmat),2]) )
expect_equal(as.numeric(set3[3,]),as.numeric(dmat[4:nrow(dmat),3]) )
