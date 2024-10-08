library(tinytest)
library(icosa)

# basic argumentation and method dispatch tests required. Should work for every plot


# create grid
b  <- hexagrid(4, sf=TRUE)

################################################################################
# Numerics
########################################----------------------------------------
# simple numbers
n <- 15
# dummy data
numb <- rnorm(n)

# on random faces
names(numb) <- sample(faces(b), length(numb))

# should not produce errors
expect_silent(plot(b, numb))


########################################----------------------------------------
# Tables
########################################----------------------------------------
# generate random points
rand<- rpsphere(50, output="polar")

# locate
cells <- locate(b, rand)

# the table
tCells <- table(cells)

# should not produce errors
expect_silent(plot(b, tCells))

########################################----------------------------------------
# Single-dim arrays
########################################----------------------------------------
d<- surfacearea(b)

#
expect_silent(plot(b, d))

########################################----------------------------------------
# Wrong faces
########################################----------------------------------------
badNumb <- numb
names(badNumb)[1] <-"A"
expect_error(plot(b, badNumb))

################################################################################
# Logical 'subscripts'
################################################################################


########################################----------------------------------------
# Good
########################################----------------------------------------
logi <- rep(TRUE, length(faces(b)))
logi[10] <- FALSE
expect_silent(plot(b, logi, col="blue"))


########################################----------------------------------------
# Wrong length
########################################----------------------------------------
logiBad <- logi[2:length(logi)]
expect_error(plot(b, logiBad))

########################################----------------------------------------
# Missing value
########################################----------------------------------------
logiBad2 <- logi
logiBad2[5] <- NA
expect_error(plot(b, logiBad2))


################################################################################
# Character 'subscripts'
################################################################################

########################################----------------------------------------
# 1. Unnamed - Good subset
########################################----------------------------------------
expect_silent(plot(b, cells, col="red"))

########################################----------------------------------------
# 1. Unnamed - Bad cell name
########################################----------------------------------------
badcells <- c(cells, "D")
expect_error(plot(b, badcells))


########################################----------------------------------------
# 2. Named - Good subset
########################################----------------------------------------

# named character data
dummy <- c(rep("a", 5), rep("b", 4))
names(dummy) <- faces(b)[1:length(dummy)]
expect_silent(plot(b, dummy))
# logical

########################################----------------------------------------
# 2. Named - Bad subset
########################################----------------------------------------

dummy2 <- dummy
names(dummy2)[2] <- "A"
expect_error(plot(b, dummy2))
# logical
