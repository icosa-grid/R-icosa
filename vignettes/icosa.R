## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(icosa)
library(rgl)
knit_hooks$set(rgl = hook_rgl)

## ----first, rgl=TRUE,dev='png',dpi=300, fig.width=6, fig.height=4--------
# create a trigrid class object
tri <- trigrid()

# the show() method displays basic information
tri

# plot the object in 3d
plot3d(tri, guides=F)

## ----crs, rgl=TRUE,dev='png',dpi=300-------------------------------------
tri@proj4string

## ----trigrid, rgl=TRUE,dev='png',dpi=300---------------------------------
# create a trigrid class object
gLow <- trigrid(tessellation=c(4,4))

# plot the object in 3d
plot3d(gLow, guides=F)

