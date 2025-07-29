#' Export trigrid class object as Wavefront .obj file
#'
#' The function will take the given \code{trigrid} class object and write it's vertex, edge and face information as a .obj file
#'
#' Note that \code{hexagrid} class objects are exported in their triangulated form (subfaces). The order of faces for \code{hexagrid}s is not the natural (UI) order but the internal order of subfaces.
#' @param x A \code{trigrid} class object.
#' @param file A \code{character} path to a file to write.
#' @param scale A \code{logical} Should the grid vertices be scaled to unit diameter? Otherwise the values in kilometers will be exported.
#' @return The function has no return value.
#' @rdname saveOBJ
#' @exportMethod saveOBJ
#' @examples
#' gr <- hexagrid(spacing=4)
#' # example written into temporary directroy
#' td <- tempdir()
#' td
#' # actual writing
#' saveOBJ(gr, file=file.path(td, "hexagrid.obj")
#'
setGeneric(
	name="saveOBJ",
	package="icosa",
	def=function(x, ...){
		standardGeneric("saveOBJ")
	}
)

#' @rdname saveOBJ
setMethod(
	"saveOBJ",
	signature="trigrid",
	function(x, file, scale=TRUE){

		con <- file(file, "w")

		# filename
		filename <- unlist(lapply(strsplit(path, "/"), function(y) y[length(y)]))

		cat("# ",filename,"\n", file=con)
		#cat("\ng Object001\n\n",file=con)

		# prepare data
		# # verices
		if(scale){
			radius <- sqrt(sum(x@vertices^2))
			vert <- x@vertices/radius
		}else{
			vert <- x@vertices
		}
		for(i in 1:nrow(vert)){
			# get rid of names
			pure <- vert[i,]
			names(pure) <- NULL

			# add vertices
			cat("v ", pure, "\n", file=con)

		}
		cat("\n", file=con)

		# faces
		face<- NormalizeTriangles(triangles=x@faces, vertices=x@vertices, cents=x@faceCenters)

		for(i in 1:nrow(face)){
			# get rid of names and P
			pure <- face[i,]
			names(pure) <- NULL
			pure <- gsub("P", "", pure)

			cat("f ", pure, "\n", file=con)
		}

		cat("\n", file=con)

		close(con)

	}
)

#' @rdname saveOBJ
setMethod(
	"saveOBJ",
	signature="hexagrid",
	function(x, file, scale=TRUE){

		con <- file(file, "w")

		# filename
		filename <- unlist(lapply(strsplit(path, "/"), function(y) y[length(y)]))

		cat("# ",filename,"\n", file=con)
		# cat("\ng Object001\n\n",file=con)

		# prepare data
		# # verices
		if(scale){
			radius <- sqrt(sum(x@vertices^2))
			vert<-x@skeleton$plotV/radius
		}else{
			vert<-x@skeleton$plotV
		}

		# the number of vertices
		nVert <- nrow(vert)

		# export the actual vertices
		for(i in 1:nrow(vert)){
			# get rid of names
			pure <- vert[i,]
			names(pure) <- NULL

			# add vertices
			cat("v ", pure, "\n", file=con)

		}
		cat("\n", file=con)

		# the internal representation
		f<-x@skeleton$f[as.logical(x@skeleton$aSF),1:3]

		# faces
		face<- NormalizeTriangles(triangles=f+1, vertices=vert)

		for(i in 1:nrow(face)){
			# get rid of names and P
			pure <- face[i,]
			names(pure) <- NULL
			pure <- gsub("P", "", pure)

			cat("f ", pure, "\n", file=con)
		}

		cat("\n", file=con)

		close(con)

	}
)

# This function will normalize the orientation of face vertices
#
# @param triangles A character matrix of vertex names
# @param vertices A numeric matrix of vertex coordinates
# @param cents A numeric matrix of face center coordinates (ordered he same as triangles)
NormalizeTriangles <- function(triangles, vertices, cents=NULL){
	# trigrid--case
	## triangles <- gr@faces
	## vertices <- gr@vertices
	## cents <- gr@faceCenters

	# go through every single face
	for(i in 1:nrow(triangles)){
		# vertices of this faces
		thisVertices <- triangles[i,]

		# the center of the current face
		if(is.null(cents)){
			thisCenter <- apply(vertices[thisVertices, ], 2, mean)
		}else{
			thisCenter<- cents[i, ]
		}

		# the normal of the current face
		thisNormal <- Normal(A=vertices[thisVertices[1],], B=vertices[thisVertices[2],], C=vertices[thisVertices[3],])

		# direction correct?
		# dot product of face center and normal
		dot <- sum(thisNormal * thisCenter)

		# if this negative, flip the order (replace 2 with 3)
		if(dot<0){
			triangles[i, 2:3] <- triangles[i, 3:2]
		}

	}

	# return the object
	return(triangles)
}

#normed<- NormalizeTriangles(triangles=gr@faces, vertices=gr@vertices, cents=gr@faceCenters)


# Calculate th cross product of two 3D vectors, a and b
CrossProduct <- function(a, b){
	c(
		(a[2]*b[3] - a[3]*b[2]),
		(a[3]*b[1] - a[1]*b[3]),
		(a[1]*b[2] - a[2]*b[1])
	)
}

# Calculate the normal vector of a plane given by 3 points
Normal <- function(A, B, C){
	dir <- CrossProduct((B-A),(C-A))
	dir/sqrt(sum(dir^2))
}
