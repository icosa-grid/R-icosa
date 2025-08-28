#' Add an igraph object to a predefined slot in an icosahedral grid
#'

#' @name newgraph
#		
#' @rdname newgraph
#' @return A new (\code{\link{trigrid}} or \code{\link{hexagrid}}) object with the recalculated graph.
#' @param gridObj (\code{\link{trigrid}}, \code{\link{hexagrid}}) An icosahedral grid.
#' @param ... Arguments passed to the \code{\link{gridgraph}} function.
#' @examples
#' #create a grid
#' g<-trigrid(4, graph=FALSE)
#' g<-newgraph(g)
#' 
#' @exportMethod newgraph
setGeneric(
	name="newgraph",
	package="icosa",
	def=function(gridObj,...){
		standardGeneric("newgraph")
	}

)

#' @rdname newgraph
setMethod(
	"newgraph",
	signature="trigrid",
	definition=function(gridObj,...){
		gridObj@graph<-gridgraph(gridObj,...)
		return(gridObj)
	
	}
)


#' Create or instantiate an \code{\link[igraph:make_graph]{graph}} class graph from the faces of an icosahedral grid
#'
#' The function can be applied to both grids and to \code{\link{facelayer}}-class object of \code{logical} values. The resulting graph will have the characteristics of the original grid (directed/undirected etc.). 
#' @name gridgraph
#' @return The function returns an undirected igraph graph.
#' @param x (\code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link{facelayer}}) The icosahedral grid or \code{\link{facelayer}}.
#' @param ... Arguments passed to the class specific methods.
#' @rdname gridgraph
#' @exportMethod gridgraph
setGeneric(
		name="gridgraph",
		def=function(x,...){
			standardGeneric("gridgraph")
		}
	
	)
	
#' Create or instantiate an 'igraph' class graph from the faces of a triangular grid
#'
#' @param directed \code{logical} Defaults to \code{FALSE}, creating an undirected graph. If \code{TRUE}, then the graph will be directed.
#' @param distances \code{logical} Defaults to \code{FALSE}. If \code{TRUE}, then the distances between the linked faces will be calculated and will be rendered to the edges as \code{"dist"}.
#' @rdname gridgraph
setMethod(
	f="gridgraph",
	signature="trigrid",
	definition= function(x, directed=FALSE,distances=FALSE){
		# if a graph object already exists in the grid
		if(!suppressWarnings(is.na(x@graph))[1]){
			gridGraph<-x@graph
		}else{
				
			# calculate the outer ordering
				# same format
				boolActFace <- x@skeleton$f[,4]==max(x@skeleton$f[,4])
				replaceSource<-x@skeleton$aF[boolActFace]
			
				# new order
				nOutOldIndex <- x@skeleton$n[x@skeleton$uiF,]
				nOutOldIndex <- nOutOldIndex +1
			
			
			nOut<-x@skeleton$n
			nOut[,1]<-replaceSource[nOutOldIndex[,1]]
			nOut[,2]<-replaceSource[nOutOldIndex[,2]]
			nOut[,3]<-replaceSource[nOutOldIndex[,3]]
			nOut[,4]<-replaceSource[nOutOldIndex[,4]]
			
			# c++ function to create an edgelist 
			edgeList <- .Call(Cpp_icosa_edgeListFromNeighbours_, nOut)
			
			# supress scientific notation!
			options(scipen=999)
			edgeListChar <- matrix(paste("F", edgeList, sep=""), ncol=2)
			options(scipen=0)
			
			# the arguments for the igraph function
		
			# get rid of the double edges
			edgeListChar <- unique(edgeListChar)
			
			
			# order the edges so they are not that messy
			edgeListChar<-edgeListChar[order(edgeListChar[,1]), ]
			
			
			# make a graph from that 
				graphArgs <- c(list(d=edgeListChar), list(directed=FALSE))
				
				gridGraph <- do.call(igraph::graph_from_data_frame, graphArgs)
		}	
		
		# depending on whether the directed argument was specified or not
		if(!is.null(directed)){
			if(directed){
				gridGraph<-igraph::as.directed(gridGraph)
				
			}
		
		}
		
		
		if(distances){
			edgeListChar<-igraph::get.edgelist(gridGraph)
			p0 <- x@faceCenters[edgeListChar[,1],]
			p1 <- x@faceCenters[edgeListChar[,2],]
			weights<- .Call(Cpp_icosa_ArcDistMany_, p0, p1, x@center, x@r)
			igraph::E(gridGraph)$dist <- weights
		}
		
		
		# subset, if necessary
			graph <- igraph::induced_subgraph(gridGraph, v=rownames(x@faces))
		
		return(graph)
})



#' Create or instantiate an 'igraph' class graph from the faces of a penta-hexagonal grid
#' @rdname gridgraph
setMethod(
	f="gridgraph",
	signature="hexagrid",
	definition= function(x, directed=FALSE,distances=FALSE){
	
	# get the edges of the original trigrid
		edgeListChar<-gsub("P", "F",x@skeleton$edgeTri)
		rownames(edgeListChar)<-NULL
	
		# depending on whether the directed argument was specified or not
		if(!is.null(directed)){
			if(directed==TRUE){
				# get rid of the double edges
				edgeList2<-cbind(edgeListChar[,2],edgeListChar[,1])
				edgeListChar<-rbind(edgeListChar,edgeList2)
			}
			
		
		}
		
		# order the edges so they are not that messy
		edgeListChar<-edgeListChar[order(edgeListChar[,1]), ]
		
		# make a graph from that 
			graphArgs <- c(list(d=edgeListChar), list(directed=directed))
			
			gridGraph <- do.call(igraph::graph_from_data_frame, graphArgs)
		
		
			if(distances){
				edgeListChar<-igraph::get.edgelist(gridGraph)
				p0 <- x@faceCenters[edgeListChar[,1],]
				p1 <- x@faceCenters[edgeListChar[,2],]
				weights<- .Call(Cpp_icosa_ArcDistMany_, p0, p1, x@center, x@r)
				igraph::E(gridGraph)$dist <- weights
			}
		
		
		# subset, if necessary
			graph <- igraph::induced_subgraph(gridGraph, v=rownames(x@faces))
		
		return(graph)

})



#' The neighbouring faces of faces in an icosahedral grid
#' 
#' This function will return neighbouring faces of the input faces. 
#' @name vicinity
#' 
#' @param gridObj (\code{\link{trigrid}} or \code{\link{hexagrid}}) Icosahedral grid object. 
#' 
#' @param faces (\code{character}) A vector specifying names of faces. 
#'
#' @param order (\code{numeric}) Passed to the \code{\link[igraph]{ego}} function, an integer value specifying the size of the neighborhood around a face.
#'	
#' @param output (\code{character}) The type of the output. The default \code{"vector"} 
#' 	will give back the names of the faces that adjacent to the faces specified, 
#' 	including themselves. \code{"list"} will return a list.
#'
#' @param self (\code{logical}) Flag indicating whether the input faces should be in the output. For the \code{"list"} output option, the input face names will be
#' omitted only from those character vectors that contain face names that are related to the face in question.
#'
#' @param namedorder (\code{logical}) Should the orders of the neighbouring cells be reported (\code{TRUE}) or just the names of the cells (default, \code{FALSE}).
#' @param ... Arguments passed to the \code{\link[igraph]{ego}} function.
#' @examples
#' g <- trigrid(3)
#' ne <- vicinity(g, c("F4", "F10"))
#' ne
#' 
#' @return A \code{character} vector or a \code{list} of \code{character} vectors.
#' 	
#' @exportMethod vicinity
#' @rdname vicinity
setGeneric(
	name="vicinity",
	def=function(gridObj,faces,...){
		standardGeneric("vicinity")
	}
)

#' @rdname vicinity
setMethod(
	"vicinity",
	signature=c("trigrid","character"),
	definition=function(gridObj,faces, order=1, output="vector",self=TRUE,namedorder=FALSE,  ...){
		#if no @graph found
		if(suppressWarnings(is.na(gridObj@graph)[1])){
			stop("Slot @graph is empty. Use newgraph() to add an igraph respresentation. ")
		}
		
		# get rid of NA entries
		faces<-faces[!is.na(faces)]
		
		# check whether the facein put is actually in the grid
		if(sum(!faces%in%rownames(gridObj@faces))!=0)
			stop("Invalid face name.")
		
		# get the actual vertices
		nList<-igraph::ego(gridObj@graph, order=order, faces,...)
		
		# get only the names of the faces
		newList<-lapply(nList, function(x){x$name})
		
		# should the orders of the neighbours be reported? if yes..
		if(namedorder){
			newStructure <- lapply(newList, function(x){
				temp <- rep(order, length(x))
				names(temp) <- x
				temp
			})

			# rerun the process for all the lower orders iterativels
			for(i in order:1){
				nList<-igraph::ego(gridObj@graph, order=i, faces,...)
		
				# get only the names of the faces
				nL<-lapply(nList, function(x){x$name})

				newStructure <- mapply(FUN=function(x,y){
					y[x] <- i
					return(y)
				},nL, newStructure)

			}

			newList <- lapply(newStructure, function(x){x[1] <- 0; return(x)})

		}


		if(!self & !namedorder){
			newList<-lapply(newList, function(x){x[-1]})
		}

		
		if(output=="list"){
			return(newList)
		}
		
		if(output=="vector"){
			temp <- unlist(newList)
			if(self){
				return(sort(unique(c(temp, faces))))
			}else{
				return(sort(temp[!temp%in%faces]))
			
			}
		}
	}
)

#' Make an spdep-style neighbor list for an icosahedral grid
#'
#' @param x A trigrid class object
#' @param queen Should he queen neighborhood be returned?
#' @return Neighbor-list object such as thoses defined in spdep.
#' @examples
#' # calculate a grid
#' hex <- hexagrid(deg=5)
#' neighborList <- face2nb(hex)
#' neighborList
#' @export
face2nb <- function(x, queen=FALSE){

	if(!inherits(x, "trigrid")) stop("The function requires an icosahedral grid object.")

	# default
	if(!(queen) | inherits(x, "hexagrid")){

		# calculate the neighbors
		faceList <- icosa::vicinity(gridObj=x, faces=faces(x), output="list", self=FALSE)

		# remove the face identifier
		faceInts <- lapply(faceList, function(x){
			sort(as.integer(gsub("F","", x)))
		})
	# for the queen trigrid method (slower)
	}else{

		# calculate the neighbors (3rd order)
		faceList <- icosa::vicinity(gridObj=x, faces=faces(x), output="list", self=TRUE, order=3)

		# remove the face identifier
		faceInts <- lapply(faceList, function(y){

			# focal vertices
			focal <- x@faces[y[1], ]

			# others
			other <- x@faces[y[-1], ]

			#
			thisVic <-apply(other, 1, function(b){
				any(b%in%focal)
			})

			res <- names(thisVic)[thisVic]


			sort(as.integer(gsub("F","", res)))
		})

	}

	# add attributes
	class(faceInts) <- "nb"
	attributes(faceInts)$region.id <- faces(x)
	if(queen){
		attributes(faceInts)$type <- "queen"
	}else{
		attributes(faceInts)$type <- "rook"
	}

	# return
	return(faceInts)
}


#' Holes of shapes on an icosahedral grid
#'
#' The function calculates the face names that represent holes in a surface shape
#'
#' The function uses the horizontal graph of a \code{\link{trigrid}}-class object, removes the subgraph corresponding to a set of faces (the shape),
#' and searches for isolated subgraphs. The largest subgraph (highest number of vertices, i.e. faces)  is considered to be outside of the shape.
#' This function relies on the \code{igraph} package to run.
#' @param x (\code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link{facelayer}}) An icosahedral grid or associated facelayer object.
#' @param y (\code{character}) Horizontal shapes defined as a character vector of face names.
#' @param outside (\code{logical}) Should the set of faces that are outside the shape be returned as well?
#' @param ... Arguments passed to class-specific methods.
#' @return A named numeric vector, names correspond to faces, numbers outline the holes. If \code{outside=FALSE} and there are no holes in the shape, the function will return \code{NULL}.
#' @examples
#'
#' # create a grid
#' hex <- hexagrid(2, sf=TRUE)
#' # an example shape
#' shape <- paste0("F", c(4, 5, 11, 13, 15, 21, 24, 26, 32, 33, 34, 35, 36))
#'
#' # visualize basic grid
#' plot(hex)
#' gridlabs(hex)
#'
#' # visualize the shape
#' plot(hex, shape, col="#FF000055", add=TRUE)
#'
#' # calculate holes
#' ho <- holes(x=hex, y=shape)
#'
#' # plot both holes
#' plot(hex, names(ho[ho==1]), add=TRUE, col="#00FF0055")
#' plot(hex, names(ho[ho==2]), add=TRUE, col="#0000FF55")
#' @exportMethod holes
#' @rdname holes
#' @name holes
setGeneric(
	name="holes",
#	package="icosa",
	def=function(x,...){
		standardGeneric("holes")
	}
)

#' @rdname holes
setMethod(
	"holes",
	signature=c(x="trigrid"),
		definition=function(x, y, outside=FALSE){

		if(suppressWarnings(is.na(x@graph)[1])){
			stop("Slot @graph of 'x' is empty. Use newgraph() to add an igraph respresentation. ")
		}

		# do tests for the validity of y
		if(!inherits(y, "character" )) stop("The given 'y' argument has to be a character vector.")
		if(!all(y%in%faces(x))) stop("The given 'y' argument needs to be the face names of 'x'.")

		# get the graph of the grid
		graph <- x@graph

		# the faces of the grid
		gridFaces <- faces(x)

		# vertices
		part <- unique(y)



		# validate face names
		if(any(!part %in% gridFaces)) stop("Invalid face names!")

		# not part
		notPart <- gridFaces[!gridFaces %in% part]

		# subGraph
		subg <- igraph::subgraph(graph, notPart)

		# the compoentns, i.e. unconnected bits
		isolates <- igraph::components(subg)

		# default output - no holes
		holeNew <- NULL

		# is the outside of the shape considered as a hole?
		# if no:
		if(!outside){
			# given that there are some holes
			if(isolates$no>1){
				# which is the largest? - defined as being outside the shape
				out <- which.max(isolates$csize)

				# the list of hole faces
				holeList <- isolates$membership[isolates$membership!=out]

				# re factorize to omit the outside part
				holeNew <- as.numeric(factor(holeList))
				names(holeNew) <- names(holeList)

			}
		}else{
			# return all sugraphs
			holeNew <-isolates$membership

		}

		return(holeNew)

	}
)

#' Patches of shapes on an icosahedral grid
#'
#' The function calculates the face names that represent patches in a surface shape
#'
#' The function uses the horizontal graph of a \code{\link{trigrid}}-class object, and searches for isolated subgraphs.
#' This function relies on the \code{igraph} package to run.
#' @param x (\code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link{facelayer}}) An icosahedral grid or associated facelayer object.
#' @param y (\code{character}) Horizontal shapes defined as a character vector of face names.
#' @param ... Arguments passed to class-specific methods.
#' @return A named numeric vector, names correspond to faces, numbers define the patches.
#' @examples
#'
#' # create a grid
#' hex <- hexagrid(2, sf=TRUE)
#' # an example shape
#' shape <- paste0("F", c(3,6,7,9, 10, 16, 22, 26))
#'
#' # visualize basic grid
#' plot(hex)
#' gridlabs(hex)
#'
#' # visualize the shape
#' plot(hex, shape, col="#FF000055", add=TRUE)
#'
#' # calculate holes
#' pa <- patches(x=hex, y=shape)
#'
#' # plot all patches (coloring borders)
#' plot(hex, names(pa[pa==1]), add=TRUE, border="#00FF00", lwd=4)
#' plot(hex, names(pa[pa==2]), add=TRUE, border="#0000FF", lwd=4)
#' plot(hex, names(pa[pa==3]), add=TRUE, border="#00FFFF", lwd=4)
#' @exportMethod patches
#' @rdname patches
#' @name patches
if(requireNamespace("terra", quietly = TRUE)){
	setGeneric("patches", def=terra::patches)
}else{
	setGeneric(
		name="patches",
		def=function(x,...){
			standardGeneric("patches")
		}
	)
}



#' @rdname patches
setMethod(
	"patches",
	signature=c(x="trigrid"),
		definition=function(x, y, ...){

		if(suppressWarnings(is.na(x@graph)[1])){
			stop("Slot @graph of 'x' is empty. Use newgraph() to add an igraph respresentation. ")
		}

		# do tests for the validity of y
		if(!inherits(y, "character" )) stop("The given 'y' argument has to be a character vector.")
		if(!all(y%in%faces(x))) stop("The given 'y' argument needs to be the face names of 'x'.")

		# get the graph of the grid
		graph <- x@graph

		# vertices
		part <- unique(y)

		# subGraph
		subg <- igraph::subgraph(graph, part)

		# the compoentns, i.e. unconnected bits
		isolates <- igraph::components(subg)

		# return
		return(isolates$membership)

	}
)
