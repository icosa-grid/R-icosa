#package namespace variables
	origin<-c(0,0,0)
	authRadius<-6371.0071810 # authalic radius (R2) based on Moritz, 1980
	meanRadius<-6371.0087714 # mean radius (R1) based on Mortiz, 1980
	volRadius <-6371.0007900 # radius of sphere of same volume
#' The vertices of an icosahedral grid object
#'
#' Shorthand function to return the vertices slot of an icosahedral grid or a grid linked to a facelayer. 
#' @name vertices
#' @param x The grid or facelayer object.
#' @param output the coordinate system of the output.
#' @rdname vertices-methods
#' @exportMethod vertices
setGeneric(
	name="vertices",
	def=function(x,output){
		standardGeneric("vertices")
	}
)

#' @rdname vertices-methods
#' @exportMethod vertices
setMethod(	
	f="vertices",
	signature=c("trigrid","character"),
	definition= function(x, output="polar"){
		if(output=="polar"){
			return(CarToPol(x@vertices, origin=x@center, norad=TRUE))
		
		}else{
			return(x@vertices)
		}
	}
)

#' The faces of a 3d object
#'
#' Shorthand function to get the faces slot of an icosahedral grid or a grid linked to a facelayer. 
#' @param x The grid or facelayer object.
#' @name faces
#' @rdname faces-methods
#' @exportMethod faces
setGeneric(
	name="faces",
	def=function(x){
		standardGeneric("faces")
	}
)

#' @rdname faces-methods
#' @exportMethod faces
setMethod(	
	f="faces",
	signature="trigrid",
	definition= function(x){
		return(x@faces)
	}
)



#' The edges of a 3d object
#'
#' Shorthand function to get the edges slot of an icosahedral grid or a grid linked to a facelayer. 
#' @param x The grid or facelayer object.

#' @name edges
#' @rdname edges-methods
#' @exportMethod edges
setGeneric(
	name="edges",
	def=function(x){
		standardGeneric("edges")
	}
)

#' @rdname edges-methods
#' @exportMethod edges
setMethod(	
	f="edges",
	signature="obj3d",
	definition= function(x){
		return(x@edges)
	}
)

#' @rdname edges-methods
#' @exportMethod edges
setMethod(	
	f="edges",
	signature="facelayer",
	definition= function(x){
		actGrid<-get(x@grid)
		return(actGrid@edges)
	}
)

#' The face centers of an icosahedral grid object
#'
#' Shorthand function to return the faceCenters slot of an icosahedral grid or a grid linked to a facelayer. 
#' @name centers
#' @param x The grid or facelayer object.
#' @param ... arguments passed to the class specific methods.
#' @rdname centers-methods
#' @exportMethod centers
setGeneric(
	name="centers",
	def=function(x,...){
		standardGeneric("centers")
	}
)
#' The face centers of a trigrid or hexagrid class object
#'
#' Shorthand function to return the faceCenters slot of an icosahedral grid . 
#
#' @param output the coordinate system of the output. Either "polar" or "cartesian".
#' @rdname centers-methods
#' @aliases centers-trigrid-method
#' @exportMethod centers
setMethod(	
	f="centers",
	signature="trigrid",
	definition= function(x, output="polar"){
		if(output=="polar"){
			return(CarToPol(x@faceCenters, origin=x@center, norad=TRUE))
		
		}else{
			return(x@faceCenters)
		}
	}
)
	
	
# S4 class definitions
# the core class! -  no need to export
obj3d <- setClass(
	#class
	"obj3d",
	
	slots=c(
		vertices = "matrix",
		faces = "matrix",
		edges = "matrix",
		center = "numeric",
		length = "numeric"
	)
)

setMethod(	
	f="show",
	signature="obj3d",
	definition= function(object){
		cat(paste("A/An ", class(object), " object with ", 
			object@length[1], " vertices, ",
			object@length[2], " edges and ",
			object@length[3], " faces.\n",
			sep=""))
		if(class(object)%in% c("trigrid", "hexagrid")){
			cat(paste("The mean grid edge length is ", 
			round(object@edgeLength[1],2), " km or ",
			round(object@edgeLength[2],2), " degrees.\n", sep=""))
		}
		cat("Use plot3d() to see a 3d render.\n")
	}
)

#' The length of a trigrid or, hexagrid class object.
#'
#' The length of the object is interpreted as the number of faces it contains.
#'
#' @param x the object.
#' @return An integer value
#' @rdname length-methods
#' @aliases trigrid-length-method
#' @exportMethod length
setMethod(	
	f="length",
	signature="trigrid",
	definition= function(x){
		return(x@length[3])
	}
)


#the icosahedron
icosahedron <- setClass(
	"icosahedron",
	
	slots = c(
		edgeLength = "numeric",
		skeleton = "list",
		r = "numeric",
		sp="ANY"
	),
	
	contain="obj3d"
)

#constructor of icosahedron
setMethod(
	"initialize",
	signature = "icosahedron",
	definition = function (.Object, r=FALSE,a=FALSE){
		# calculate the missing thing
		if(missing(a))
		{
			.Object@r<-r
			.Object@edgeLength<-r/(sin(2*pi/5))
		}
		if(missing(r))
		{
			.Object@edgeLength<-a
			.Object@r<-a*(sin(2*pi/5))
		}
		
		phi<-0.5*(1+sqrt(5))
		
		P1<-c(0,1,phi)/2
		P2<-c(0,1,-phi)/2
		P3<-c(0,-1,phi)/2
		P4<-c(0,-1,-phi)/2
		
		P5<-c(1,phi,0)/2
		P6<-c(1,-phi,0)/2
		P7<-c(-1,phi,0)/2
		P8<-c(-1,-phi,0)/2
		
		P9<-c(phi,0,1)/2
		P10<-c(phi,0,-1)/2
		P11<-c(-phi,0,1)/2
		P12<-c(-phi,0,-1)/2
		
		vertices<-rbind(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12)*.Object@edgeLength
		
		#maximum precision achieved by rotation
		rotVal<-0.55357435889704526002
		vertices<-t(apply(vertices, 1, rotateOnePoint, angles=c(rotVal,0,0), origin=c(0,0,0)))

		colnames(vertices)<-c("x","y","z")
		
		#the edges
		edges<-NULL
		for(i in 1:nrow(vertices))
		{
			for (j in 1:nrow(vertices))
			{
				A<-unlist(vertices[i,])
				B<-unlist(vertices[j,])
				mTwo<-matrix(c(A,B),ncol=3, byrow=TRUE)
				d<-stats::dist(mTwo)
				if(round(d-.Object@edgeLength,10)==0)
				{
					E<-c(rownames(vertices)[i], rownames(vertices)[j])
					E<-sort(E)
					edges<-rbind(edges, E)
				}
			
			
			
			}
			
		}
		edges<-unique(edges)
		rownames(edges)<-paste("E", 1:nrow(edges), sep=(""))
		
		faces<-NULL
		#the faces
		for(i in 1:nrow(edges))
		{
			actEdge<-unlist(edges[i,])
			
			#P1
			b1<-edges[,1]%in%actEdge[1] | edges[,2]%in%actEdge[1]
			
			u1<-unique(c(edges[b1,1], edges[b1,2]))
			
			#P3
			b2<-edges[,1]%in%actEdge[2] | edges[,2]%in%actEdge[2]
			
			u2<-unique(c(edges[b2,1], edges[b2,2]))
			#double faces
				chDoub<-u1[u1%in%u2]
			#new points
				chNew<-chDoub[!chDoub%in%actEdge]
			
			#new lines in the faces
			#the first new point
			F<-sort(c(actEdge, chNew[1]))
			faces<-rbind(faces,F)
			
			#the second new point
			F<-sort(c(actEdge, chNew[2]))
			faces<-rbind(faces,F)
			
				
		}
		faces<-unique(faces)
		rownames(faces)<-paste("F",1:nrow(faces), sep="")
		
		.Object@faces<-faces
		.Object@edges<-edges
		.Object@vertices<-vertices
		.Object@center <- c(0,0,0)
		.Object@length<- c(
			"vertices"=nrow(vertices),
			"edges"=nrow(edges),
			"faces"=nrow(faces))
			
		# the skeleton:
			#vertices
			v<-vertices
			rownames(v)<-NULL
			
			#faces
			f<-matrix(NA, nrow=20,ncol=5)
			fTemp<-as.numeric(unlist(lapply(strsplit(as.character(faces),"P"), function(x){x[2]})))
			f[,1:3]<-matrix(fTemp, ncol=3)-1
			f[,4] <- rep(-1,nrow(f))
			f[,5] <- rep(-1,nrow(f))
		
	
		.Object@skeleton<-list(v=v, f=f)
		
		return(.Object)
	}
)


#' A triangular icosahedral grid
#' 
#' \code{trigrid} creates a triangular grid based on the
#'    tessellation of an icosahedron.
#'
#' The grid structure functions as a frame for data graining, plotting and spatial
#'	calculations. Data can be stored in layers that are linked to the grid object. In the current version only the 
#'	\code{facelayer} class is implemented, which allows the user to render data to the cells
#'	of the grid, which are usually referred to as faces. 
#'	The grid 'user interface' is made up of four primary tables: the \code{@vertices} table for the coordinates of the vertices,
#' 	the \code{faceCenters} for the coordinates of the centers of faces,
#'	the \code{faces} and the \code{edges} tables that contain which vertices form which faces and edges respectively.
#'	In these tables, the faces and vertices are sorted to form spirals that go from the north pole in a counter-clockwise
#'	direction. In case grid subsetting is performed these tables get truncated.
#'	
#'	At finer resolutions, the large number of spatial elements render all calculations resource demanding and slow, 
#'	therefore the hierarchical structure created during the tessellation procedure is retained for efficient implementation.
#'	These data are stored in a list in the slot \code{@skeleton} and are 0-indexed integer tables for Rccp-based functions. \code{$v} 
#'	stores vertex, \code{$f} the edge, and \code{$e} contains the edge data for plotting and calculations. In these tables
#'	the original hierarchy based orderings of the units are retained, during subsetting, additional vectors are used to indicate
#'	deactivation of these units. Any sort of meddling with the @skeleton object will lead to unexpected behavior.
#'	
#' @slot vertices Matrix of the vertex XYZ coordinates.
#'
#' @slot faces Matrix of the verticies forming the faces.
#'
#' @slot edges Matrix of the vertices forming the edges.
#'
#'	@slot tessellation Contains the tessellation vector.
#'
#'	@slot orientation Contains the grid orientation in xyz 3d space, values in radian relative to the (0,1,0) direction.
#'
#'	@slot center is the xyz coordinates of the grids origin/center.
#'
#'	@slot div vector contains the number of faces that a single face of the previous tessellation level is decomposed to.
#'
#'	@slot faceCenters contains the xyz coordinates of the centers of the faces on the surface of the sphere.	
#'	@slot belts Vector of integers indicating the belt the face belongs to.
#' @slot edgeLength the length of an average edge in km and degrees.
#' @slot graph an 'igraph' class graph object.
#' @slot length integer vector of length=3. The number of vertices, edges and faces in this order.
#' @slot proj4string a CRS class object indicating the model in the PROJ.4 system
#' @slot r the radius of the grid
#' @slot sp The SpatialPolygons representation of the grid. If missing, it can be created with newsp().
#' @slot skeleton data tables with sequential indexing for the C functions.
#'
#'
#' @param tessellation An integer vector with the tessellation values. Each number
#'    describes the number of new edges replacing one original edge. Multiple series of tessellations
#' 	  are possible this way. The total tessellation is the product of the tessellation vector. 
#'	  Higher values result in more uniform cell sizes, but the larger number of tessellation series
#'	  increases the speed of lookup functions.
#'
#' @param sp A logical value indicating whether the 'SpatialPolygons' class representation of the grid
#'	should be added to the object when the grid is calculated. If set to \code{TRUE} the \code{SpPolygons()} function will be run with with the resolution parameter set to 25. The 
#'  resulting object will be stored in slot \code{@sp}. As the calculation of this object can substantially increase the grid creation time,
#'	 by default this argument has a value of \code{FALSE}. The 'SpatialPolygons' class representation can be added on demand by running the function \code{newsp}.
#'
#' @param graph A logical value indicating whether the \code{'igraph'} class representation of the grid
#'	should be added to the object when the grid is calculated. This argument defaults to TRUE because this option has only minor performance load on the grid 
#' constructor function. For familiarization with the
#' object structure, however, setting this parameter to \code{FALSE} might help, as invoking \code{str()} on the \code{'igraph'} class slot of the class might flood the console.
#'
#' @param radius The radius of the grid. Defaults to the authalic radius of Earth.
#' @param center The origin of the grid in the reference Cartesian coordinate system. Defaults to (0,0,0).
#' @rdname trigrid-class
#' @name trigrid
#' @aliases trigrid-class
#'
#' @return A triangular grid object, with class \code{trigrid}.
#' @examples
#' 	# single tessellation value
#'	g <- trigrid(c(8))
#'	g
#'	# series of tessellations
#' 	g1 <- trigrid(c(2,3,4))
#'	g1
#' @exportClass trigrid
trigrid<-setClass(
	"trigrid",
	contain="icosahedron",
	slots=c(
		tessellation="numeric",
		div="numeric",
		faceCenters="matrix",
		orientation="numeric",
		belts="numeric",
		proj4string="CRS",
		graph="ANY"
	)
)



#' @rdname trigrid-class
#' @aliases trigrid-initalize-method
#' @param .Object non-argument pointing to self.
#' @exportMethod trigrid
setMethod(
	"initialize",
	signature="trigrid",
	definition=function(.Object, tessellation=1, sp=FALSE, graph=TRUE, radius=authRadius, center=origin){
		
		# trial variables
	#	tessellation<-c(2,2,2,2)
	#	authRadius<-6371
	#	degree<-1
	#	fa<-4
		##check the tessellation vector
		if(!sum(tessellation%%1)==0 | !is.numeric(tessellation))
		{
			stop("Invalid tessellation vector. Please enter a vector of integers only.")
		}
		if(prod(tessellation)==0){
			stop("Invalid tessellation vector. Please enter a vector of integers without 0s.")
		}
		

		# create a basic icosahedron
			icosa<-icosahedron(r=radius)
		
			# the final trigrid object
			.Object@tessellation <-tessellation
			.Object@r <- radius
			.Object@center <- icosa@center
			
			# add the CRS for spatial transformations
			#supress scientific notation
			options(scipen=999)
			.Object@proj4string <- sp::CRS(paste("+proj=longlat +a=", round(radius*1000), " +b=", round(radius*1000), sep=""))
			
			
		# extract the skeleton	
			f=icosa@skeleton$f
			v=icosa@skeleton$v
		
		# in case a tessellation is happening
		if(prod(tessellation)>1){
			# invoke the c++ function to create a grid
			newGrid<-.Call(Cpp_icosa_IcosahedronTesselation_, 
				v,
				f, 
				tessellation, 
				icosa@center)
		}else{
			newGrid <-list(f=f, v=v)
		}
			
		
		# additional data
			# the divisions
			div <- c(20, tessellation^2)
			
			# in case of the icosahedron
			if(prod(tessellation)==1){
				div<-20
			}
			
			.Object@div <- div
	
		# calculate the edges
			# boolean variables for the subsetting
			if(prod(tessellation)>1){
				aF <- newGrid$f[,4]==(length(tessellation)-1)
			# for special case of the icosahedron
			}else{
				aF <- newGrid$f[,4]==-1
			}
			offSet<-min(which(aF))-1
			
			aV <- rep(T, nrow(newGrid$v))
		
			faces<-subset(newGrid$f, aF)[,1:3]
			# edges
			e<-unique(.Call(Cpp_icosa_expandFacesToEdges_, faces))
			aE <- rep(T, nrow(e))
			# the neighbours of the faces
			n<-.Call(Cpp_icosa_AllNeighboursTri_, newGrid$f[,1:3], div)
			
		
		# the R grid UI
		# vertices
			vertices<- newGrid$v
			colnames(vertices) <- c("x","y","z")
			
		
		# the centers of the triangles
			faceCenters<-.Call(Cpp_icosa_allTriangleCenters_, newGrid$v, faces, icosa@center)
			colnames(faceCenters)<-c("x","y","z")
			
	
		#ordering the face and vertex data
			#vertex at the very north
				topVert<- which(max(vertices[,3])==vertices[,3])-1
			
			#top five faces
				firstFiveFace<-order(faceCenters[,3], decreasing=TRUE)[1:5]
				#ordered by longitude
				longOrd<-order(CarToPol(faceCenters[firstFiveFace,], norad=TRUE, origin=icosa@center)[,"long"])
				startFaces<-firstFiveFace[longOrd]
			
			#starting vertices
				startF<-faces[startFaces,]
				
				startVert<-numeric(6)
				startVert[1]<-topVert
				startVert[2]<-startF[1,c(F,startF[1,2:3]%in%startF[5,2:3])]
				
				for(i in 1:4){
					startVert[i+2]<-startF[i,!startF[i,]%in%c(startVert)]
				}
				
			# arguments for the ordering function	
				nBelts<-prod(tessellation)*3
				# in case the  thing is an icosahedron
				
				nV<-nrow(vertices)
				startFaces<-startFaces-1
			
			#call
				ordering<-.Call(Cpp_icosa_orderTriGrid_, faces, n, startFaces, startVert, nBelts, nV)
			
			# the belts
				# where do the belts start
				beltStartsAndEnd<-c(ordering$belts+1, nrow(faces))
				
				#empty container
				belts<-rep(NA, nrow(faces))
				
				#for every belt
				for(i in 1:(length(beltStartsAndEnd)-1)){
					
					if(i<(length(beltStartsAndEnd)-1)){
						actInd<-beltStartsAndEnd[i]:(beltStartsAndEnd[i+1]-1)
					}else{
						actInd<-beltStartsAndEnd[i]:(beltStartsAndEnd[i+1])
					}
					#store
					belts[actInd]<-i
				}

				.Object@belts<-belts
			#output formatting (R indices)
				#UI-skeleton translation
				faceOrder<-ordering$faceOrder+1 # good
				names(faceOrder)<-paste("F", 1:nrow(faces), sep="") #good
				
				vertexOrder<-ordering$vertexOrder+1
				names(vertexOrder)<-paste("P", 1:nrow(vertices), sep="") #good
				
				#skeleton-UI translation (R indicies)
				faceInvertOrder<-rep(0, length(faceOrder))
				faceInvertOrder[faceOrder]<-1:length(faceOrder)
				
				vertexInvertOrder<-rep(0, length(vertexOrder))
				vertexInvertOrder[vertexOrder]<-1:length(vertexOrder)
				
				
				aF[aF]<-faceInvertOrder #not good
				aV[aV]<-vertexInvertOrder#not good
				
			
			# the skeleton
				.Object@skeleton <- list(v=newGrid$v,aV=aV, uiV=vertexOrder, e=e, aE=aE,  f=newGrid$f, aF=aF, uiF=faceOrder, n=n, offsetF=offSet)
#				.Object@skeleton <- list(v=newGrid$v,aV=aV, e=e, aE=aE,  f=newGrid$f, aF=aF)
			
			#using the output to order
			vertices<-vertices[vertexOrder,]
			rownames(vertices) <- paste("P",1:nrow(vertices) ,sep="")
		
			# the edges (outer ordering)
			tempE<-aV[as.numeric(e+1)]
			
			edges<-matrix(paste("P",tempE, sep=""), ncol=2)
			rownames(edges)<-paste("E", 1:nrow(edges), sep="")
			
			# the faces
			faces<-faces[faceOrder,]
			#translate the vertex information!
			facesNum<-as.numeric(faces)+1
			faces2<-aV[facesNum]
			
			faces<-matrix(paste("P",faces2, sep=""), ncol=3)
			rownames(faces)<-paste("F", 1:nrow(faces), sep="")
		
			#the faceCenters
			faceCenters<-faceCenters[faceOrder,]
			rownames(faceCenters)<-paste("F", 1:nrow(faceCenters), sep="")
			
			options(scipen=0)
			
			#add to the object
			.Object@faces <- faces
			.Object@vertices <- vertices
			.Object@edges <- edges
			.Object@faceCenters <- faceCenters
		
		#length attribute
		.Object@length<- c(
			"vertices"=nrow(.Object@vertices),
			"edges"=nrow(.Object@edges),
			"faces"=nrow(.Object@faces))
			
		.Object@edgeLength <- c(
			mean(.Call(Cpp_icosa_edges_, newGrid$v, e, icosa@center, 1)),
			mean(.Call(Cpp_icosa_edges_, newGrid$v, e, icosa@center, 0))/pi*180)
			
		names(.Object@edgeLength) <- c("km", "deg")
		
		.Object@orientation<-c(0,0,0)
		
		# add the igraph of the grid
		dummy<-NA
		.Object@graph<-dummy
		
		if(graph==TRUE){
				.Object@graph<-gridgraph(.Object)
		}
		
		#2d grid!
		if(sp==TRUE){
				.Object@sp<-SpPolygons(.Object, res=25)
		}else{
			dummy<-NA
			
			.Object@sp<-dummy
		}
		# correct the coordinates with the center data
		for(vc in 1:3){
			.Object@vertices[,vc]<-.Object@vertices[,vc]+center[vc]
			.Object@faceCenters[,vc]<-.Object@faceCenters[,vc]+center[vc]
			.Object@skeleton$v[,vc]<-.Object@skeleton$v[,vc]+center[vc]
		}
		.Object@center <- center
		
		return(.Object)
	
	}
)

	
#' A penta-hexagonal icosahedral grid
#' 
#' \code{hexagrid} creates a hexa-pentagonal grid based on the inversion of a 
#'    tessellated icosahedron.
#'
#' Inherits from the \code{trigrid} class.
#'
#' The grid structure functions as a frame for data graining, plotting and
#'	calculations. Data can be stored in layers that are linked to the grid object. In the current version only the 
#'	\code{facelayer} class is implemented which allows the user to render data to the cells
#'	of the grid which are called faces. 
#' 	The grid 'user interface' is made up of four primary tables: the \code{@vertices} table for the coordinates of the vertices,
#' 	the \code{faceCenters} for the coordinates of the centers of faces,
#'	the \code{faces} and the \code{edges} tables that contain which vertices form which faces and edges respectively.
#'	In these tables, the faces and vertices are sorted to form spirals that go from the north pole in a counter-clockwise
#'	direction. In case grid subsetting is performed these tables get truncated.
#'	
#'	At finer resolutions, the large number of spatial elements render all calculations very resource demanding and slow, 
#'	therefore the hierarchical structure created during the tessellation procedure is retained for efficient implementations.
#'	These data are stored in a list in the slot \code{@skeleton} and are 0-indexed integer tables for Rccp-based functions. \code{$v} 
#'	stores vertex, \code{$f} the edge, and \code{$e} contains the edge data for plotting and calculations. In these tables
#'	the original hierarchy based orderings of the units are retained, during subsetting, additional vectors are used to indicate
#'	deactivation of these units. Any sort of meddling with the @skeleton object will lead to unexpected behavior.
#'	
#' @slot vertices Matrix of the vertex coordinates.
#'
#' @slot faces Matrix of the verticies forming the faces
#'
#' @slot edges Matrix of the vertices forming the edges.
#'
#'	@slot tessellation Contains the tessellation vector.
#'
#'	@slot orientation Contains the grid orientation in xyz 3d space, values in radian.
#'
#'	@slot center The xyz coordinates of the grid's origin/center.
#'
#'	@slot div Contains the number of faces that a single face of the previous tessellation level is decomposed to.
#'
#'	@slot faceCenters Contains the xyz coordinates of the centers of the faces on the surface of the sphere.	
#'
#'
#' @param tessellation An integer vector with the tessellation values. Each number
#'    describes the number of new edges replacing one original edge. Multiple series of tessellations
#' 	  are possible this way. The total tessellation is the product of the tessellation vector. 
#'	  Higher values result in more uniform cell sizes, but the larger number of tessellation series,
#'	  increases the speed of lookup functions
#'
#' @param sp A logical value indicating whether the 'SpatialPolygons' class representation of the grid
#'	should be added to the object when the grid is calculated. If set to true the SpPolygons() function will be run with with the resolution parameter set to 25. The 
#'  resulting object will be stored in slot @sp. As the calculation of this object can increase the grid creation time substantially
#'	 by default this argument has a value FALSE. This can be added on demand by running the function \code{newsp}.
#'
#' @param graph A logical value indicating whether the 'igraph' class representation of the grid
#'	should be added to the object when the grid is calculated. This argument defaults to TRUE because this option has only minor performance load on the grid 
#' constructor function. For familiarization with the
#' object structure, however, setting this parameter to FALSE might help, as invoking str() on the 'igraph' class slot of the class might flood the console.
#'
#' @param radius The radius of the grid. Defaults to the authalic radius of Earth.
#' @param center The origin of the grid in the reference Cartesian coordinate system. Defaults to (0,0,0).
#'
#' @rdname hexagrid-class
#' @name hexagrid
#' @aliases hexagrid-class
#'
#' @return A hexagonal grid object, with class \code{hexagrid}.
#' @examples
#' 		g <- hexagrid(c(8))
#' 		g1 <- hexagrid(c(2,3,4))
#' @exportClass hexagrid
hexagrid<-setClass(
	"hexagrid",
	contain="trigrid",
)



#' @rdname hexagrid-class
#' @aliases hexagrid-initalize-method
#' @param .Object non-argument pointing to self.
#' @exportMethod hexagrid
setMethod(
	"initialize",
	signature="hexagrid",
	definition=function(.Object, tessellation=1, sp=FALSE, graph=TRUE, center=origin, radius=authRadius){
			
		tGrid<-trigrid(tessellation, radius=radius)
		# v part of the skeleton and the active vertices
			# new $v: first part original vertices of the trigrid (faceCenters)
			# of the hexagrid
			# second par: original face centers of the trigrid (in the order of n and f)
			# vertices of the new hexagrid
			v<-rbind(tGrid@skeleton$v,tGrid@faceCenters[tGrid@skeleton$aF[as.logical(tGrid@skeleton$aF)],])
			rownames(v)<-NULL
			aV<-rep(FALSE, nrow(v))
			
			aV[(nrow(tGrid@skeleton$v)+1):length(aV)]<- tGrid@skeleton$aF[as.logical(tGrid@skeleton$aF)]
			
		# the number of original vertices
			nOrigV<-nrow(tGrid@skeleton$v)
			
			#vertices member (ordered in the trigrid)
			vertices<-tGrid@faceCenters
			options(scipen=999)
			rownames(vertices)<-paste("P", 1:nrow(vertices), sep="")
			
			#uiV - the R indexes of the vertices in $v
			uiV<-rep(0, nrow(vertices))
			uiV[aV[as.logical(aV)]]<-1:length(uiV)+nOrigV
			names(uiV)<-rownames(vertices)
			
		#the faces
			#triangular faces in the last tessellation
			fTemp<-tGrid@skeleton$f[(tGrid@skeleton$offsetF+1):nrow(tGrid@skeleton$f),1:3]
			
			#table of the subfaces:
			#first column: original vertices of trigrid, the internal order of hexagonal faces
			sF<-.Call(Cpp_icosa_CreateHexaSubfaces_, tGrid@skeleton$n, fTemp, nOrigV)
			
			#total faces table required for lookups
			f<-rbind(tGrid@skeleton$f, sF)
			
			# links the subfaces to their UI orders (0-no subface, 1: F1 in the ui)
			aSF<-rep(0, nrow(f))
			aSF[(nrow(tGrid@skeleton$f)+1):nrow(f)]<-tGrid@skeleton$aV[sF[,1]+1]
			
		#edges 
			#internal representation of edges - plotting
			e<-unique(sF[,2:3])
			
			#outer
			# the vertices with outer indices
			edges<-matrix(paste("P", aV[as.numeric(e)+1], sep=""), ncol=2)
			rownames(edges)<-paste("E", 1:nrow(edges), sep="")
			
			#activation - for subsetting
			aE<-rep(T, nrow(e))
			
		#faces matrix
			#the first column implies the face the subfaces belong to
			fOrdered<-unique(sF[order(sF[,1]),1:3])
			
			#which subfaces (R indices of rows in $f table) belong to which face
			#ordered like the internal representation of trigrids vertices!
			facesInternal<-.Call(Cpp_icosa_HexaFaces_, fOrdered)
			facesInternal[1:12,6]<-NA
			vF<-facesInternal
			
			#replace the vertex indices to the outer indices (names)
			facesExpandOut<-aV[as.numeric(facesInternal)+1]
			facesExpandOut[!is.na(facesExpandOut)]<-paste("P", facesExpandOut[!is.na(facesExpandOut)], sep="")
			
			#create a matrix
			facesExpandOut<-matrix(facesExpandOut, ncol=6)
			
			#reorder the faces from north to south
			faces<-facesExpandOut
			faces[tGrid@skeleton$aV,]<-facesExpandOut
			
			# suppress scientific notation
			rownames(faces)<-paste("F", 1:nrow(faces), sep="")
			
			#uiF - will be used for subsetting
			indices<-tGrid@skeleton$aV[sF[,1]+1]
			
			uiF<-.Call(Cpp_icosa_RetrieveIndexMat_, indices)
			uiF[uiF==0]<-NA
			
			#add the offset (R indices)
			uiF<-uiF+nrow(tGrid@skeleton$f)
			rownames(uiF)<-paste("F", 1:nrow(uiF), sep="")
			
			# when doing subsetting:
			# 1. select the faces by names
			# 2. flatten with as.numeric()
			# 3. subset $f for rows
			# 4. unique the output -> than you can use the indices for $v
			
		#face centers
			faceCenters<-tGrid@vertices
			rownames(faceCenters) <- paste("F", 1:nrow(faceCenters), sep="")
			
			# the centers of the faces for plotting ($plotV)
			#ordered as tGrird@skeleton$v
			transFace<-t(faces)
			flatF<-as.character(transFace)
			nas<-is.na(flatF)
			flatF[nas]<-"P1"
			
			vertsF<-vertices[flatF,]
			vertsF[nas,]<-NA
			rownames(vertsF)<-NULL
			indF<-rep(1:nrow(faces),each=6)
			x<-tapply(vertsF[,1], indF, mean, na.rm=TRUE)
			y<-tapply(vertsF[,2], indF, mean, na.rm=TRUE)
			z<-tapply(vertsF[,3], indF, mean, na.rm=TRUE)
			
			#still the outer order
			fcPlot<-cbind(x,y,z)
			
			#the inner order (which is necessary for plotting)
			plotV<-v
			plotV[tGrid@skeleton$uiV,]<-fcPlot[,]
			
			
			#the inner order
			
			# total skeleton
			aF<-tGrid@skeleton$aV
			
			skeleton<-list(f=f, vF=vF, aF=aF,aSF=aSF, uiF=uiF, v=v, aV=aV, uiV=uiV,plotV=plotV, e=e, aE=aE, edgeTri=tGrid@edges)
		
	
			.Object@tessellation <-tessellation
			.Object@div <-tGrid@div
			
		# the face belts of the hexagrid
			# the total number of subdivisons
			prodTess<-prod(tessellation)
			
			# the ascending part of the face indices
			beltIndicesPart<-rep(NA, prodTess+1)
			beltIndicesPart[1]<-1
			
			# increasing by 5 in every belt
			for(i in 2:length(beltIndicesPart)){
				beltIndicesPart[i]<-(i-1)*5
			}
			beltIndicesMid<-rep(beltIndicesPart[i], prodTess-1)
			
			beltIndices<-c(beltIndicesPart, beltIndicesMid, rev(beltIndicesPart))
			beltIndices<-cumsum(beltIndices)
			
			# transform the indices to belt numbers
			belts<-rep(length(beltIndices), max(beltIndices))
			for(i in (length(beltIndices)-1):1){
				belts[beltIndices[i]:1]<-i
			}
			.Object@belts<-belts
		
		
		#user interface
			.Object@faces <- faces
			.Object@vertices <- vertices
			.Object@edges <- edges
			.Object@skeleton <- skeleton
			.Object@faceCenters <- faceCenters
			
			#edge Length calcualtion
			.Object@edgeLength <- c(
				mean(.Call(Cpp_icosa_edges_, skeleton$v, skeleton$e, tGrid@center, 1)),
				mean(.Call(Cpp_icosa_edges_, skeleton$v, skeleton$e, tGrid@center, 0))/pi*180)
				names(.Object@edgeLength) <- c("km", "deg")
			
			.Object@length<- c(
				"vertices"=nrow(.Object@vertices),
				"edges"=nrow(.Object@edges),
				"faces"=nrow(.Object@faces))
				
		
			#temporary solutions
			.Object@r <- tGrid@r
			.Object@proj4string<- tGrid@proj4string
			.Object@orientation<-c(0,0,0)
			.Object@center <- center
		
		
		# calculate the graph representation
		dummy<-NA
		.Object@graph<-dummy		
		if(graph==TRUE){
			.Object@graph<-gridgraph(.Object)
		}		
		
		if(sp==TRUE){
				.Object@sp<-SpPolygons(.Object, res=25)
		}else{
			dummy<-NA
			.Object@sp<-dummy
		}
		
		# translate the 3d information with the center
		for(vc in 1:3){
			.Object@vertices[,vc] <- .Object@vertices[,vc]+center[vc]
			.Object@faceCenters[,vc] <- .Object@faceCenters[,vc]+center[vc]
			.Object@skeleton$v[,vc] <- .Object@skeleton$v[,vc]+center[vc]
			.Object@skeleton$plotV[,vc] <- .Object@skeleton$plotV[,vc]+center[vc]
		}
		options(scipen=0)	
		return(.Object)
		
	}
)
	
		




#' Subsetting an icosahedral grid
#' 
#' This is a generic function used to access data from either a triangular or hexagonal grid using the names of the faces, integers or logical vectors. 
#' 
#' The function returns subsets of the grid pertaining to the specified faces that can be used for additional operations (e.g. plotting). 
#' The subscript vector can be either a logical, character or numeric one. The character vector should contain the names of faces, the logical subscript should have 
#' the same length as the number of faces in the order in which the faces are present in the \code{faces} slot. 
#' The numeric vector can either refer to indices to the rownames of faces in the faces slot, or
#' to surfaces bounded by longitude/latitude data. In the latter case, the the vector should contain an element with a names of at least one of the "lomax", "lamax", 
#' "lomin" or "lamin" strings (lo for longitude, la: latitude, min: minimum, max: maximum). In case a subset around the dateline is needed a larger longitude to a smaller longitude value is needed (e.g. between 150° to -150°). 
#' 
#' 
#' 
#' @param i A subscript vector, specifying the names of the face that are used for subsetting.
#' 
#' @examples
#'     #create a triangular grid
#'         g <- trigrid(c(2,2))
#'     #make a subset pertaining to the faces
#'         subG1 <- subset(g, c("F1", "F33"))
#'     
#'	   #additional way of subsetting
#'			subG2 <- g[1:15] # selects faces F1 through F15
#'     logicalSub<-sample(c(TRUE,FALSE), nrow(g@faces), replace=TRUE)
#'			subG3 <- g[logicalSub]
#'     #plot the subset in 3d space
#'         plot3d(subG3)
#'		# previously mentioned case around the dateline
#'		gDateLine<-g[c(lomax=-150, lomin=150)]
#'			plot3d(gDateLine)
#'
#' @rdname subset-methods
#' @aliases subset, subset-trigrid-method
#' @return Subset of the input grid. The class of the original object is retained, the \code{@skeleton} slot contains all previous information.
#' @exportMethod subset
setMethod(
	"subset",
	signature="trigrid",
	definition=function(x, i){
	
	#	i<-c("F295", "F300")
	#	x<-grid
		
		#checking
		if(is.numeric(i)){
			#add checking for lat/long subsetting
			# lat-long mode of subsetting
			potConds<-c("lamin", "lamax", "lomin", "lomax")
			if(sum(names(i)%in%potConds)>0){
				#if it contains an unitelligible names
				if(sum(!names(i)%in%potConds)>0) 
					warning("Some subcript condition names were not recognized.")
				
				
				#in case you want something at the dateline
				normal <- T
				if(sum(c("lomax", "lomin")%in%names(i))==2){
					if(i["lomin"]>i["lomax"]){
						normal<- F
					}
				}
				
				#get the facecenters
				pol <- CarToPol(x@faceCenters, norad=TRUE, origin=x@center)
				
				boolSelect<-rep(T, nrow(pol))
				
				#longitude
				if(normal){
					#minimum longitude condition
					if("lomin"%in%names(i)){
						boolSelect <- boolSelect & pol[,1]>=i["lomin"]
					}
					
					#maximum longitude condition
					if("lomax"%in%names(i)){
						boolSelect <- boolSelect & pol[,1]<=i["lomax"]
					}
				}else{
					#minimum longitude condition
					if("lomin"%in%names(i)){
						boolSelect <- boolSelect & pol[,1]>=i["lomin"]
					}
					
					#maximum longitude condition
					if("lomax"%in%names(i)){
						boolSelect <- boolSelect | pol[,1]<=i["lomax"]
					}
				
				}
				
				#minimum latitude condition
				if("lamin"%in%names(i)){
					boolSelect <- boolSelect & pol[,2]>=i["lamin"]
				}
				
				#minimum latitude condition
				if("lamax"%in%names(i)){
					boolSelect <- boolSelect & pol[,2]<=i["lamax"]
				}
				
				i<-rownames(x@faceCenters)[boolSelect]
				# control will pass over to the subsetting by facenames

			}else{
			
			# index subsetting
				i<-paste("F", i, sep="")
			}
		}		
		
		if(is.logical(i)){
			if(length(i)==nrow(x@faces)){
				i<-rownames(x@faces)[i]
			}else{
				stop("Logical subscript has wrong length.")
			}
		}
		
		#check whether there are NAs in the i variables
		if(sum(is.na(i))){
			warning("The specified face names contain NA values.")
			#omit
			i<-i[!is.na(i)]
		}
		
		if(sum(!i%in%rownames(x@faces))>0) stop("Invalid face names entered.")
		
	
		#triGrid subset
		#1. subset of faces
			subsetFaces<-x@faces[i, ,drop=FALSE]
			
			# skeleton - faces - use skeleton$uiF to deactivate faces in skeleton$aF
			#false vector
			tempFacesLog<-rep(F, length(x@skeleton$aF))
			#what should not be removed
			tempFacesLog[x@skeleton$offsetF+x@skeleton$uiF[which(names(x@skeleton$uiF)%in%i)]] <- TRUE
			#remove everthing but that
			x@skeleton$aF[!tempFacesLog]<- FALSE
		
		#2. points
			pointnames<-unique(as.character(subsetFaces))
			subsetVertices<-x@vertices[pointnames,, drop=FALSE]
			
			# skeleton - use skeleton$uiV to deactivate faces in skeleton$aV
			tempVerticesLog<-rep(F,length(x@skeleton$aV))
			tempVerticesLog[x@skeleton$uiV[which(names(x@skeleton$uiV)%in%pointnames)]] <- TRUE
			
			x@skeleton$aV[!tempVerticesLog] <- FALSE
		
		#3. subset of edges
			#the original logical of the edges
			edgeTemp<-x@skeleton$aE
			
			#logical for the points - kept or not?
			logE<-matrix(as.logical(x@skeleton$aV)[as.numeric(x@skeleton$e+1)], ncol=2)
			
			#where both points are needed in the subset, keep both!
			aE<-apply(logE,1, sum)==2
			x@skeleton$aE<-aE
			
			#use that but only where it was subsetted previous - for UI
			subsetEdges<-x@edges[aE[edgeTemp],, drop=FALSE]
			
			
		
		#4. subset of faceCenters
			subsetFaceCenters<-x@faceCenters[i,, drop=FALSE]
			
		
		#6. copy over the original object
		#	y<-x
			
			#and change it accordingly
			x@vertices=subsetVertices
			x@faces=subsetFaces
			x@edges=subsetEdges
			x@faceCenters=subsetFaceCenters
			
			x@length<- c(
			"vertices"=nrow(x@vertices),
			"edges"=nrow(x@edges),
			"faces"=nrow(x@faces))
		
		#7. if the @sp slot contains data, subset it too
		if(suppressWarnings(!is.na(x@sp))){
			x@sp <- x@sp[rownames(x@faces)]
		}
		
		if(suppressWarnings(!is.na(x@graph))[1]){
			x@graph <- igraph::induced_subgraph(x@graph,rownames(x@faces))
		}
		
		
		return(x)
		
	}
)

#' @rdname subset-methods
#' @aliases subset, subset-hexagrid-method
#' @exportMethod subset
setMethod(
	"subset",
	signature="hexagrid",
	definition=function(x, i){
	
		#checking
		if(is.numeric(i)){
			#add checking for lat/long subsetting
			# lat-long mode of subsetting
			potConds<-c("lamin", "lamax", "lomin", "lomax")
			if(sum(names(i)%in%potConds)>0){
				#if it contains an unitelligible names
				if(sum(!names(i)%in%potConds)>0) 
					warning("Some subscript condition names were not recognized.")
				
				
				#in case you want something at the dateline
				normal <- T
				if(sum(c("lomax", "lomin")%in%names(i))==2){
					if(i["lomin"]>i["lomax"]){
						normal<- F
					}
				}
				
				#get the facecenters
				pol <- CarToPol(x@faceCenters, norad=TRUE, origin=x@center)
				
				boolSelect<-rep(T, nrow(pol))
				
				#longitude
				if(normal){
					#minimum longitude condition
					if("lomin"%in%names(i)){
						boolSelect <- boolSelect & pol[,1]>=i["lomin"]
					}
					
					#maximum longitude condition
					if("lomax"%in%names(i)){
						boolSelect <- boolSelect & pol[,1]<=i["lomax"]
					}
				}else{
					#minimum longitude condition
					if("lomin"%in%names(i)){
						boolSelect <- boolSelect & pol[,1]>=i["lomin"]
					}
					
					#maximum longitude condition
					if("lomax"%in%names(i)){
						boolSelect <- boolSelect | pol[,1]<=i["lomax"]
					}
				
				}
				
				#minimum latitude condition
				if("lamin"%in%names(i)){
					boolSelect <- boolSelect & pol[,2]>=i["lamin"]
				}
				
				#minimum latitude condition
				if("lamax"%in%names(i)){
					boolSelect <- boolSelect & pol[,2]<=i["lamax"]
				}
				
				i<-rownames(x@faceCenters)[boolSelect]
				# control will pass over to the subsetting by facenames

			}else{
			
			# index subsetting
				i<-paste("F", i, sep="")
			}
		}		
		
		if(is.logical(i)){
			if(length(i)==nrow(x@faces)){
				i<-rownames(x@faces)[i]
			}else{
				stop("Logical subscript has wrong length.")
			}
		}
		
	#	i<-c("F295", "F300")
	#	x<-grid
	
		#check whether therer are NAs in the i variables
		if(sum(is.na(i))){
			warning("The specified face names contain NA values")
			#omit
			i<-i[!is.na(i)]
		}
		
		
		if(sum(!i%in%rownames(x@faces))>0) stop("Invalid face names entered.")
		
		# order the i vector to avoid any potential errors
			i<-sort(i)
		
		
		# hexaGrid subset
		#1. subset of faces
			#ui representation
			subsetFaces<-x@faces[i, ,drop=FALSE]
			
			# indexing of $f
			subFaceIndex<-as.numeric(x@skeleton$uiF[i,])
			aSF<-x@skeleton$aSF
			aSF[!1:length(x@skeleton$aSF)%in%subFaceIndex]<-0
		
			aF<-x@skeleton$aF
			aF[!paste("F",aF,sep="")%in%i]<-0
			
		#2. points 
		#2a.(vertices)
			pointnames<-unique(as.character(subsetFaces))
			subsetVertices<-x@vertices[rownames(x@vertices)%in%pointnames,]
		
			#indexing in $v
			aV<-x@skeleton$aV
			aV[!1:length(aV)%in%x@skeleton$uiV[rownames(subsetVertices)]]<-0
		
		
		#3b. subset of edges
			edgeTemp<-x@skeleton$aE
			
			logE<-matrix(as.logical(aV)[as.numeric(x@skeleton$e+1)], ncol=2)
			aE<-apply(logE,1, sum)==2
			
			subsetEdges<-x@edges[aE[edgeTemp],]
		
		#4. subset of faceCenters
			subsetFaceCenters<-x@faceCenters[i,, drop=FALSE]
			
		#5. copy over the original object
			y<-x
			
			#and change it accordingly
			y@vertices=subsetVertices
			y@faces=subsetFaces
			y@edges=subsetEdges
			y@faceCenters=subsetFaceCenters
			
			y@skeleton$aV<-aV
			y@skeleton$aSF<-aSF
			y@skeleton$aE<-aE
			y@skeleton$aF<-aF
			
			y@length<- c(
			"vertices"=nrow(y@vertices),
			"edges"=nrow(y@edges),
			"faces"=nrow(y@faces))
			#7. if the @sp slot contains data, subset it too
		
		if(suppressWarnings(!is.na(x@sp))){
			y@sp <- y@sp[rownames(y@faces)]
		}
		
		if(suppressWarnings(!is.na(x@graph))[1]){
			y@graph <- igraph::induced_subgraph(y@graph,rownames(y@faces))
		}
			return(y)
		
	}
)


#' Extract subset faces of a trigrid or hexagrid object using.
#' 
#' Shorthand for the subset function.
#'
#' @rdname subset-methods
#' @aliases [-trigrid-method
#' @exportMethod
setMethod(
	"[",
	signature="trigrid",
#	definition=function(x,i,j,..., drop=TRUE){
	definition=function(x,i){
		subset(x, i)
	
	}
)



#' 3d plotting of an icosahedral grid or its subset
#' 
#' This is a generic function used to plot either a \code{trigrid} or a \code{hexagrid} object or their \code{facelayer} in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @param x The \code{trigrid}, \code{hexagrid} or \code{facelayer} object to be plotted.
#' 
#' @param type A character value specifying the part of the grid to be plotted by the call of the function. 
#' \code{"v"} plots the grid vertex points. 
#' \code{"e"} draws the grid edges.
#' \code{"f"} draws the grid faces.
#' \code{"c"} draws the face centers of the grid.
#' 
#' @param sphere Defaults to NULL, adding a central white sphere to the plot. Assigning a numeric value will draw a new sphere with the given radius,
#'		\code{FALSE} does not plot the sphere. 
#' @param guides Logical value indicating whether the guidelines of the polar coordinate system shall be plotted.
#' @param add Logical value indicating whether a new plot shall be drawn, or the currently plotted information should be added to the active rgl device.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}).
#' 
#' @return The function does not return any value.
#'
#' @rdname plot3d-method
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'     plot3d(g, col="blue")
#' # make a subset to select faces
#'    subG <- subset(g, c("F5", "F2"))
#' # plot the subset defined above
#'     plot3d(subG, type="f", col=c("orange"), add=TRUE, lwd=1)
#' @rdname plot3d-methods
#' @aliases plot3d, plot3d-trigrid-method
#' @exportMethod plot3d
setMethod(
	"plot3d",
	signature="trigrid",
	definition=function(x, type=c("l"),sphere=NULL,  add=FALSE, guides=TRUE, ...){
		
		#create new plot?
		if(add==FALSE)
		{
			#checking plotting
			rgl::plot3d(x@vertices, type="n", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			
			#default sphere plotting
			if(is.null(sphere)){
				#get the radius
				fc<-apply(x@vertices[x@faces[1,],],2,mean)-x@center
				rad<-sqrt(fc[1]^2+fc[2]^2+fc[3]^2)-15
				blankSphere(x@center[1],x@center[2], x@center[3], radius = rad, color ="white", ng=200, box=FALSE, axes=FALSE)
			}else{
				if(sphere){
					blankSphere(x@center[1],x@center[2], x@center[3], radius = sphere, color ="white", ng=200, box=FALSE, axes=FALSE)
				}
			}
		}
		
		
		if(type=="p")
		{
			#single point
			if(length(x@vertices)==3)
			{
				rgl::points3d(x=x@vertices[1],y=x@vertices[2],z=x@vertices[3],...)
			}else{
				rgl::points3d(x@vertices, ...)
			}
		}
			
		if(type=="l")
		{
			icosa::lines3d(x, ...)
		}
		
		if(type=="f")
		{
			faces3d(x,...)
		}
		
		if(type=="n"){
		
		}
		# guides
		if(guides){
			guides3d(col="green", origin=x@center, radius=x@r, lwd=2)
		}
		
		
	}
)
#' 3d plotting of an icosahedral grid or its subset
#' @rdname plot3d-method
#' @param color Only for the hexagrid plotting: character value/values, passed to the faces3d() function instead of col.
#' @aliases plot3d, plot3d-hexagrid-method
#' @exportMethod plot3d
setMethod(
	"plot3d",
	signature="hexagrid",
	definition=function(x, type=c("l"),sphere=NULL, color="gray70", add=FALSE, guides=TRUE, ...){
		
		#create new plot?
		if(add==FALSE)
		{
			#empty plotting plotting
			rgl::plot3d(x@vertices, type="n", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			
			#default sphere plotting
			if(is.null(sphere)){
				fVect<-x@faces[2,!is.na(x@faces[2,])]
				#get the radius
				fc<-apply(x@vertices[fVect,],2,mean)-x@center
				rad<-sqrt(fc[1]^2+fc[2]^2+fc[3]^2)-10
				blankSphere(x@center[1],x@center[2], x@center[3], radius = rad, color ="white", ng=200, box=FALSE, axes=FALSE)
			}else{
				if(sphere){
					blankSphere(x@center[1],x@center[2], x@center[3], radius = sphere, color ="white", ng=200, box=FALSE, axes=FALSE)
				}
			}
		}
		
		
		if(type=="p")
		{
			#single point
			if(length(x@vertices)==3)
			{
				rgl::points3d(x=x@vertices[1],y=x@vertices[2],z=x@vertices[3], col=color,...)
			}else{
				rgl::points3d(x@vertices, col=color, ...)
			}
		}
			
		if(type=="l")
		{
			icosa::lines3d(x, ...)
		}
		
		if(type=="f")
		{
			faces3d(x,...)
		}
		
		if(type=="c")
		{
			#single point
			if(length(x@faceCenters)==3)
			{
				rgl::points3d(x=x@faceCenters[1],y=x@faceCenters[2],z=x@faceCenters[3], col=color, ...)
			}else{
				rgl::points3d(x@faceCenters, col=color, ...)
			}
		}
		
		if(type=="t")
		{
			rgl::text3d(x@faceCenters, texts=rownames(x@faceCenters),col=color, ...)
		}
		
		if(type=="n"){
		
		}
		if(guides){
			guides3d(col="green", origin=x@center, radius=x@r, lwd=2)
		}
		
		
	}
)

#' Methods of 3d line plotting.
#' 
#' This is a generic function used to plot the edge lines of either a \code{trigrid} or a \code{hexagrid} object in 3d space. The method is also implemented for 
#' the object classes defined by the package 'sp'.
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @param x The \code{trigrid}, \code{hexagrid}, \code{facelayer} or \code{sp} object to be plotted.
#' @param arcs Logical value setting whether great circle arcs or segments shall be drawn betwenn the points of the grid.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}).
#' 
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'     plot3d(g, col="blue")
#' # make a subset to select faces
#'    subG <- subset(g, c("F5", "F2"))
#' # plot the subset defined above
#'     plot3d(subG, type="f", col=c("orange"), add=TRUE, lwd=1)
#' @rdname lines3d-method
#' @aliases lines3d-trigrid-method
#' @exportMethod lines3d
setMethod(
	"lines3d",
	signature="trigrid",
	definition=function(x, arcs=FALSE, ...){
		v<-x@skeleton$v
		e<-x@skeleton$e[x@skeleton$aE,]
		
		#create edgeMat with a simple Rccp function
		edgeMat<-.Call(Cpp_icosa_edgeMatTri_, v=v, e=e)
		
		# get the list of additional arguments
		newArgs<-list(...)
		
		
		if(prod(x@tessellation)<16 & arcs){
			res<-10
			edgeMat<-.Call(Cpp_icosa_expandEdges_, edgeMat, x@center, res)
		}
		edgeMatExp<-edgeMat
		rgl::segments3d(x=edgeMatExp[,1],y=edgeMatExp[,2],z=edgeMatExp[,3], ...)
	}
)


#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of either a \code{trigrid} or a \code{hexagrid} object in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @name faces3d
#' @param x The \code{trigrid}, \code{hexagrid} or \code{facelayer} object to be plotted.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}) and the heatMapLegend() function.
#' 
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'     plot3d(g)
#' # make a subset to select faces
#'    subG <- subset(g, c("F5", "F2"))
#' # plot the subset defined above
#'     faces3d(subG, col="orange")
#' @exportMethod faces3d
#' @rdname faces3d-methods
	setGeneric(
		name="faces3d",
		package="icosa",
		def=function(x,...){
			standardGeneric("faces3d")
			
		}
	)

#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of either a \code{trigrid} or a \code{hexagrid} object in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @return The function does not return any value.
#'
#' @rdname faces3d-methods
#' @aliases faces3d, faces3d-trigrid-method	
setMethod(
	"faces3d",
	signature="trigrid",
	definition=function(x, ...){
		
		v<-x@skeleton$v
		f<-x@skeleton$f[as.logical(x@skeleton$aF),1:3]
		
		#create edgeMat with a simple Rccp function
		triMat<- .Call(Cpp_icosa_triMatTri_, v, f)
		
		rgl::triangles3d(x=triMat[,1],y=triMat[,2],z=triMat[,3],...)
			
	}
)

#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of either a \code{trigrid} or a \code{hexagrid} object in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @return The function does not return any value.
#'
#' @rdname faces3d-methods
#' @aliases faces3d, faces3d-hexagrid-method	
setMethod(
	"faces3d",
	signature="hexagrid",
	definition=function(x,...){
		v<-x@skeleton$plotV
		f<-x@skeleton$f[as.logical(x@skeleton$aSF),1:3]
		
	#	f2<-f[order(f[,1]),]
	#	
	#	f2<-unique(f2)
		
		#create edgeMat with a simple Rccp function
		triMat<- .Call(Cpp_icosa_triMatTri_, v, f)
		
		rgl::triangles3d(x=triMat[,1],y=triMat[,2],z=triMat[,3],...)
	
	
	}
)

#' Guides for 3d spherical plotting.
#' 
#' This function plots 3d guidelines for navigation on the surface of the sphere,
#' 	 includings the drawing of the rotational axis and a polar coordinate system.
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @param axis Numeric argument draws the -90(lat. deg. ) +90 (lat. deg.) axis. The plotted radius will be 'axis' times the authalic radius ca. 6371km.
#' 
#' @param polgrid numeric argument with the length of 2, where the first argument specifies
#' the size of the longitudinal and the second the latitudinal divisions (degrees). Setting this argument to NULL will turn this feature off.
#'
#' @param res numeric argument for graphical resolution of the curves:
#' the distance in degrees between the points of the rendered guides. 
#'	@param textPG logical value, indicating whether the coordinate values should be added to the 3d render.
#' 
#' @param origin Numeric vector of length=3. Indicates the center of the guiding sphere.
#' @param radius Numeric values indicating the radius of the guiding sphere. Defaults to the R2 radius of Earth (6371.007km). 
#' @param drad Numeric value, indicates the position of coordinate 3d text relative to the guiding sphere radius.
#' @param ... additional arguments passed to rgl::segments3d(), rgl::lines3d() and rgl::text3d().
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'	   plot3d(g, guides=FALSE)
#' # plot the rotational axis in blue
#' 		guides3d(axis=2, polgrid=NULL, col="blue")
#' # plot the polar grid at 10 degree resolution
#'     guides3d(axis=NULL, polgrid=c(10,10), col="red")
#' # plot some coordinates
#'    guides3d(axis=NULL, polgrid=c(30,30), textPG=TRUE, col="orange", cex=1.4)
#' @export
guides3d<-function(axis=1.5, polgrid=c(30,30), textPG=FALSE, res=1,  origin=c(0,0,0), radius=authRadius, drad=1.1, ...){
	
	if(!is.null(axis)){
		rgl::segments3d(x=c(0,0)+origin[1], y=c(0,0)+origin[2], z=c(-axis*radius,axis*radius)+origin[3],...)
		rgl::segments3d(x=c(200,0)+origin[1], y=c(0,0)+origin[2], z= c(axis*radius-500,axis*radius)+origin[3],...)
		rgl::segments3d(x=c(-200,0)+origin[1], y=c(0,0)+origin[2], z= c(axis*radius-500,axis*radius)+origin[3],...)
	}
	if(!is.null(polgrid[1])){

		if(360%%polgrid[1]!=0) 
			stop(paste("360 is not divisble by ", polgrid[1],sep=""))
		if(180%%polgrid[2]!=0) 
			stop(paste("180 is not divisble by ", polgrid[2],sep=""))
		
		#meridians
		#division
		usedLongs<-seq(-180,180,polgrid[1])
		a<-usedLongs
		#resolution
		b<-c(seq(-90,90,res), NA)
		
		lngs<-rep(a,each=length(b))
		lngs[1:length(a)*length(b)]<-NA
		
		lats<-rep(b, length(a))
		merid<-cbind(lngs, lats)
		merid3d<-PolToCar(merid,origin=origin, radius=radius)
		
		#lat circles
		#division
		usedLats<-seq(-90,90,polgrid[2])
		a<-usedLats
		#resolution
		b<-c(seq(-180,180,res),NA)
		
		
		lats<-rep(a,each=length(b))
		lats[1:length(a)*length(b)]<-NA
		
		lngs<-rep(b, length(a))
		latC<-cbind(lngs, lats)
		
		latC3d<-PolToCar(latC,origin=origin, radius=radius)
		
		rgl::lines3d(merid3d,...)
		rgl::lines3d(latC3d,...)
		
		if(textPG){
			# the latitudes
			latTab<-cbind(rep(0,length(usedLats)), usedLats)
			coordLat<-PolToCar(latTab, origin=origin, radius=radius)*drad
			rgl::text3d(coordLat, text=usedLats, ...)
		
			# the longitudes
			# you need only 180, no -180
			usedLongs<- usedLongs[-length(usedLongs)]
			longTab<-cbind(usedLongs,rep(0,length(usedLongs)))
			coordLong<-PolToCar(longTab, origin=origin, radius=radius)*drad
			rgl::text3d(coordLong, text=usedLongs, ...)
		
		}
		
	}
	
}


#' Display the names of the grid elements in 3d plots.
#' 
#' This function will display the names of vertices, faces and edges on 3d plots.
#' 
#' @name gridlabs3d
#'  
#' @param gridObj A \code{trigrid} or \code{hexagrid} object to be plotted.
#' 
#' @param type A character vector containing either "f", "e" or "v", rendering the names
#' of either the faces, edges or vertives respectively.
#'
#' @param ... further arguments passed to \code{text3d} of the rgl package.
#' 
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'	   plot3d(g, guides=FALSE)
#' # plot the names of the faces
#' 		gridlabs3d(g, type="f", col="red")
#' # plot the names of the vertices
#'     gridlabs3d(g, type="v", col="blue", cex=0.6)
#' @exportMethod gridlabs3d
#' @rdname gridlabs3d-methods
setGeneric(
	name="gridlabs3d",
	package="icosagrid",
	def=function(gridObj,...){
		standardGeneric("gridlabs3d")
		
	}
)

#' @rdname gridlabs3d-methods
#' @aliases gridlabs3d, gridlabs3d-trigrid-method
setMethod(
	"gridlabs3d",
	signature="trigrid",
	definition=function(gridObj,type="f",...){
		
		# vertex names
		if("v"%in%type){
			rgl::text3d(texts=rownames(gridObj@vertices), gridObj@vertices*1.005,...)
		}
		
		
		if("f"%in%type){
			rgl::text3d(texts=rownames(gridObj@faceCenters), gridObj@faceCenters*1.005,...)
		}
		
		if("e"%in%type){
			#the coordinates
			coords<-apply(gridObj@edges,1,function(x){
				apply(gridObj@vertices[x,], 2, mean)
			})
			
			rgl::text3d(t(coords)*1.005, texts=rownames(gridObj@edges),...)
		}
	
	
	}
)
	
#' @rdname gridlabs3d-methods
#' @aliases gridlabs3d, gridlabs3d-hexagrid-method
setMethod(
	"gridlabs3d",
	signature="hexagrid",
	definition=function(gridObj,type="f",...){
		
		# vertex names
		if("v"%in%type){
			rgl::text3d(texts=rownames(gridObj@vertices), gridObj@vertices*1.005,...)
		}
		
		
		if("f"%in%type){
			rgl::text3d(texts=rownames(gridObj@faceCenters), gridObj@faceCenters*1.005,...)
		}
		
		if("e"%in%type){
			#the coordinates
			coords<-apply(gridObj@edges,1,function(x){
				apply(gridObj@vertices[x,], 2, mean)
			})
			
			rgl::text3d(t(coords)*1.005, texts=rownames(gridObj@edges),...)
		}
	
	
	}
)



#' Add an igraph object to a predefined slot in a trigrid or hexagrid object
#'
#' @name newgraph
#		
#' @rdname newgraph-methods
#' @param gridObj an icosahedral grid.
#' @param ... arguments passed to the gridgraph() function.
#' @examples
#'		#create a grid
#'		g<-trigrid(4, graph=FALSE)
#'		g<-newgraph(g)
#' 
#' @exportMethod newgraph
setGeneric(
	name="newgraph",
	package="icosa",
	def=function(gridObj,...){
		standardGeneric("newgraph")
	}

)

#' Add an igraph object to a predefined slot in a trigrid or hexagrid object
#'
#' @rdname newgraph-methods
#' @aliases newgraph-trigrid-method
#' @examples
#'		#create a grid
#'		g<-trigrid(4, graph=FALSE)
#'		g<-newgraph(g)
#' 
#' @exportMethod newgraph
setMethod(
	"newgraph",
	signature="trigrid",
	definition=function(gridObj,...){
		gridObj@graph<-gridgraph(gridObj,...)
		return(gridObj)
	
	}
)

