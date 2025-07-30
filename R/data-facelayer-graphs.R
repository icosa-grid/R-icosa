
#' Create or instantiate an 'igraph' class graph based on a facelayer
#'
#' @rdname gridgraph
setMethod(
	f="gridgraph",
	signature="facelayer",
	definition=function(x){
		# the grid object of the facelayer
		actGrid <- dynGet(x@grid, minframe=0L)
		
		checkLinkedGrid(actGrid, x)
		
		# only the occupied cells should be part of the grid
		# or: where the logical values indicate absence
		if(is.logical(x@values)){
			x@values[x@values==FALSE] <- NA
		}
		
		# these will be the nodes of the graph
		nodes<- x@names[!is.na(x@values)]
		
		# subset the graph to the nodes which are indicated in the facelayer
		actGraph <- igraph::induced_subgraph(actGrid@graph, nodes)
		
		# the values of the facelayer should be also present
		if(!is.logical(x@values)){
			igraph::V(actGraph)$value <- x@values[!is.na(x@values)]
		}
		
		return(actGraph)

	}
)


#' @rdname patches
setMethod(
	"patches",
	signature=c(x="facelayer"),
		definition=function(x){

			if(inherits(values(x), "character") | inherits(values(x), "numeric") | inherits(values(x), "integer")){
				# grab the cell ids for valid values of the facelayer
				cells <- names(x)[!is.na(values(x))]

			# logical
			}
			if(inherits(values(x), "logical")){
				# grab the cell ids for TRUE values of the facelayer
				# NA values are IGNORED!
				cells <- names(x)[values(x)[!is.na(values(x))]]
			}

			# look up the patches (fall back to trigrid-method)
			res <- patches(x=dynGet(x@grid, minframe=0L), y=cells )

			return(res)

	}
)

#' @rdname holes
setMethod(
	"holes",
	signature=c(x="facelayer"),
		definition=function(x){

			if(inherits(values(x), "character") | inherits(values(x), "numeric") | inherits(values(x), "integer")){
				# grab the cell ids for valid values of the facelayer
				cells <- names(x)[!is.na(values(x))]

			# logical
			}
			if(inherits(values(x), "logical")){
				# grab the cell ids for TRUE values of the facelayer
				# NA values are IGNORED!
				cells <- names(x)[values(x)[!is.na(values(x))]]
			}

			# look up the patches (fall back to trigrid-method)
			res <- holes(x=dynGet(x@grid, minframe=0L), y=cells )

			return(res)

	}
)
