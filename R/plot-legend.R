# function to create a legend for a heatmap
#' Legend for a heatmap with predefined colors.
#'
#' This function will invoke the plot function to draw a heatmap legend.
#' 
#' The 'percents' refer to the plotting area measured from the lower left corner.
#' @rdname heatMapLegend
#'
#' @param cols Character vector, containnig the ordered colors that are used for the heatmap.
#' @param tick.text The values on the heatmap legend. If missing, will be calculated with minVal and maxVal. Should have the length as 'ticks'.
#' @param vals If tick.text is missing, the lowest value in the heatmap
#' @param ticks The number of ticks/values in the heatmap legend (the bar will be divided to this number).
#' @param tick.cex Letter size of the values on the legend.
#' @param varName The label of the variable name plotted to the heatmap.
#' @param barWidth The width (percent) of the bar featuring the colors of the heatmap.
#' @param barHeight The height (percent)of the bar featuring the colors of the heatmap.
#' @param tickLength The length (percent) of the ticks at the bars.
#' @param xLeft the x coordinate of the lower left hand corner of the bar.
#' @param yBot the y coordinate of the lower left hand corner of the bar.
#' @param add indicates wheter a new plot should be drawn or not. Defaults to FALSE.
#' @param bounds Logical vector indicating whether open intervals should be indicated for the legend. 
#' @param ... arguments passed to the plot() function.
#' @export		
heatMapLegend <- function(cols, vals, varName, tick.text=NULL,tick.cex=1.5, barWidth=3, barHeight=50,tickLength=1, xLeft=88, yBot=25, add=FALSE, bounds=c(FALSE, FALSE),...){
#	usedCols<<-cols
#	usedVals <<- vals

	if(!add){
		plot(NULL, NULL, xlim=c(0,100), ylim=c(0,100), axes=FALSE, xlab="", ylab="",xaxs="i", yaxs="i",...)
	}
	#	barWidth<-3
	#	tickLength=1
	#	xLeft<-88
	
		# plotting mode is dependent on the relationship of cols and vals
		if(length(cols)==length(vals)){
			inter<-FALSE
		}else{
			if(length(cols)==(length(vals)-1)){
				inter <- TRUE
			}else{
				stop("Invalid color length.")
			}
		}

		# calculate horizontal positions
		x2<-xLeft+barWidth
		x3<-x2+tickLength
		x4<-x3+tickLength
	
		# y coordinates of the bar
		yTop<-yBot+barHeight
		
		# plot the colour bar first (this doesn't change)
		rectTops<-seq(yBot, yTop,length.out=length(cols)+1)
		for(i in 2:length(rectTops)){
			graphics::rect(xleft=xLeft, xright=x2, ytop=rectTops[i],ybottom=rectTops[i-1], 
			col=cols[i-1], border=NA)
		}
		graphics::rect(ytop=yBot, ybottom=yTop, xleft=xLeft, xright=x2)
	
		# the ticks 
		# what should be plotted?
		# intervals
		if(inter){
			# are the values given? - plot those
			if(!is.null(tick.text)){
				# the number of ticks is set by the tick text provided by the user
				ticks <- length(tick.text)

			# should the default be used?
			}else{
				# if the number of colours is smaller than 8, plot all of them 
				if(length(cols)<8){
					ticks <- length(vals)
					tick.text <- vals
				# treat it as a continuous scale
				}else{
					ticks <- 5
					minVal<-min(vals)
					maxVal <- max(vals)
					tick.text <- seq(minVal, maxVal, length.out=ticks)
				}
			}
			
			# calculate the coordinates where the ticks have to be drawn
			ts<-seq(yBot,yTop, length.out=ticks)
		
		# values
		}else{
			# cooridnates should be at the middle of the rectangles
			rectMid <- (rectTops+rectTops[2:(length(rectTops)+1)])/2
			rectMid <- rectMid[!is.na(rectMid)]
		
			if(!is.null(tick.text)){
				# the number of ticks is set by the tick text provided by the user
				ticks <- length(tick.text)

				ts <- seq(rectMid[1], rectMid[length(rectMid)], length.out=ticks)
			# should the default be used?
			}else{
				# if the number of colours is smaller than 8, plot all of them 
				if(length(cols)<8){
					ticks <- length(vals)
					tick.text <- vals
					ts <- seq(rectMid[1], rectMid[length(rectMid)], length.out=ticks)
				# omit some of them 
				}else{
					allIndex <- 1:length(vals)
					good<- allIndex
					counter <- 2
					while(length(good)>7){
						good<- which(allIndex%%counter==1)

						counter<- counter+1
					}
					ticks <- length(good)
					tick.text <- vals[good]
					ts<- rectMid[good]
				}
			}
			
			# 
				
		}


		# plot the chosen number of ticks 

		for(i in 1:ticks){
			graphics::segments(x0=x2,x1=x3, y0=ts[i], y1=ts[i])
			
			# plus or minus signs? 
			sig <- NULL
			if(bounds[1] & i==1){
				sig <- "(-)"
			}
			if(bounds[2] & i==ticks){
				sig <- "(+)"
			}

			graphics::text(label=paste(format(tick.text[i], digits=5), sig), x=x4, y=ts[i], pos=4,cex=tick.cex)
		}


		# the variable name
		ySubLab<-yTop*1.1
		if(missing(varName)) varName<-""
		graphics::text(varName, x=xLeft, y=ySubLab, pos=4, cex=tick.cex)
	
}
	

