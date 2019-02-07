#' A Color Funtion 
#'
#' This function allows you to create a vector of distinct colors. 
#' @param outPutLength length of the color vector 
#' @param gradient Do you with to have a color gradient. Defaults to TRUE 
#' @param customColor Vector of colors you want to be included. Those are appended to the outputVector 
#' @param seed If you have more than 20 colors the colors are chosen Randomly. To make it reproducible this seed is given. Default is 25. 
#' @param show switches the output not to a vector of colors but a plot, to visualize the pallet. Defaults to FALSE 
#' @keywords colors
#' @export
#' @examples
#' CHNColor(6)


CHNColor <- function(outPutLength, gradient = FALSE, customColor = c(), seed= 25, show = FALSE){
	# ColorStrings
	List95 <- c('#000000','#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080')
	List99 <- c('#000000',"#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#42d4f4", "#f032e6", "#fabebe", "#469990", "#e6beff", "#9A6324", "#fffac8", "#800000", "#aaffc3", "#000075", "#a9a9a9")
		
	List9999 <- c('#000000',"#ffe119", "#4363d8", "#f58231", "#fabebe", "#e6beff", "#800000", "#000075", "#a9a9a9")
	
	List100 <- c('#000000',"#ffe119", "#4363d8", "#a9a9a9")
	
	# Functions
	if (gradient != FALSE) {
		library(colorRamps)
		colfunc <- colorRampPalette(gradient)
		tempList <- colfunc(outPutLength)
	}
	
	
	if (gradient == FALSE) {
		
		if(outPutLength <= length(List100)){
			tempList <- (List100[1:outPutLength])
		}
		if(outPutLength <= length(List9999) & outPutLength > length(List100) ){
			tempList <- (List9999[1:outPutLength])
		}
		if(outPutLength <= length(List99) & outPutLength > length(List9999) ){
			tempList <- (List99[1:outPutLength])
		}
		if(outPutLength <= length(List95) & outPutLength > length(List99) ){
			tempList <- (List95[1:outPutLength])
		}
		if(outPutLength > length(List95) ){
			library(colorRamps)
			Allcolor = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
			set.seed(seed)
			listOfColors <- sample(Allcolor, outPutLength) 
			tempList <- (listOfColors[1:outPutLength])
		}
	}
	
	
	tempList <- c(tempList, customColor)
	
	
	if (show == FALSE) {
		return((tempList))
	}
	if (show == TRUE) {
		plot(1,1, xlim = c(0,length(tempList)+1), ylim = c(0,2), col= "White"
			 , xlab = "Color Index", ylab = "",yaxt='n')
		
		counter <- 0
		for (value in tempList) {
		
			counter <- counter +1 
			lines(c(counter,counter), c(-1,2), col=value, pch= 16, lwd= 40)
			}
	}
	
}

