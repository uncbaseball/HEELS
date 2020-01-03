#####################################		Release Point Graphs		#####################################


#	find rms distance from each point to all others
##	select point with min distance
##	get points within 2 sd of that point
##	use ellipsoidhull to get ellipsoid

commasplit <- function(string){
		split <- strsplit(string, ", ", fixed = TRUE)
	
		return(paste(split[[1]][2], split[[1]][1]))
}
		
releasepointgraph <- function(data1, data2, pitcher = mode(data1$Pitcher), image, ball.strike = F, axistitles = T, ybot = 0, save = F, pitcherview = F){
  

	pitchtypes <- c("FB", "FC", "FS", "CH", "CU", "SL", "SI")
	#data1 <- data1[which(data1$Pitcher == pitcher & !is.element(data1$TaggedPitchType, c("Undefined", "IntentionalBall"))),]
	if(!pitcherview){
	  data1$RelSide <- data1$RelSide * -1
	  if(!missing(data2)){
	    data2$RelSide <- data2$RelSide * -1
	  }
	}

	if(ball.strike){
	  data1$PitchCol <- ifelse(data1$PitchCall == "BallCalled", "green", ifelse(data1$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"), "red", NA))
	  data1 <- data1[which(!is.na(data1$PitchCol)),]
	}
	
	data1 <- data1[which(data1$Pitcher == pitcher & is.element(data1$TaggedPitchType, c(pitchtypes))),]
	
	pside <- mode(data1$PitcherThrows)
	
	meanx <- mean(data1$RelSide, na.rm = TRUE) # use aggregate for ball.strike
	meanz <- mean(data1$RelHeight, na.rm = TRUE)
	
	xmax <- ifelse(meanx > 0, meanx + 5, meanx - 5) 
	xmin <- ifelse(meanx > 0, meanx - 3, meanx + 3)

	plot(0, 0, type = "n", xlim = c(ifelse(xmin < xmax, xmin, xmax), ifelse(xmax > xmin, xmax, xmin)), ylim = c(ybot, 7), xlab = ifelse(axistitles, "Horizontal Release Point (ft.)", ""), ylab = ifelse(axistitles, "Vertical Release Point (ft.)", ""), main = paste(pitcher, "Release Points"))
	
	if(missing(image)){
	  imagedir <- paste("data/Release Point Pics/")
	  if(paste0(commasplit(pitcher), ".png") %in% dir(imagedir)){
	    image <- paste0(imagedir, commasplit(pitcher), ".png")
	  } else{
	    image <- NULL
	  }
		
	}
	
	require(png)
	if(!is.null(image)){
	  img <- readPNG(image)
	  
  	if(pside == "Right"){
  		rasterImage(img, meanx, 0, c(meanx + 3.5), meanz)
  	} else if(pside == "Left"){
  		rasterImage(img, c(meanx - 3.5), 0,  meanx, meanz)
  	}
	}
	
	cols <- c("green", "red", "blue", "orange", "brown", "pink", "purple")
	
	colnum <- 1
	alph <- 0.8
	
	if(ball.strike){
	  points(data1$RelSide, data1$RelHeight, col = adjustcolor(data1$PitchCol, alpha = 0.5), pch = 16)
	  legend("topright", legend = c("Ball", "Strike"), col = c("green", "red"), pch = 16, cex = 0.4, pt.cex = 1)
	  # star avg ball, star avg strike
	} else{
	  for(pitch in c(unique(data1[which(data1$TaggedPitchType != "Undefined"),"TaggedPitchType"]))){
		  #print(c(unique(data1[which(data1$TaggedPitchType != "Undefined"),"TaggedPitchType"])))
		  relps <- data1[which(data1$TaggedPitchType == pitch), c("RelSide", "RelHeight")]
		  alph <- alph - 0.2
		  points(relps[,1], relps[,2], pch = 16, cex = 0.75, col = adjustcolor(cols[colnum], alpha = ifelse(missing(data2), alph, 0.2)))
		  colnum <- colnum + 1
	  }
	  legend("topright", legend = unique(data1$TaggedPitchType), pch = 16, cex = 0.4, pt.cex = 1, col = cols[c(1:colnum)])
	}
	
	if(!missing(data2)){
		colnum <- 1
		data2 <- data2[which(data2$Pitcher == pitcher & !is.element(data2$TaggedPitchType, c("Undefined", "IntentionalBall"))),]
		
	  for(pitch in c(unique(data1$TaggedPitchType))){
		  relps <- data2[which(data2$TaggedPitchType == pitch), c("RelSide", "RelHeight")]
		  points(relps[,1], relps[,2], pch = 16, col = adjustcolor(cols[colnum], alpha = 0.9))
		  #points(relps[,1], relps[,2]) was making it dark w/ small screen
		  colnum <- colnum + 1
	  }
	}

	
	
	if(missing(data2)){
		avgdist <- round(c(12 * mean(sqrt((data1$RelSide - meanx)^2 + (data1$RelHeight - meanz)^2))), digits = 1)
	} else{
		avgdist <- round(c(12 * mean(sqrt((data2$RelSide - mean(data2$RelSide, na.rm = TRUE))^2 + (data2$RelHeight - mean(data2$RelHeight, na.rm = TRUE))^2))), digits = 1)
	}
	
	if(!ball.strike){
	  text(meanx, 7.5, paste("Avg. Distance from Center:", avgdist, "in."), cex = 0.8)
	}
	
		
	if(save){
		if(missing(data2)){
			quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point.png", sep = ""))
		} else{
			quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point ", gsub("/", ".", unique(data2$Date)), ".png", sep = ""))
		}
	}
	
	
}

releasepointgraph.ly <- function(data1, data2, pitcher = mode(data1$Pitcher), plot.title = paste(pitcher, "Release Points"), image, xvar = "RelSide", yvar = "RelHeight", ball.strike = F, axistitles = T, ybot = 0, save = F, pitcherview = F){
  require(png)
  
  pitchtypes <- c("FB", "FC", "FS", "CH", "CU", "SL", "SI")
  cols <- c("green", "red", "blue", "orange", "brown", "pink", "purple")

  #### Filter Data  ####
  data <- cbind(data1, Ind = c(1))
  if(!missing(data2)) rbind(data, cbind(data2, Ind = c(2)))
  data$x <- ifelse(xvar %in% c("RelSide", "Extension") | xvar == "RelSide" & pitcherview, data[,xvar], 
                   ifelse(xvar == "RelSide" & !pitcherview, data$RelSide * -1, ""))
  data$x <- data[,yvar]
  data <- data[which(!is.element(data$PitchCall, c("Undefined", "IntentionalBall")) & data$Pitcher == pitcher & is.element(data$TaggedPitchType, c(pitchtypes))),]
  
  
  ####  Assign Colors ####
  if(ball.strike){
    data$PitchCol <- ifelse(data$PitchCall == "BallCalled", "green", 
                                        ifelse(data$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"), "red", NA))
    data$Type <- ifelse(data$PitchCol == "green", "Ball", "Strike")
    data$PitchCol <- adjustcolor(col = data$PitchCol, alpha.f = 0.5 + 0.2 / data$Ind)
  } else{
    data$Type <- data$TaggedPitchType
    pitchcols <- ifelse(data$TaggedPitchType == "FB", "blue", 
                       ifelse(data$TaggedPitchType == "CH", "green", 
                              ifelse(data$TaggedPitchType == "CU", "red", 
                                     ifelse(data$TaggedPitchType == "SL", "orange", "purple"))))
    
    data$PitchCol <- adjustcolor(col = pitchcols, 
                                 alpha.f = 0.7)#0.5 + 0.2 / data$Ind)
  }
  
  data <- data[which(!is.na(data$PitchCol)),]
  
  #### Data Summary ####
  pside <- mode(data[which(data$Ind == 1), "PitcherThrows"])
  
  meanx <- mean(data[which(data$Ind %in% c(1, ifelse(ball.strike, 0, 2))), xvar], na.rm = T) 
  meanz <- mean(data[which(data$Ind %in% c(1, ifelse(ball.strike, 0, 2))), yvar], na.rm = T)
  
  xmax <- ifelse(meanx > 0, meanx + 5, meanx + 5) 
  xmin <- ifelse(meanx > 0, meanx - 3, meanx - 3)
  
  
 
  ##### Image ####
  if(missing(image)){
    imagedir <- paste("data/Release Point Pics/")
    if(xvar == "Extension"){
      image <- paste0(imagedir, "/baum extension.png")
    } else if(paste0(commasplit(pitcher), ".png") %in% dir(imagedir)){
      image <- paste0(imagedir, commasplit(pitcher), ".png")
    } else{
      image <- NULL
    }
    
  }
  
  
  if(!is.null(image)){
    img <- readPNG(image)
    
    if(xvar == "Extension"){
      img.list <- list(source = raster2uri(img), xref = "x", yref = "y", x = meanx - 2, y = 0, sizex = 3.5, sizey = meanz, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below")
    } else if(pside == "Right"){
      img.list <- list(source = raster2uri(img), xref = "x", yref = "y", x = meanx, y = 0, sizex = 3.5, sizey = meanz, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below")
    } else if(pside == "Left"){
      img.list <- list(source = raster2uri(img), xref = "x", yref = "y", x = meanx - 3.5, y = 0, sizex = 3.5, sizey = meanz, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below")
    }
  } else{
    img.list <- list()
  }
  
  ####  Plot  ####
  p <- plot_ly(data = data, x = ~get(xvar), y = ~get(yvar), color = ~PitchCol, name = ~Type) %>%
    layout(title = plot.title, xaxis = list(title = ifelse(axistitles, "Horizontal Release Point (ft.)", ""), range = range(c(xmin, xmax)), visible = F), yaxis = list(title = ifelse(axistitles, "Vertical Release Point (ft.)", ""), range = c(ybot, 7), visible = F), images = img.list)
  
  
  return(p)
  #### Show Averages  ####
  # if(missing(data2)){
  #   avgdist <- round(c(12 * mean(sqrt((data1$RelSide - meanx)^2 + (data1$RelHeight - meanz)^2))), digits = 1)
  # } else{
  #   avgdist <- round(c(12 * mean(sqrt((data2$RelSide - mean(data2$RelSide, na.rm = TRUE))^2 + (data2$RelHeight - mean(data2$RelHeight, na.rm = TRUE))^2))), digits = 1)
  # }
  # 
  # if(!ball.strike){
  #   text(meanx, 7.5, paste("Avg. Distance from Center:", avgdist, "in."), cex = 0.8)
  # }
  # 
  # 
  # if(save){
  #   if(missing(data2)){
  #     quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point.png", sep = ""))
  #   } else{
  #     quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point ", gsub("/", ".", unique(data2$Date)), ".png", sep = ""))
  #   }
  # }
  
  
}









makeellipse <- function(data, pitchername){
	data <- data[which(data$Pitcher == pitchername & !is.na(data$RelSide)),]
	
	relpoints <- data[,c("RelSide", "RelHeight")]
	
	alldists <- c()
	for(row in c(1:nrow(relpoints))){
		
		dist <- sum(sqrt(c(relpoints[row, "RelSide"] - relpoints[-row, "RelSide"])^2 + c(relpoints[row, "RelHeight"] - relpoints[-row, "RelHeight"])^2)) / (nrow(relpoints) - 1)
	
		alldists <- append(alldists, dist)
		
	}
	
	
	center <- relpoints[which.min(alldists),]
	
	row <- which.min(alldists)
		
	mindists <- sqrt(c(relpoints[row, "RelSide"] - relpoints[-row, "RelSide"])^2 + c(relpoints[row, "RelHeight"] - relpoints[-row, "RelHeight"])^2)

	sd2 <- sd(mindists, na.rm = TRUE) * 0.5

	range <- relpoints[which(abs(mean(mindists) - mindists) < sd2),]
	range <- relpoints[order(mindists),]
	
	range <- range[c(1:(0.67 * nrow(range))),]	
	
	#require(ellipse)
	#return(ellipse(range))
	
	require(cluster)
	#elip <- ellipsoidhull(as.matrix(range),)
	
	require(mixtools)
	ellipse(mu = elip[[1]], sigma = elip[[2]])	
}


#### Improvements ####
# show avgs and ranges (star avg ball, star avg strik)
# add mound, plate
# add 3d
# specific pitch result & dist from avg in tooltip