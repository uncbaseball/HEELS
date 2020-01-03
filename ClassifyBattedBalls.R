#############################		Classify Batted-Ball		###############################

getbbtype <- function(exitvelo, launchangle, horizangle = 0, dist){
	
	table <- read.csv(file = "data/Groundball Coords.csv", stringsAsFactors = F)

	d <- table[which.min(as.numeric(table$HorizAng) - as.numeric(horizangle)), "Dist"]
	#print(d)
	#print(dist)
	if(is.na(dist)){
		return(NA)
	}
	if(dist <= d){
		return("GB")
	}
	
	aa <- getarcangle(launchangle = launchangle, exitvelo = exitvelo)
	
	if(aa > 135){
		return("LD")
	}
	else{
		return("FB")
	}
	
	
}



applybbtype <- function(data){
	data <- cbind(data, BBtype = 
		ifelse(is.na(data$Angle) | is.na(data$ExitSpeed) | is.na(data$Distance) | is.na(data$Bearing), NA, 
		ifelse(sqrt((cos((90 - data$Bearing) * pi / 180) * data$Distance)^2 + ((sin((90 - data$Bearing) * pi / 180) * data$Distance) - 60.5)^2) <= 75.89 & data$Angle < 20, "GB", 
		ifelse((182.05 - 1.18364 * sin(data$Angle * pi / 180) * data$ExitSpeed) >= 135, "LD", 
		ifelse((182.05 - 1.18364 * sin(data$Angle * pi / 180) * data$ExitSpeed) >= 108, "FB", "PU")))))
	
	return(data)
	
}

applyarcangle <- function(data, source = "TrackMan"){
  if(!is.element(source, c("TrackMan", "Trackman", "Statcast"))){
    stop("source must be one of 'TrackMan' 'Trackman' 'Statcast'")
  }
  if(source == "TrackMan" | source == "Trackman"){
    iuv <- as.data.frame(cbind(data$Angle, data$ExitSpeed))
  }
  if(source == "Statcast"){
    iuv <- as.data.frame(cbind(as.numeric(data$hit_angle), as.numeric(data$hit_speed)))
  }
  colnames(iuv) <- c("Angle", "ExitSpeed")
  iuv <- cbind(iuv, iuv = c(iuv$ExitSpeed * sin(iuv$Angle * pi / 180)))
  iuv <- cbind(iuv, iuv.mph = c(iuv$iuv / 1.467))
  iuv <- cbind(iuv, Arc.Angle = c(182.05 - (1.7364 * iuv$iuv.mph)))
  iuv[which(is.na(iuv$Angle)),"Arc.Angle"] <- NA
  
  data <- cbind(data, Arc.Angle = c(iuv$Arc.Angle))
  
  return(data)
  
  
  
  
}


getarcangle <- function(launchangle, exitvelo){
  iuv <- exitvelo * sin(launchangle * pi / 180)
  iuv.mph <- iuv / 1.467
  aa <- 182.05 - (1.7364 * iuv.mph)
  return(aa)
}