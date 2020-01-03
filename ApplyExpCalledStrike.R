######################    Apply Exp. Called Strike    ##################

apply.exp.called.strike <- function(data){
  require(earth)
  
  data$Exp.Called.Strike <- NA
  
  if(all(is.na(data$RelSpeed))) return(data)
  
  load("data/Strike Zone Models.RData") # if not already loaded
  
  if(!is.element("AutoPitchTagged", colnames(data)) || !all(data$AutoPitchTagged)) data <- generic.pitch.tag(data)
  if(!is.element("CountDiff", colnames(data))) data <- cbind(data, CountDiff = data$Balls - data$Strikes)
  
  
  for(bside in c("l", "r")){
    for(pitch in c("fb", "ch", "bb")){
      combos <- which(startsWith(data$BatterSide, toupper(bside)) & data$AutoPitchType == toupper(pitch))
      if(length(combos) > 0) data[combos,"Exp.Called.Strike"] <- unname(predict(get(paste0(bside, "hb.", pitch, ".sz.mod")), 
                                                                                data[which(startsWith(data$BatterSide, toupper(bside)) & data$AutoPitchType == toupper(pitch)),]))
      
    }
  }
  
  return(data)
  
}
