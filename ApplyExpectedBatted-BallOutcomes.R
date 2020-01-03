applybbexpoutcomes <- function(data, type = "tm", loc.indep = F, babip = F){
  
  require(earth)
  require(nnet)
  
  if(type == "tm"){  
    ####  Verify Columns   ####
    assert_that(all(is.element(c("Angle", "ExitSpeed", "Bearing"), colnames(data)) & ("LBearing" %in% names(data) | "BatterSide" %in% names(data))), msg = "data must have Angle, ExitSpeed, and Bearing columns as well as LBearing and/or BatterSide")
  
    if(!is.element("LBearing", names(data))) data <- cbind(data, LBearing = ifelse(data$BatterSide == "Left", data$Bearing, 0))
    
    validate_that(all(is.element(c("PlayResult", "HitType"), names(data))), msg = "PlayResult and/or HitType columns were missing from data so pitches missing Angle will remain NA")
    
    
    ####  Separate batted balls
    data <- cbind(data, Index = c(1:nrow(data)), Type = paste(data$PlayResult, data$HitType, sep = ""), BatsL = ifelse(!is.element("BatterSide", colnames(data)), 0, as.numeric(data$BatterSide == "Left")), ExpOBP = c(NA), ExpSLG = c(NA), Exp2OPS = c(NA))
    
    if(!is.element("PitchCall", names(data))){
      ip <- data
      nip <- data.frame()
    } else{
      ip <- data[which(data$PitchCall == "InPlay"),]
      nip <- data[which(data$PitchCall != "InPlay"),]
    }
    
    ip <- cbind(ip, Pred_1B = NA, Pred_2B = NA, Pred_3B = NA, Pred_HR = NA)
    if(nrow(nip) > 0) nip <- cbind(nip, Pred_1B = NA, Pred_2B = NA, Pred_3B = NA, Pred_HR = NA)
    
    
    load("data/Batted Ball Models.RData")
  
    ####  Scale LA/EV/Bear  ####
    unscaled <- ip[order(ip$Index), c("Index", "Angle", "ExitSpeed", "Bearing")]
    
    ip[which(!is.na(ip$Bearing)), rownames(scale.bear)] <- as.data.frame(scale(ip[which(!is.na(ip$Bearing)), rownames(scale.bear)], center = scale.bear[,"center"], scale = scale.bear[,"scale"]))
    ip[which(is.na(ip$Bearing) & !is.na(ip$Angle)), rownames(scale.laev)] <- as.data.frame(scale(ip[which(is.na(ip$Bearing) & !is.na(ip$Angle)), rownames(scale.laev)], center = scale.laev[,"center"], scale = scale.laev[,"scale"]))
    
    ####  Predict w/ neural nets  ####
    if(any(!is.na(ip$Bearing) & ip$BatterSide == "Left")) lpred <- predict(lhb.bb.nnet, ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Left"), rownames(scale.bear)])
    if(any(!is.na(ip$Bearing) & ip$BatterSide == "Right")) rpred <- predict(rhb.bb.nnet, ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Right"), rownames(scale.bear)])
    if(any((is.na(ip$Bearing) | !is.element(ip$BatterSide, c("Left", "Right")) | loc.indep == T) & !is.na(ip$Angle))) opred <- predict(laev.bb.nnet, ip[which((is.na(ip$Bearing) | !is.element(ip$BatterSide, c("Left", "Right")) | loc.indep == T) & !is.na(ip$Angle)), rownames(scale.laev)])
    
    ## would be good to add exp 1b/2b/3b/hr columns
    
    ####  Calc Exp OBP/SLG  ####
    if(any(ip$BatterSide == "Left" & !is.na(ip$Bearing))){
      ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Left"), c("Pred_1B", "Pred_2B", "Pred_3B", "Pred_HR")] <- lpred[,c("Single", "Double", "Triple", "HomeRun")]
      # ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Left"), "ExpOBP"] <- rowSums(lpred[,c("Single", "Double", "Triple", "HomeRun")])
      # ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Left"), "ExpSLG"] <- rowSums(as.data.frame(lpred[,c("Single", "Double", "Triple", "HomeRun")]) * matrix(c(1:3, ifelse(babip, NA, 4)), nrow = 1, ncol = 4), na.rm = T)  / (1 - ifelse(babip, lpred[,"HomeRun"], 0))
    } 
    
    if(any(ip$BatterSide == "Right" & !is.na(ip$Bearing))){
      ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Right"), c("Pred_1B", "Pred_2B", "Pred_3B", "Pred_HR")] <- rpred[,c("Single", "Double", "Triple", "HomeRun")]
      # ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Right"), "ExpOBP"] <- rowSums(rpred[,c("Single", "Double", "Triple", "HomeRun")])
      # ip[which(!is.na(ip$Bearing) & ip$BatterSide == "Right"), "ExpSLG"] <- rowSums(as.data.frame(rpred[,c("Single", "Double", "Triple", "HomeRun")]) * matrix(c(1:3, ifelse(babip, NA, 4)), nrow = 1, ncol = 4), na.rm = T)  / (1 - ifelse(babip, rpred[,"HomeRun"], 0))
    }
    
    if(any(is.na(ip$Bearing) & !is.na(ip$Bearing))){
      ip[which((is.na(ip$Bearing) | !is.element(ip$BatterSide, c("Left", "Right")) | loc.indep == T) & !is.na(ip$Angle)), c("Pred_1B", "Pred_2B", "Pred_3B", "Pred_HR")] <- opred[,c("Single", "Double", "Triple", "HomeRun")]
      # ip[which((is.na(ip$Bearing) | !is.element(ip$BatterSide, c("Left", "Right")) | loc.indep == T) & !is.na(ip$Angle)), "ExpOBP"] <- rowSums(opred[,c("Single", "Double", "Triple", "HomeRun")])
      # ip[which((is.na(ip$Bearing) | !is.element(ip$BatterSide, c("Left", "Right")) | loc.indep == T) & !is.na(ip$Angle)), "ExpSLG"] <- rowSums(as.data.frame(opred[,c("Single", "Double", "Triple", "HomeRun")]) * matrix(c(1:3, ifelse(babip, NA, 4)), nrow = 1, ncol = 4), na.rm = T)  / (1 - ifelse(babip, opred[,"HomeRun"], 0))
    }
    
    ip %<>% mutate(ExpOBP = Pred_1B + Pred_2B + Pred_3B + Pred_HR,
                   ExpSLG = Pred_1B + Pred_2B * 2 + Pred_3B * 3 + Pred_HR * 4)
    
    ####  Predict missing data  ####
    
    nodat <- read.csv(file = "data/ExpOutcomesTMMissingData.csv", stringsAsFactors = F)
    
    ip <- merge(ip, nodat, all.x = T, all.y = F, by = "Type", suffixes = c("", ".NoDat"))
    ip[which(is.na(ip$Angle)), "ExpOBP"] <- ip[which(is.na(ip$Angle)), "ExpOBP.NoDat"]
    ip[which(is.na(ip$Angle)), "ExpSLG"] <- ip[which(is.na(ip$Angle)), "ExpSLG.NoDat"]
    
    ####  2 OPS, remove extra columns
    ip$Exp2OPS <- c((ip$ExpOBP * 1.7 + ip$ExpSLG) / 3)
    
    ip <- ip[,-c(which(is.element(names(ip), c("ExpOBP.NoDat", "ExpSLG.NoDat"))))]
    
    
    ip <- ip[order(ip$Index),]
    assert_that(all(ip$Index == unscaled$Index))
    ip[,c("Angle", "ExitSpeed", "Bearing")] <- unscaled[,c("Angle", "ExitSpeed", "Bearing")]
    
    data <- rbind(ip, nip)
    
    return(data[order(data$Index, decreasing = F), -c(which(names(data) %in% c("Type", "Index", "LBearing", "BatsL")))])
  } else if(type == "pbp"){
    #data[which(!is.element(data$BatterHand), c("L", "R")), "BatterHand"] <- c("B")
    exps <- read.csv(file = "/Users/micahdaley-harris/Desktop/chillmedia/data/ExpOutcomesNonTM.csv", stringsAsFactors = F)
    data <- merge(data, exps, by = c("BatterHand", "BattedBallType", "Result", "Location"), all.x = T, all.y= F)
    data$Exp2OPS <- (data$ExpOBP * 1.7 + data$ExpSLG) / 3
    return(data)
  }
}

# p1 <- dbGetQuery(tarconn, "SELECT * FROM `FullPlaybyPlay`")
# alltm <- dbGetQuery(tarconn, "SELECT * FROM `alltm`")
# alltm <- givetmdataids(alltm)
# 
# both <- merge(alltm, p1, by = c("GameID", "PlayID"), suffixes = c("", ".pbp"), all = F)
# 
# both <- applybbexpoutcomes(both)
# ag <- both %>%
#   filter(PitchCall == "InPlay" & ContactFlag == 1 & !is.na(ExpOBP) & !is.na(Angle)) %>%
#   group_by(BatterSide, BattedBallType, Location, Result) %>%
#   summarise_at(c("ExpOBP", "ExpSLG", "Exp2OPS"), funs(mean = mean(., na.rm = T), sd = sd(., na.rm = T), n = n())) %>%
#     #ExpOBP = mean(ExpOBP, na.rm = T), ExpOBPsd = sd(ExpOBP, na.rm = T), ExpSLG = mean(ExpSLG, na.rm = T), ExpSLGsd = sd(ExpSLG, na.rm = T), Exp2OPS = mean(Exp2OPS, na.rm = T), Exp2OPSsd = sd(Exp2OPS, na.rm = T), n = n()) %>%
#   as.data.frame()
# 
# 
# names(ag)[1] <- "BatterHand"
# ag[,1] <- substr(ag[,1], 1, 1)
# names(ag) <- gsub(x = names(ag), pattern = "_mean", replacement = "")

# ag2 <- both %>%
#   filter(PitchCall == "InPlay" & ContactFlag == 1 & !is.na(ExpOBP) & !is.na(Angle)) %>%
#   group_by(BattedBallType, Location, Result) %>%
#   summarise_at(c("ExpOBP", "ExpSLG", "Exp2OPS"), funs(mean = mean(., na.rm = T), sd = sd(., na.rm = T), n = n())) %>%
#     #ExpOBP = mean(ExpOBP, na.rm = T), ExpOBPsd = sd(ExpOBP, na.rm = T), ExpSLG = mean(ExpSLG, na.rm = T), ExpSLGsd = sd(ExpSLG, na.rm = T), Exp2OPS = mean(Exp2OPS, na.rm = T), Exp2OPSsd = sd(Exp2OPS, na.rm = T), n = n()) %>%
#   as.data.frame()
# 
# names(ag2) <- gsub(x = names(ag2), pattern = "_mean", replacement = "")
# 
# ag2 <- cbind(BatterHand = c("B"), ag2)
# ag <- rbind(ag, ag2)

