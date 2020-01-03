#########################   Hot/Cold +/-    #################################

## adding to data

##  adding to app filters
  # balls and strikes


hot.cold.plus.minus <- function(gameids){
  hitterhc <- read.csv(file = "Hitter Hot Cold.csv", stringsAsFactors = F)
  
  data %>% 
    filter(GameID %in% gameids & endsWith(Date, "9") & BatterTeam %in% c("NOR_TAR", "NOR_TAR2")) %>% left_join(., hitterhc, by = "Batter", suffix = c("", ".Hot")) %>% 
    mutate(Swing = PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 
           In_Hot_Zone = sqrt((PlateLocSide_Catcher - PlateLocSide_Catcher.Hot)^2 + (PlateLocHeight - PlateLocHeight.Hot)^2) < radius,
           In_Strike_Zone = (PitchCall == "StrikeCalled" | Exp.Called.Strike > 0.5),
           HitterHotColdPM = ifelse(Strikes < 2, 
                                           ifelse(Swing, 
                                                  ifelse(In_Hot_Zone, "Hot Zone Swing",
                                                         ifelse(In_Strike_Zone, "Cold Zone Swing", "Chase")),
                                                  ifelse(In_Hot_Zone, "Hot Zone Take",
                                                         ifelse(In_Strike_Zone, "Cold Zone Take", "Ball Take"))),
                                           ifelse(as.logical(Hit) | as.logical(BB), "Reached Base",
                                                  ifelse(BBtype %in% c("GB", "FB"), "Solid Contact",
                                                         ifelse(as.logical(K) | ContactType %in% c("Under", "Weak"), "Strikeout/Weak Contact", "None")))),
           HotColdPoints = ifelse(HitterHotColdPM  == "Hot Zone Swing", 2, 
                                  ifelse(HitterHotColdPM %in% c("Cold Zone Take", "Reached Base"), 1, 
                                         ifelse(HitterHotColdPM %in% c("Ball Take", "Solid Contact"), .5,
                                                ifelse(HitterHotColdPM %in% c("Cold Zone Swing", "Hot Zone Take", "Strikeout/Weak Contact"), -1,
                                                       ifelse(HitterHotColdPM == "Chase", -2, 0))))),
           BadSwing = as.logical(HotColdPoints < 0 & as.logical(Swing)),# == T
           BadTake = as.logical(HotColdPoints < 0 & !as.logical(Swing))) %>% # == F
    group_by(Batter) %>%
    summarise(HotColdPoints = sum(HotColdPoints, na.rm = T), BadTake = sum(BadTake, na.rm = T), BadSwings = sum(BadSwing, na.rm = T), BadCallByUmp = sum(PitchCall == "StrikeCalled" & Exp.Called.Strike < .5, na.rm = T), Pitches_Seen = n())
}

# table(dat2[,c("Date", "HotColdPoints", "Batter")])
# 
# 
# hitterhc %<>% mutate(minSide = PlateLocSide_Catcher * -1 - radius, maxSide = PlateLocSide_Catcher * -1 + radius,
#                      minHeight = PlateLocHeight - radius, maxHeight = PlateLocHeight + radius,
#                      minSideft = floor(abs(minSide)) * sign(minSide), minSidein = ceiling((minSide - minSideft) * 12),
#                      maxSideft = floor(abs(maxSide)) * sign(maxSide), maxSidein = ceiling((maxSide - maxSideft) * 12),
#                      minHeightft = floor(minHeight), minHeightin = ceiling((minHeight - minHeightft) * 12),
#                      maxHeightft = floor(maxHeight), maxHeightin = ceiling((maxHeight - maxHeightft) * 12)
# )
# 
# hitterhc2 <- cbind(hitterhc[,c(1:4)], minSide = paste(hitterhc$minSideft, "' ", hitterhc$minSidein, '"'), 
#                   maxSide = paste(hitterhc$maxSideft, "' ", hitterhc$maxSidein, '"'),
#                   minHeight = paste(hitterhc$minHeightft, "' ", hitterhc$minHeightin, '"'),
#                   maxHeight = paste(hitterhc$maxHeightft, "' ", hitterhc$maxHeightin, '"'))




##  BELOW CAME FROM GLOBAL.R
hot.cold.plus.minus <- function(gameids){
  hitterhc <- read.csv(file = "Hitter Hot Cold.csv", stringsAsFactors = F)
  
  data %>% 
    filter(GameID %in% gameids & endsWith(Date, "9") & BatterTeam %in% c("NOR_TAR", "NOR_TAR2")) %>% left_join(., hitterhc, by = "Batter", suffix = c("", ".Hot")) %>% 
    mutate(Swing = PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 
           In_Hot_Zone = sqrt((PlateLocSide_Catcher - PlateLocSide_Catcher.Hot)^2 + (PlateLocHeight - PlateLocHeight.Hot)^2) < radius,
           In_Strike_Zone = (PitchCall == "StrikeCalled" | Exp.Called.Strike > 0.5),
           HitterHotColdPM = ifelse(Strikes < 2, 
                                    ifelse(Swing, 
                                           ifelse(In_Hot_Zone, "Hot Zone Swing",
                                                  ifelse(In_Strike_Zone, "Cold Zone Swing", "Chase")),
                                           ifelse(In_Hot_Zone, "Hot Zone Take",
                                                  ifelse(In_Strike_Zone, "Cold Zone Take", "Ball Take"))),
                                    ifelse(as.logical(Hit) | as.logical(BB), "Reached Base",
                                           ifelse(BBtype %in% c("GB", "FB"), "Solid Contact", 
                                                  ifelse(as.logical(K) | ContactType %in% c("Under", "Weak"), "Strikeout/Weak Contact", "None")))),
           HotColdPoints = ifelse(HitterHotColdPM  == "Hot Zone Swing", 2, 
                                  ifelse(HitterHotColdPM %in% c("Cold Zone Take", "Reached Base"), 1, 
                                         ifelse(HitterHotColdPM %in% c("Ball Take", "Solid Contact"), .5,
                                                ifelse(HitterHotColdPM %in% c("Cold Zone Swing", "Hot Zone Take", "Strikeout/Weak Contact"), -1,
                                                       ifelse(HitterHotColdPM == "Chase", -2, 0))))),
           BadSwing = as.logical(HotColdPoints < 0 & as.logical(Swing)),# == T
           BadTake = as.logical(HotColdPoints < 0 & !as.logical(Swing))) %>% # == F
    group_by(Batter, Game) %>%
    summarise(HotColdPoints = sum(HotColdPoints, na.rm = T), BadTake = sum(BadTake, na.rm = T), BadSwings = sum(BadSwing, na.rm = T), BadCallByUmp = sum(PitchCall == "StrikeCalled" & Exp.Called.Strike < .5, na.rm = T), Pitches_Seen = n())
}
