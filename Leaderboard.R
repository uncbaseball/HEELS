
pitcher.leaderboard <- function(gameids){
  data %>% filter(GameID %in% gameids & PitcherTeam == "NOR_TAR" & PitchCall != "Undefined") %>%
    mutate(strike = as.numeric(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"))) %>%
    group_by(Pitcher, Game) %>% summarise(strikeper = mean(strike), whiff = sum(PitchCall == "StrikeSwinging"),
                                          p1k = mean(ifelse(Balls != 0 | Strikes != 0, NA_integer_, strike), na.rm = T),
                                          maxvelo = max(RelSpeed, na.rm = T), kills = sum(Balls < 2 & Strikes == 2 & (strike | PlayResult == "Out")),
                                          avg_exit_velo = mean(ExitSpeed * ifelse(InPlay, 1, NA_integer_), na.rm = T)
                                          )
}












# live %<>% mutate(yyyymmdd = paste0("20", str_sub(as.Date(Date, format = "%m/%d/%Y"), 3, -1)),
#                  XCoord = c(cos(Angle * pi / 180) * ExitSpeed), 
#                  YCoord = c(sin(Angle * pi / 180) * ExitSpeed), 
#                  XLand = c(cos((90 - Bearing) * pi / 180) * Distance), 
#                  YLand = c(sin((90 - Bearing) * pi / 180) * Distance)) %>%
#   as.data.frame()
# 
# 
# for(contact.type in c("barrel", "solid", "F&B", "topped", "under")){
#   live[which(!is.na(live$XCoord)), contact.type] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == contact.type),c("X", "Y")]), 
#                                                                       as.matrix(live[which(!is.na(live$XCoord)),c("XCoord", "YCoord")])))
# }
# 
# live %<>% mutate(XCoord = XCoord * ifelse(BatterSide == "Left", -1, 1),
#                  weak = as.numeric(ExitSpeed < 60),
#                  InPlay = as.numeric(PitchCall == "InPlay" & PlayResult != "Undefined"), 
#                  AB_End = c(PitchCall == "HitByPitch" | InPlay | KorBB %in% c("Strikeout", "Walk")),
#                  ContactType = ifelse(as.logical(barrel), "Barrel", ifelse(as.logical(solid), "Solid", ifelse(as.logical(`F&B`), "F&B", ifelse(as.logical(weak), "Weak", ifelse(as.logical(topped), "Topped", ifelse(as.logical(under), "Under", NA_character_)))))),
#                  Good_Contact = ifelse(is.na(ContactType), NA_integer_, as.numeric(ContactType %in% c("Barrel", "Solid", "F&B")))