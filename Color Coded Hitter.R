library(DT)
game <- data9 %>% filter(BatterTeam == "NOR_TAR" & GameID == "CAMUNC032619")

absummaries <- function(singlegame = F){
  ag <-  data9 %>% filter(BatterTeam == "NOR_TAR" & PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay") & if(singlegame) yyyymmdd == max(data9$yyyymmdd) else yyyymmdd > "2019-03-22") %>%
          mutate(HitterHotColdPM = ifelse(Strikes == 2, ifelse(PitchCall == "StrikeCalled", "Strikeout Looking", ifelse(PitchCall == "StrikeSwinging", "Strikeout Swinging", gsub(HitterHotColdPM, pattern = "Strikeout/", fixed = T, replacement = ""))),
                                          ifelse(PitchCall == "InPlay", paste(HitterHotColdPM, "- In Play"), HitterHotColdPM)),
                 `Exit Velo/Angle` = ifelse(PitchCall == "InPlay", ifelse(is.na(ExitSpeed), "No TM Data", paste0(round(ExitSpeed), "mph/", round(Angle), "˚")),""),
                 `Contact Quality` = ifelse(PitchCall == "InPlay", ifelse(is.na(ContactType), "No TM Data", ContactType), "")) %>%
          select(Batter, Game, Times_In_The_Box, Strikes, HitterHotColdPM, AB_Result, `Contact Quality`, `Exit Velo/Angle`)

  k2.fouls <- ag %>% group_by(Batter, Game, Times_In_The_Box) %>%
    summarise("Two Strike Fouls" = sum(Strikes == 2 & HitterHotColdPM == "Foul"), `Contact Quality` = paste0(`Contact Quality`, collapse = ""), `Exit Velo/Angle` = paste0(`Exit Velo/Angle`, collapse = ""))
  
  ag %<>% select(-`Exit Velo/Angle`) %>% select(-`Contact Quality`) %>% filter(!(Strikes == 2 & HitterHotColdPM == "Foul")) %>% spread(Strikes, HitterHotColdPM)
  
  names(ag) <- c("Batter", "Game", "At-Bat", "Result", "Strike 1", "Strike 2", "Strike 3")
  
  ag2 <- left_join(ag, k2.fouls, by = c("Batter", "Game", "At-Bat" = "Times_In_The_Box"))
  
  ag2 <- ag2[,c(1:4, 9:10, 5:8)]
  
  ag2$`Contact Quality` <- gsub(ag2$`Contact Quality`, pattern = "F&B", replacement = "Flare/Burner", fixed = T)
  
  if(singlegame) ag2 %<>% select(-Game)
  
  datatable(ag2) %>%
    formatStyle('Contact Quality', backgroundColor = styleEqual(c("Barrel", "Solid", "Flare/Burner", "Weak", "Under", "Topped"), c("green", "green", "green", "red", "red", "red"))) %>%
    
    formatStyle(c('Strike 1', 'Strike 2', 'Strike 3'),
                backgroundColor = styleEqual(c("Hot Zone Take", "Chase - In Play", "Chase", "Cold Zone Swing - In Play", 
                                               "Cold Zone Swing", "Strikeout Swinging", "Weak Contact", "Strikeout Looking",
                                               "Cold Zone Take", "Hot Zone Swing", "Hot Zone Swing - In Play", "Solid Contact", "Reached Base"), 
                                             c(rep("red", 8), rep("green", 5)))
                )
  }

## add pitch locations
## add 2k foul balls
## add ab results