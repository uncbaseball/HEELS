https://s3.console.aws.amazon.com/s3/object/unchitterhighspeedvideo/1NTRSQ012519 09) 01 05.mov?region=us-east-1&tab=overview

data[which(data$Batter == "Martorano, Brandon" & data$PitchCall == "InPlay" & data$Scrimmage == "Season"), ] %>%
  arrange(desc(ExitSpeed)) %>%
  slice(1:25) %>%
  select(Batter, Date, PitcherTeam, ExitSpeed, Angle, PlayResult, Balls, Strikes, PlateLocHeight, PlateLocSide, key)
  

length(unique(data[which(!is.na(data$ExitSpeed)), "key"]))

bmart <- data[which(data$Batter == "Martorano, Brandon" & data$Scrimmage == "Season" & data$PitchCall == "InPlay" & endsWith(data$Date, "8")),]
vid <- bmart[order(bmart$ExitSpeed, decreasing = T)[1:20], c("Date", "ExitSpeed", "Angle", "PlayResult", "key")]
vid <- cbind(vid, url = as.character(paste0("https://s3.amazonaws.com/publicuncsynergyvideo/", substr(vid$key, 1, 12), "/", vid$key, ".mp4")))

