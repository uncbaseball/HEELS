reset.cumsum <- function(fullcumsum, reset)c(fullcumsum - c(cummax(fullcumsum * reset)))


one <- data[which(data$GameID == data[4000, "GameID"]),]


test <- cbind(one, Real_POP = reset.cumsum(cumsum(ifelse(one$PitchCall == "Undefined", 0 , 1)), factor.diff(one$PAofInning)) + 2)

View(test[,c("Batter", "PAofInning", "Real_POP", "PAofInning")])

table(data[which(data$PitchofPA > 1 & data$Count == "0-0" & data$AwayTeam != "NOR_TAR2" & data$PitcherTeam != "NOR_TAR2"), "PAofInning"])

PAofInning 1 -> number of non-Undefined PitchCalls w/ the PAofInning + 1


loop thru half-innings
substr(one$PlayID, 1, 17)

## good ids
goodids <- data[which(nchar(data$GameID) == 12),]

## loop thru half innings
tot = 0
for(halfinn in unique(substr(goodids$PlayID[which(goodids$Inning == 1)], 1, 16))){
  print(halfinn)
  if(!all(goodids[which(substr(goodids$PlayID, 1, 16) == halfinn & goodids$Count == "0-0" & goodids$PitchCall != "Undefined"), "PitchofPA"] == 1)){
    message("yes")
    tot = tot + 1
  }
}
if they have an 0-0 non-1 PitchofPA
  start with PAofInning 1
