d1 <- read.csv(file = "../TAR Database/UPDATE MICAH NCAA TRACKMAN DATA 10-2-18 - PART 1.csv")
d2 <- read.csv(file = "../TAR Database/UPDATE MICAH NCAA TRACKMAN DATA 10-2-18 - PART 2.csv")
d3 <- read.csv(file = "../TAR Database/UPDATE MICAH NCAA TRACKMAN DATA 10-2-18 - PART 4.csv")
d4 <- read.csv(file = "../TAR Database/UPDATE MICAH NCAA TRACKMAN DATA 10-2-18 - PART 5.csv")
d5 <- read.csv(file = "../TAR Database/UPDATE MICAH NCAA TRACKMAN DATA 10-2-18 - PART 6.csv")


lgdata <- rbind(d1, d2, d3, d4, d5)
names(lgdata)[3] <- "PitcherThrows"
lgdata <- generic.pitch.tag(lgdata)

ag <- lgdata %>% filter(!is.na(RelSpeed)) %>% group_by(PITCHERID) %>% summarise(pctFB = mean(AutoPitchType == "FB"), velo95 = quantile(RelSpeed, .95, na.rm = T))
plot(ag[,2:3])

ag <- as.data.frame(ag)

retags <- ag[which(ag$pctFB < .2),"PITCHERID"]
retag.df <- data.frame()
for(id in retags[441:length(retags)]){
  plot(lgdata[which(lgdata$PITCHERID == id),c("HorzBreak", "RelSpeed")], pch = 16)
  retag.df <- rbind(retag.df, data.frame(PITCHERID = id, as.data.frame(locator(n= 1))$y))
}

for(i in 1:nrow(retag.df)){
  lgdata[which(as.character(lgdata$PITCHERID) == retag.df[i, 1] & lgdata$RelSpeed > retag.df[i, 2]),"AutoPitchType"] <- "FB"
}


write.csv(lgdata, file = "College TrackMan for Pitch Values Tagged.csv", row.names = F)
# remove bunts

# swing/no swing model

# swinging strike / foul / single / double-triple / out - error - sacrifice / homerun | swing

# all of the above for LL/LR/RL/RR

lgdata <- read.csv(file = "College TrackMan for Pitch Values Tagged.csv", stringsAsFactors = F)
lgdata <- lgdata[which(lgdata$X0 != "NULL"),]
num.cols <- c("X0", "EXTENSION", "Z0", "VX0", "VY0", "VZ0", "AX0", "AY0", "AZ0")
for(i in num.cols) lgdata[,i] <- as.numeric(as.character(lgdata[,i]))

scale.avgs <- apply(lgdata[,num.cols], 2, mean)

centered <- lgdata[,num.cols] - matrix(scale.avgs, nrow = nrow(lgdata), ncol = length(scale.avgs), byrow = T)

scale.sd <- apply(centered, 2, sd)

scaled <- centered / matrix(scale.sd, nrow = nrow(lgdata), ncol = length(scale.sd), byrow = T)

fbavg <- cbind(PITCHERID = lgdata$PITCHERID, AutoPitchType = lgdata$AutoPitchType, scaled) %>%
  filter(AutoPitchType == "FB") %>%
  group_by(PITCHERID, AutoPitchType) %>%
  summarise_all(mean)

both <- merge(cbind(PITCHERID = lgdata$PITCHERID, scaled), fbavg[,-2], by = "PITCHERID", all = T, suffixes = c("", ".FB"))
for(i in num.cols) both[,paste0(i, ".FB")] <- both[,i] - both[,paste0(i,".FB")]
both$Swing <- as.numeric(lgdata$PITCHCALL %in% c("StrikeSwinging", "InPlay", "FoulBall"))
both <- cbind(both, lgdata[,c("BALLS", "STRIKES", "PitcherThrows", "BATTERSIDE")])

both[which(both$BATTERSIDE == "S"),"BATTERSIDE"] <- "Left"

both$SwingResult <- ifelse(!both$Swing, NA_character_, ifelse(lgdata$PITCHCALL %in% c("StrikeSwinging", "FoulBall"), "StrikeSwinging", ifelse(lgdata$PlayResult %in% c("Double", "Triple"), "Double-Triple", ifelse(lgdata$PlayResult %in% c("Out", "Error", "Sacrifice", "FieldersChoice"), "Out", lgdata$PlayResult))))

contact_type <- function(data){
  coords <- read.csv(file = "data/Batter LAEV Visual Coords.csv", stringsAsFactors = F)
  data %<>% mutate(XCoord = c(cos(Angle * pi / 180) * ExitSpeed), 
                   YCoord = c(sin(Angle * pi / 180) * ExitSpeed))
  for(contact.type in c("barrel", "solid", "F&B", "topped", "under")){
    data[which(!is.na(data$XCoord)), contact.type] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == contact.type),c("X", "Y")]), 
                                                                        as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  }
  names(data)[which(names(data) == "F&B")] <- "F.B"

  ####  null exit data for foul balls?
  data %<>% mutate(ContactType = ifelse(as.logical(barrel), "Barrel", ifelse(as.logical(solid), "Solid", ifelse(as.logical(`F.B`), "F&B", ifelse(as.logical(topped), "Topped", ifelse(as.logical(under), "Under", NA_character_))))))
  return(data$ContactType)
}

lgdata$Angle <- as.numeric(as.character(lgdata$Angle))
lgdata$ExitSpeed <- as.numeric(as.character(lgdata$ExitSpeed))

both$ContactType <- contact_type(lgdata)
both[which(lgdata$PITCHCALL != "InPlay"),"ContactType"] <- NA
both[which(both$ContactType == "Solid"),"ContactType"] <- "Barrel"

both$Swing <- as.factor(both$Swing)
ll <- both[which(both$BATTERSIDE == "Left" & both$PitcherThrows == "Right" & both$STRIKES == 2),]
library(nnet)
train <- sample(1:nrow(ll), size = .7 * nrow(ll), replace = F)
llmod <- nnet(Swing~X0+EXTENSION+Z0+VX0+VY0+VZ0+AX0+AY0+AZ0+X0.FB+EXTENSION.FB+Z0.FB+VX0.FB+VY0.FB+VZ0.FB+AX0.FB+AY0.FB+AZ0.FB+BALLS, data = ll[train,-c(1, 23, 24)], size = 7, linout = F, maxit = 1000)
pred <- predict(llmod, ll[-train,])
scatter.smooth(pred, as.numeric(as.character(ll[-train, "Swing"])))


ll2 <- both[which(both$BATTERSIDE == "Left" & both$PitcherThrows == "Left" & both$STRIKES == 2),]
train <- sample(1:nrow(ll2), size = .7 * nrow(ll2), replace = F)
ll2mod <- nnet(Swing~., data = ll2[train,-c(1, 22, 23, 24)], size = 7)
pred <- predict(ll2mod, ll2[train,-c(1, 22, 23, 24)])
scatter.smooth(pred, ll2[train, "Swing"])



bothswing <- cbind(both, ExitSpeed = as.numeric(lgdata$ExitSpeed))[which(!is.na(both$SwingResult) & both$SwingResult != "Undefined" & lgdata$HitType != "Bunt" & as.numeric(lgdata$ExitSpeed) > 0),]
bothswing$SwingResult <- as.factor(bothswing$SwingResult)


bothexpoutcomes <- applybbexpoutcomes(lgdata %>%rename(BatterSide = BATTERSIDE) %>% mutate(Bearing = as.numeric(as.character(Bearing))))

bothcontact <- cbind(ExitSpeed = lgdata$ExitSpeed, XBH = ifelse(lgdata$PlayResult %in% c("Double", "Triple", "HomeRun"), "XBH", "Not"), PredSLG = bothexpoutcomes$ExpSLG, PredOBP = bothexpoutcomes$ExpOBP, PredXBH = bothexpoutcomes$Pred_2B + bothexpoutcomes$Pred_3B + bothexpoutcomes$Pred_HR, Pred_HR = bothexpoutcomes$Pred_HR, Angle = lgdata$Angle, AngleRange = ifelse(lgdata$Angle < -15, "Topped", ifelse(lgdata$Angle > 35, "Under", "Solid")), both)[which(!is.na(both$ContactType)),]
bothcontact$ContactType <- as.factor(bothcontact$ContactType)
bothcontact$AngleRange <- as.factor(bothcontact$AngleRange)
bothcontact$XBH <- as.factor(bothcontact$XBH)


both$Barrel <- as.factor(as.numeric(both$ContactType %in% c("Barrel", "Solid")))

both$SwingMiss <- ifelse(both$SwingResult == "StrikeSwinging", "SwingMiss", "Contact")
both$SwingMiss <- as.factor(both$SwingMiss)
bothswing <- both[which(as.logical(both$Swing)),]
bothcontact <- both[which(!is.na(both$ContactType)),]

llcontact <- bothcontact[which(bothcontact$PitcherThrows == "Right" & bothcontact$BATTERSIDE == "Right" & bothcontact$STRIKES == 2),]
train <- sample(1:nrow(llcontact), size = .7 * nrow(llcontact), replace = F)
llcontactmod <- nnet(ExitSpeed~X0+EXTENSION+Z0+VX0+VY0+VZ0+AX0+AY0+AZ0+X0.FB+EXTENSION.FB+Z0.FB+VX0.FB+VY0.FB+VZ0.FB+AX0.FB+AY0.FB+AZ0.FB+BALLS,#+STRIKES, 
                     data = llcontact[train,], size =11, linout = T, maxit= 1000)

pred <- cbind.data.frame(predict(llcontactmod, llcontact[-train,]), as.character(llcontact[-train,"AngleRange"]))

hist(unname(pred)[which(!as.logical(as.numeric(as.character(llcontact[-train,"Barrel"]))))], col = adjustcolor("red", .3), probability = T, breaks = 30)
hist(unname(pred)[which(as.logical(as.numeric(as.character(llcontact[-train,"Barrel"]))))], col = adjustcolor("blue", .3), probability = T, breaks = 20, add = T)

pred %>% group_by(`as.character(llcontact[-train, "AngleRange"])`) %>% summarise_all(mean)
pred <- predict(llcontactmod, llcontact[-train,])
scatter.smooth(pred, 
               llcontact[-train, "ExitSpeed"],
               lpars = list(col = "red"))
pred <- predict(llcontactmod, llcontact[-train,])[,1]
loess.smooth(unname(pred), as.numeric(as.character(llcontact[-train,"Barrel"])))

llswing <- bothswing[which(bothswing$BATTERSIDE == "Left" & bothswing$PitcherThrows == "Left" & bothswing$STRIKES == 2),]
train <- sample(1:nrow(llswing), size = .7 * nrow(llswing), replace = F)
llswingmod <- nnet(SwingMiss~X0+EXTENSION+Z0+VX0+VY0+VZ0+AX0+AY0+AZ0+X0.FB+EXTENSION.FB+Z0.FB+VX0.FB+VY0.FB+VZ0.FB+AX0.FB+AY0.FB+AZ0.FB+BALLS+STRIKES, 
                   data = llswing[train,], size = 6, linout = F, maxit = 1000)
pred <- predict(llswingmod, llswing[-train,])
scatter.smooth(pred, as.numeric(as.character(llswing[-train, "SwingMiss"]) == "SwingMiss"))

pred <- cbind.data.frame(pred, as.character(llswing[-train,"SwingMiss"]) == "SwingMisss")
for(i in 1:5) pred[,i] <- as.numeric(as.character(pred[,i]))

pred %>% group_by(`as.character(llswing[-train, "SwingMiss"])`) %>% summarise_all(mean)
