#############   Essentials    ###########

### First 20 lines of Essentials.R
  ## mysql
  ## reticulate
  ## tar converters tar database/tar converters
  ## roster


lapply(c("plyr", "dplyr", "ggplot2", "lubridate", "rvest", "stringr", "tidyr", "mgcv", "assertthat", "default", "plotly", "png", "crosstalk", "magrittr"), require, character.only = T)

r.stuff <- "/users/micahdaley-harris/desktop/r_stuff"
this.year <- ifelse(as.numeric(substr(Sys.Date(), 6, 7)) >= 2, as.numeric(substr(Sys.Date(), 1, 4)), as.numeric(substr(Sys.Date(), 1, 4)) - 1)
strikezone <- data.frame(x = c(-0.7083, -0.7083, 0.7083, 0.7083, -0.7083), z = c(1.75, 3.3, 3.3, 1.75, 1.75))
real.strikezone <- read.csv(file = "data/Real Strike Zone.csv", stringsAsFactors = F)
gap <- 17 / 12 / 7
boshcoords <- read.csv("data/Boshamer Outline Points.csv", stringsAsFactors = F)


source("pitchtagging.R")
source("ReleasePointGraphs.R")
source("ApplyExpCalledStrike.R")
source("ApplyExpectedBatted-BallOutcomes.R")
source("HotCold2018.R")
source("ClassifyBattedBalls.R")
source("GenerateBPBreakdown.R")
source("PitcherReport2018.R")
source("PitchMovementandScouting.R")

####  LOAD DATA   ####

# load("/users/micahdaley-harris/desktop/tar/webscrape/NCAA Codes.RData")
# load("/users/micahdaley-harris/desktop/tar/webscrape/TBC Codes.RData")

#if(!exists("data") || !is.data.frame(data)) 
data <- read.csv(file = "data/uncall.csv", stringsAsFactors = F) # load("data/uncall.RData")
if(!exists("bp") || !is.data.frame(bp)) bp <- read.csv(file = "data/bp.csv", stringsAsFactors = F)# load("data/bp.RData")

## we could determine this by reading ui.R, extracting selected=
if(!exists("initialinputs") || !is.list(initialinputs)) initialinputs <- readRDS("data/initial chillmedia inputs.rds")

####  General Functions   #####
mode <- function(x) rev(names(sort(table(x))))[1]
mode.num <- function(x) rev(sort(table(x)))[1]
breakeven <- function(start, succ, fail){return((start - fail) / (succ - fail))}


## Text Formatting
psplit <- function(x) strsplit(x = x, split = ", ")[[1]][1]
"%startswith%" <- function(x, y) startsWith(as.character(x), as.character(y))
"%endswith%" <- function(x, y) endsWith(as.character(x), as.character(y))
remove.last <- function(x) x[-length(x)]
remove.spaces <- function(x) gsub(x, pattern = " ", replacement = "")
is.upper <- function(x){substr(x, 1, 1) %in% LETTERS} #strsplit(x, split = "") %in% LETTERS validate_that(class(x) == "character" & nchar(x) == 1); 
# eventually list, [[1]] is logical vector for each char of x[1], special chars currently return T


## Graphics
full.screen.plot <- function(rows = 1, cols = 1, ...){quartz(width = 11, height = 7); par(mfrow = c(rows, cols), ...)}
fsp <- full.screen.plot

## Number Formatting
percent <- function(x, digits = 0) round(100 * x, digits = digits)
round3 <- function(x) gsub(formatC(x, format = "f", digits = 3), pattern = "0\\.", replacement = ".")
leading0 <- function(x) ifelse(x < 10, paste0(0, x), x)

vector.ifelse <- function(tests, yeses, if.none){
  validate_that(!is.list(yeses), msg ="yeses should be a list")
  
  if(sum(tests) > 0){
    return(yeses[[min(which(tests))]])
  } else{
    return(if.none)
  }
}
factor.diff <- function(x) c(0, x[-1] != x[-length(x)])
reset.cumsum <- function(fullcumsum, reset) c(fullcumsum - c(cummax(fullcumsum * reset)[-1], 0))# reset is logical/binary

yn <- function(prompt){
  ret <- readline(prompt)
  while(!is.element(toupper(ret), c("Y", "N"))){
    ret <- readline("Enter either 'Y' or 'N'")
  }
  return(toupper(ret) == "Y")
}

default(table) <- list(useNA = "ifany")

slide.trackman.columns <- function(data){
  if(is.character(data)) data <- read.csv(file = data, stringsAsFactors = F)
  if(grepl("/", data[1,1]) | names(data)[1] == "row.names"){
    data[,c(2:ncol(data))] <- data[,c(1:(ncol(data) - 1))]
    data[,1] <- c(1:nrow(data))
  }
  return(data[,which(names(data) != "row.names")])
}

download.trackman <- function(gameid){
  read.csv(textConnection(getURL(paste0("http://152.19.152.25/trackman.online.baseball/ExportResponsePage.aspx?ApplicationType=TrackGame&useExt=False&gameId=", gameid))))
}

# sched <- dbGetQuery(tarconn, "SELECT * FROM `schedules` WHERE `Tm` = 'UNC'")
# sched <- cbind(sched, Game = paste(sched$Date, sched$Opponent, sep = " - "), yyyymmdd = paste0("20", str_sub(as.Date(sched$Date, format = "%m/%d/%Y"), 3, -1))
# for(gameid in unique(sched$yyyymmdd)) data[which(data$yyyymmdd == gameid), "Game"] <- as.character(sched[which(sched$yyyymmdd == gameid)[1], "Game"])

givetmdataids <- function(data, livebp = F, scrim = F){
  
  assert_that(!all(c(livebp, scrim)))
  
  if(livebp){
    data$GameID <- paste0("L1V3BP", condensedates(data$Date))
  } else if(scrim){
    data$GameID <- paste0("1NTRSQ", condensedates(data$Date))
  } else{
    data$GameID <- paste0(tar.team.abbrev.converter(x = data$AwayTeam, from = "TMAbbrev", to = "TARAbbrev"), 
                         tar.team.abbrev.converter(x = data$HomeTeam, from = "TMAbbrev", to = "TARAbbrev"), 
                         condensedates(data$Date))
  }
  
  data$PlayID <- paste0(data$GameID, " ", leading0(data$Inning),  ifelse(data$Top.Bottom == "Top", "(", ")"), 
                        " ", leading0(data$PAofInning))
  
  data %<>% select(GameID, PlayID, everything())
  # if(sum("GameID" %in% colnames(data)) > 1){
  #   data <- data[, -c(which(colnames(data) == "GameID")[2])]
  # }
  
  return(data)
  
  
}






## can extend to Pitch Type, 2 strikes
## option for default plot titling
## specify which hands/pitches (or preempt by seeing which are in the data)
splitLR <- function(data, fun, batter = T, pitcher = F, 
                    handcols = intersect(colnames(data), 
                                         c(vector.ifelse(tests = batter, yeses = list(c("BatterSide", "BSide")), if.none = c()), 
                                           vector.ifelse(tests = pitcher, yeses = list(c("PitcherThrows", "PSide")), if.none = c())))){
  
  for(col in handcols){
    for(side in c("L", "R")){
      subdat <- data[which(data[,handcols] %startswith% side),]
      if(nrow(subdat) > 0){
        fun(subdat)
      } else{
        warning(paste("no data found with", side, col))
      }
    }
  }
  
}


full.postgame <- function(data, our.catcher, their.catcher){
  ## Add option for multiple catchers
  bpbreakdown(data, game = T)
  full.screen.plot()
  pitcherreport(data, save = T)
  full.screen.plot()
  pitcher.report.by.batter(data)
  full.screen.plot()
  framing.breakdown(data[which(data$PitcherTeam == "NOR_TAR"),], catcher = our.catcher, save = T)
  full.screen.plot()
  framing.breakdown(data[which(data$PitcherTeam != "NOR_TAR"),], catcher = their.catcher, save = T)
  full.screen.plot()
  framing.breakdown(data, catcher = "All Catchers", unsurprising.calls = T, save = T)
  
  
}

make.button <- function(button.order, tot.buttons, loc = "top", txt, txt.cex = 1, button.col = "grey", text.col = "blue"){
  width <- ifelse(loc == "top", diff(par("usr")[c(1, 2)]) / tot.buttons, NA)
  left <- par("usr")[1] + (button.order - 1) * width
  right <- left + width
  top <- ifelse(loc == "top", par("usr")[4], NA)
  height <- ifelse(loc == "top", diff(par("usr")[c(3,4)]) / 11)
  bot <- top - height
  rect(xleft = left, ybottom = bot, xright = right, ytop = top, col = button.col)
  text(mean(c(left, right)), mean(c(top, bot)), labels = txt, cex = txt.cex, col = text.col)
  return(data.frame(x = c(left, left, right, right), y = c(bot, top, top, bot)))
}

make.button2 <- function(button.order, tot.buttons, loc = "top", txt, txt.cex = 1, button.col = "grey", text.col = "blue"){
  width <- ifelse(loc == "top", diff(par("usr")[c(1, 2)]) / tot.buttons, NA)
  left <- par("usr")[1] + (button.order - 1) * width
  right <- left + width
  top <- ifelse(loc == "top", par("usr")[4], NA)
  height <- ifelse(loc == "top", diff(par("usr")[c(3,4)]) / 11)
  bot <- top - height
  rect(xleft = left, ybottom = bot, xright = right, ytop = top, col = button.col)
  text(mean(c(left, right)), mean(c(top, bot)), labels = txt, cex = txt.cex, col = text.col)
  return(function(x)in.out(as.matrix(x), bnd = matrix(data = c(left, left, right, right, bot, top, top, bot), byrow = F, nrow = 2, ncol = 2)))
}



clear.buttons <- function(loc = "top"){
  if(loc == "top"){
    rect(xleft = par("usr")[1], ybottom = par("usr")[4] - diff(par("usr")[c(3, 4)]) / 11, xright = par("usr")[2], ytop = par("usr")[4], col = "white", border = "white")
  }
  
}


plot.pitches <- function(data = data.frame(), show.strikezone = T, show.real.strikezone = T, shape = c("pitchtype", "Game", "ABNum", 16), type = c("PlateLoc", "Contact"),
                         star.fb = F, star.lhb = F, star.2k = F, p.view = F, pitch.numbers = T, bside = "Right", pitch.numbers.col = "orange",
                         plot.title, give.axes = F, point.size = 2.4, legend.type = c("full", "take", "contact result", "full with contact result"), legend.border = "black", legend.cex = 1.2, legend.pars = list(legend.border = "black", legend.cex = 1.2, x = "bottomright")){
  legend.type <- match.arg(legend.type[1], c("full", "take", "contact result", "full with contact result", "laev", "none"))
  shape <- match.arg(as.character(shape[1]), c("pitchtype", "Game", "ABNum", 16))
  type <- match.arg(type[1], c("PlateLoc", "Contact"))
  
  ## do.call with legend.pars
  
  if(p.view){
    lhb.pic <- readPNG("data/Release Point Pics/LHB Pitcher View.png")
    rhb.pic <- readPNG("data/Release Point Pics/RHB Pitcher View.png")
  } else{
    lhb.pic <- readPNG("data/LHB Catcher View.png")
    rhb.pic <- readPNG("data/RHB Catcher View.png")
  }
  
  
  #### Strike Zones ####
  plot(strikezone$x, strikezone$z, type = ifelse(show.strikezone, "l", "n"), xlim = c(-3, 3), ylim = c(ifelse(type == "Contact", -2, -0.5), 5), axes = give.axes, col = "red", main = ifelse(missing(plot.title), ifelse(nrow(data) == 0, "", paste(mode(data$Pitcher), "vs.", mode(data$Batter))), plot.title), xlab = "", ylab = "", lwd = 3)#ifelse(p.view, "Pitcher View", "Catcher View")
  
  abline(h = 0, lwd = 1.5)
  
  if(nrow(data) > 0) bside <- mode(data$BatterSide)
  lefty <- as.numeric(bside %startswith% "L")
  other.real.strikezone <- real.strikezone[which(real.strikezone$BatterSide != bside), c("PlateLocSide", "PlateLocHeight")]
  real.strikezone <- real.strikezone[which(real.strikezone$BatterSide == bside), c("PlateLocSide", "PlateLocHeight")]
  
  
  if(show.real.strikezone){
    points(real.strikezone[c(1:nrow(real.strikezone), 1),1] * ifelse(p.view, 1, -1), real.strikezone[c(1:nrow(real.strikezone), 1),2], type = "l", lwd = 2, col = ifelse(lefty, "blue", "hotpink"))
    if(length(which(data$BatterSide != bside)) >= 2 | bside == "Both"){
      points(other.real.strikezone[,1] * ifelse(p.view, 1, -1), other.real.strikezone[,2], type = "l", lwd = 2, col = ifelse(lefty, "hotpink", "blue"))
    }
  }
  
  #### Batter Pics and Balls Off  ####
  
  if(lefty){
    rasterImage(lhb.pic, ifelse(p.view, -2.75, 1.5), ifelse(type == "Contact", -1.5, 0), ifelse(p.view, -1.5, 2.75), ifelse(type == "Contact", 0, 5))
    text(seq(-gap * 6, gap * 6, by = gap), rep(ifelse(type == "Contact", -1, 1.45), 13), c(3:1, 7:1, 1:3), col = c(rep("purple", 3), rep("hotpink", 7), rep("purple", 3)))
    
  } else{
    rasterImage(rhb.pic, ifelse(p.view, 1.5, -2.75), ifelse(type == "Contact", -1.5, 0), ifelse(p.view, 2.75, -1.5), ifelse(type == "Contact", 0, 5))
    text(seq(-gap * 6, gap * 6, by = gap), rep(ifelse(type == "Contact", -1, 1.45), 13), c(3:1, 1:7, 1:3), col = c(rep("purple", 3), rep("hotpink", 7), rep("purple", 3)))
    
  }
  
  text(-2, 1.45, "Balls off:", font = 2)
  segments(x0 = seq(-gap * 7, gap * 5, by = gap) + gap / 2, x1 = seq(-gap * 7, gap * 5, by = gap) + gap / 2, y0 = ifelse(type == "Contact", -1.15, 1.3), y1 = 4, lty = 2)
  
  
  if(nrow(data) > 0){
    data$PlateLocSide <- as.numeric(as.character(data$PlateLocSide))
    data$PlateLocHeight <- as.numeric(as.character(data$PlateLocHeight))
    
    data <- data[order(data$PitchNo, decreasing = F),]
    
    ####  Assign Shapes ####
    data$ABNumPCH <- NA
    abshapes <- c(15, 16, 17, 18, 1, 8, 11, 4)
    abnum <- 1
    for(ab in unique(paste(data$Inning, data$PAofInning))){
      data[which(paste(data$Inning, data$PAofInning) == ab),"ABNumPCH"] <- c(abshapes[abnum])
      data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] <- data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] - min(data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"], na.rm = T) + 1
      abnum <- abnum + 1
    }
    
    
    if(any(data$TaggedPitchType == "Undefined" & !is.na(data$RelSpeed) & !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2")))){
      data[which(data$TaggedPitchType == "Undefined" & !is.na(data$RelSpeed) & !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))), "TaggedPitchType"] <- generic.pitch.tag(data[which(data$TaggedPitchType == "Undefined" & !is.na(data$RelSpeed) & !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))), ])$AutoPitchType
    }
    
    
    if(shape == "pitchtype"){
      ifelse <- dplyr::if_else
      
      data <- cbind(data, PitchTypePCH = ifelse(data$TaggedPitchType %in% c("CU", "BB"), 17, 
                                                ifelse(data$TaggedPitchType == "SL", 18, 
                                                  ifelse(data$TaggedPitchType == "CH", 15, 
                                                         ifelse(data$TaggedPitchType %in% c("FB", "FT", "FC", "FS"), 16, 8)))))
      
    } else if(shape == "Game"){
      data <- data[order(data$Date, decreasing = F),] #lubridate here
      data$GamePCH <- NA
      abshapes <- c(15, 16, 17, 18, 1, 8, 11, 4)
      abnum <- 1
      for(ab in unique(data$Date)){
        data[which(data$Date == ab),"GamePCH"] <- c(abshapes[abnum])
        data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] <- data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] - min(data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"], na.rm = T) + 1
        abnum <- abnum + 1
      }
    } 
    
    ####  Assign Colors ####
    data <- cbind(data, PitchResCol = as.character(ifelse(data$PitchCall == "InPlay", "blue",
                                                          ifelse(is.element(data$PitchCall, c("BallCalled", "BallIntentional")), "forestgreen",  
                                                                 ifelse(data$PitchCall == "StrikeCalled", "red",
                                                                        ifelse(is.element(data$PitchCall, c("StrikeSwinging", "FoulBall")), "darkred", "black"))))))
    
    if(legend.type %endswith% "contact result"){
      data$PitchResCol <- ifelse(data$PitchCall != "InPlay", as.character(data$PitchResCol), 
                                 ifelse(data$PlayResult %in% c("Out", "Error", "Sacrifice", "FieldersChoice"), "purple", 
                                        ifelse(data$PlayResult == "Single", "pink", 
                                               ifelse(data$PlayResult %in% c("Double", "Triple"), "orange", 
                                                      ifelse(data$PlayResult == "HomeRun", "gold", "black")))))
    } else if(legend.type == "laev"){
     data$PitchResCol <- data$LAEVcol
   }
    
    points(ifelse(p.view, 1, -1) * data$PlateLocSide, data$PlateLocHeight, pch = vector.ifelse(tests = c(shape == "ABNum", shape == "pitchtype", shape == "Game"), yeses = list(data$ABNumPCH, data$PitchTypePCH, data$GamePCH), if.none = 16), col = as.character(data$PitchResCol), cex = point.size)
    
    
    #### Stars
    if(star.fb & length(which(data$TaggedPitchType == "FB")) > 0){
      points(data[which(data$TaggedPitchType == "FB"), "PlateLocSide"] * ifelse(p.view, 1, -1) + .05, data[which(data$TaggedPitchType == "FB"), "PlateLocHeight"] + .05, pch = 8, cex = point.size / 3, col = "yellow")
    }
    
    if(star.lhb){
      points(data[which(data$BatterSide == "Left"), "PlateLocSide"] * ifelse(p.view, 1, -1) + ifelse(star.fb, -1, 1) * .05, data[which(data$BatterSide == "Left"), "PlateLocHeight"] + .05, pch = 8, col = ifelse(star.fb, "orange", "yellow"))
    }
    
    if(star.2k){
      
    }
    
    if(pitch.numbers){
      text(data$PlateLocSide * ifelse(p.view, 1, -1), data$PlateLocHeight, data$PitchofPA, col = pitch.numbers.col, cex = 0.9)
    }
  } 
  
  #### Legends
  if(shape == "pitchtype"){
    legend("topright", legend = c("FB", "CH", "CU", "SL"), col = "black", pch = c(16, 15, 17, 18))
  } else if(shape %in% c("ABNum", "Game")){
    legend("topright", legend = vector.ifelse(tests = (shape == "ABNum"), yeses = list(c("PA 1", "PA 2", "PA 3", "PA 4", "PA 5", "PA 6", "PA 7", "PA 8")[c(1:(abnum - 1))]), if.none = unique(data$Date)), col = "black", pch = abshapes[c(1:(abnum - 1))])
  }
  
  if(sum(as.numeric(c(star.fb, star.lhb))) == 2){
    legend("bottomright", legend = c("Fastball", "LHB"), pch = 8, col = c("yellow", "orange"), cex = 1)
  } else if(sum(as.numeric(c(star.fb, star.lhb))) == 1){
    legend("bottomright", legend = ifelse(star.fb, "Fastball", "LHB"), pch = 8, col = "yellow", cex = 1)
  }
  
  if(legend.type == "full"){
    legend("bottomright", legend = c("In Play", "Ball", "Swinging Strike/Foul Ball", "Called Strike", "Hit by Pitch"), pch = 16, col = c("blue", "forestgreen", "darkred", "red", "black"), cex = legend.cex, border = legend.border)
  } else if (legend.type == "contact result"){
    #legend("topleft", legend = c("In Play", "Ball", "Swinging Strike/Foul Ball", "Called Strike"), pch = 16, col = c("blue", "forestgreen", "darkred", "red"), cex = legend.cex, border = legend.border)
    legend("topleft", legend = c("Out", "Single", "Double/Triple", "Home Run"), pch = 16, col = c("purple", "pink", "orange", "gold"), cex = legend.cex, border = legend.border)
  } else if(legend.type == "take"){
    legend("topleft", legend = c("Ball", "Called Strike"), pch = 16, col = c("forestgreen", "red"), cex = legend.cex, border = legend.border)
  } else if(legend.type == "laev"){
    legend("bottomright", legend = c("Barrel", "Solid Contact", "Flares & Burners", "Under", "Topped", "Weak", "No Hit Data"), pch = 16, col = c("red", "lightpink", "coral", "lightskyblue", "darkgreen", "tan", "white"), cex = legend.cex, border = legend.border)
  } else if(legend.type != "none"){
    warning("unrecognized legend.type")
  }
  
  
}


plot.pitches.gg <- function(data = data.frame(), show.strikezone = T, show.real.strikezone = T, shape.type = c("pitchtype", "Game", "ABNum", 16), type = c("PlateLoc", "Contact"),
                                            star.fb = F, star.lhb = F, star.2k = F, p.view = F, pitch.numbers = T, bside = "Right", tag.pitches = F,
                                            plot.title, give.axes = F, fixaxes = F, point.size = 2.4, dragmode = "lasso", returndata = F,
                            legend.type = c("full", "take", "contact result", "full with contact result"), legend.border = "black", legend.cex = 1.2){
  legend.type <- match.arg(legend.type[1], c("full", "take", "contact result", "full with contact result", "none"))
  shape.type <- match.arg(as.character(shape.type[1]), c("pitchtype", "Game", "ABNum", 16))
  type <- match.arg(type[1], c("PlateLoc", "Contact"))
  
  
  if(p.view){
    lhb.pic <- readPNG("data/LHB P View Real Zone.png")
    rhb.pic <- readPNG("data/RHB P View Real Zone.png")
  } else{
    lhb.pic <- readPNG("data/LHB Real Zone.png")
    rhb.pic <- readPNG("data/RHB Real Zone.png")
  }
  

  
  #### Strike Zones ####
  
  if(is.data.frame(data) && nrow(data) > 0) bside <- mode(data$BatterSide)
  lefty <- as.numeric(bside %startswith% "L")
  
  other.real.strikezone <- real.strikezone[which(real.strikezone$BatterSide != bside), c("PlateLocSide", "PlateLocHeight")]
  real.strikezone <- real.strikezone[which(real.strikezone$BatterSide == bside), c("PlateLocSide", "PlateLocHeight")]
  
  
  if(is.data.frame(data) && nrow(data) > 0){
    data$PlateLocSide <- as.numeric(as.character(data$PlateLocSide)) * ifelse(p.view, 1, -1)
    data$PlateLocHeight <- as.numeric(as.character(data$PlateLocHeight))
    
    data <- data[order(data$PitchNo, decreasing = F),]
    
    if(!is.element("thekey", colnames(data))) data$thekey <- paste(data$PlayID, data$PitchofPA)
    
    ####  Assign Shapes ####
    data$ABNumPCH <- NA
    abshapes <- c(15, 16, 17, 18, 1, 8, 11, 4)
    names(abshapes) <- c(1:length(abshapes))
    abnum <- 1
    for(ab in unique(paste(data$Inning, data$PAofInning))){
      data[which(paste(data$Inning, data$PAofInning) == ab),"ABNumPCH"] <- abnum#c(abshapes[abnum])
      data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] <- data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] - min(data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"], na.rm = T) + 1
      abnum <- abnum + 1
    }
    
    if(mode(data$PitcherTeam) %in% c("NOR_TAR", "NOR_TAR2") & tag.pitches) data <- pitchtagging(data)
    if(any(data$TaggedPitchType == "Undefined" & !is.na(data$RelSpeed) & !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2")) & tag.pitches)){
      data[which(data$TaggedPitchType == "Undefined" & !is.na(data$RelSpeed) & !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))), "TaggedPitchType"] <- generic.pitch.tag(data[which(data$TaggedPitchType == "Undefined" & !is.na(data$RelSpeed) & !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))), ])$AutoPitchType
    }
    
    
    if(shape.type == "Game"){
      data <- data[order(data$Date, decreasing = F),] #lubridate here
      data$GamePCH <- NA
      abshapes <- c(15, 16, 17, 18, 1, 8, 11, 4)
      
      abnum <- 1
      for(ab in unique(data$Date)){
        data[which(data$Date == ab),"GamePCH"] <- c(abshapes[abnum])
        data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] <- data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"] - min(data[which(paste(data$Inning, data$PAofInning) == ab),"PitchofPA"], na.rm = T) + 1
        abnum <- abnum + 1
      }
      
    } 
    
    ####  Assign Colors ####
   
    if(legend.type %endswith% "contact result"){
      data$PitchResCol <- ifelse(data$PitchCall != "InPlay", as.character(data$PitchResCol), 
                                 ifelse(data$PlayResult %in% c("Out", "Error", "Sacrifice", "FieldersChoice"), "purple", 
                                        ifelse(data$PlayResult == "Single", "pink", 
                                               ifelse(data$PlayResult %in% c("Double", "Triple"), "orange", 
                                                      ifelse(data$PlayResult == "HomeRun", "gold", "black")))))
    }
    
    data$key <- data$thekey#rownames(data)
    
    data[which(data$PitchCall == "InPlay"), "PitchCall"] <- gsub(data[which(data$PitchCall == "InPlay"), "PlayResult"], pattern = "HomeRun", replacement = "Home Run")
  } 
  

  pitchtype.shapes <- c("FB" = 16, "FT" = 16, "FC" = 18, "FS" = 16, "CH" = 15, "SL" = 18, "CU" = 17, "BB" = 17)

  # g <- plot_ly(colors = c("InPlay" = "blue", "BallCalled" = "forestgreen", "BallIntentional" = "forestgreen", "StrikeCalled" = "red", "StrikeSwinging" = "darkred", "FoulBall" = "darkred", "HitByPitch" = "black",  "Undefined" = 'white'))
  # if(show.strikezone) g <- g %>% add_paths(x = ~x, y = ~z, data = strikezone, name = 'Strike Zone', color = I('red'), showlegend = F)
  # if(show.real.strikezone) g <- g %>% add_paths(x = ~PlateLocSide, y = ~PlateLocHeight, data = real.strikezone[c(1:nrow(real.strikezone), 1),], name = paste0(ifelse(lefty, "L", "R"), "HB Real Strike Zone"), color = I(ifelse(lefty, 'blue', 'pink')), showlegend = F)
  # if(show.real.strikezone & (length(which(data$BatterSide != bside)) >= 2 | bside == "Both")) g <- g %>% add_paths(x = ~PlateLocSide, y = ~PlateLocHeight, data = other.real.strikezone[c(1:nrow(other.real.strikezone), 1),], name = paste0(ifelse(lefty, "R", "L"), "HB Real Strike Zone"), color = I(ifelse(!lefty, 'blue', 'pink')))
  # g <- g %>%
  #   add_trace(data = data, x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
  #              symbol = ~TaggedPitchType, symbols = pitchtype.shapes, marker = list(color = "black", size = 5)) %>%
  #   add_trace(data = data, x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'marker',
  #             color = ~as.factor(PitchCall)) %>%

  # axes = give.axes
  
  
  if(returndata) return(data)
  
  gg <- ggplot(data, aes(PlateLocSide, PlateLocHeight, text = paste("Result:", gsub(gsub(gsub(PitchCall, pattern = "FoulBall", replacement = "Foul"), pattern = "BallCalled", replacement = "Ball"), pattern = "([A-Z][a-z]+)([A-Z][a-z]+)", replacement = "\\2 \\1"),#key = key
                                                                               "<br>Pitch:", TaggedPitchType,
                                                                               "<br>Count:", paste(Balls, Strikes, sep = "-"),
                                                                               "<br>Date:", Date,
                                                                               "<br>Batter:", Batter,
                                                                               "<br>Inning:", Inning))) +
    #{if(show.strikezone) geom_rect(aes(xmin = min(strikezone$x), xmax = max(strikezone$x), ymin = min(strikezone$z), ymax = max(strikezone$z)), colour = 'red', fill = NA, size = I(2)) }+
    geom_point(aes(colour = factor(PitchCall), shape = factor(TaggedPitchType)), size = I(point.size)) + # , key = key #factor(ifelse(shape == "pitchtype", PitchTypePCH, ifelse(shape =="ABNum", ABNumPCH, ifelse(shape == "Game", GamePCH, 16)))))) +
    scale_color_manual(name = "\nPitch Result, Type", values = c("Out" = "blue", "Single" = "blue", "Double" = "blue", "Triple" = "blue", "Home Run" = "blue", "InPlay" = "blue", "BallCalled" = "forestgreen", "BallIntentional" = "forestgreen", "StrikeCalled" = "red", "StrikeSwinging" = "darkred", "FoulBall" = "darkred", "HitByPitch" = "black",  "Undefined" = 'white', "Sacrifice" = "blue")) +
    {if(shape.type == "ABNumPCH") scale_shape_manual(values = abshapes)} +#shape == "ABNumPCH"
    {if(shape.type == "pitchtype") scale_shape_manual(values = pitchtype.shapes)} + #shape == "pitchtype"
    #   #{if(pitch.numbers) geom_text(aes(PlateLocSide, PlateLocHeight, label = PitchofPA, colour = 'orange', size = 0.9)) }+    
       # {if(show.real.strikezone) geom_path(aes(PlateLocSide, PlateLocHeight, key = key), colour = ifelse(lefty, "blue", "hotpink"), lwd = 1.3, data = data.frame(PlateLocSide = real.strikezone[c(1:nrow(real.strikezone), 1),1] * ifelse(p.view, 1, -1), PlateLocHeight = real.strikezone[c(1:nrow(real.strikezone), 1),2], key = 'realstrikezone')) }+
    #   #{if(show.real.strikezone & (length(which(data$BatterSide != bside)) >= 2 | bside == "Both")) geom_path(aes(PlateLocSide, PlateLocHeight, key = key), colour = ifelse(lefty, "hotpink", "blue"), lwd = 1.3, data = data.frame(PlateLocSide = other.real.strikezone[,1] * ifelse(p.view, 1, -1), PlateLocHeight = other.real.strikezone[,2]), key = 'otherrealstrikezone') }+
    scale_x_continuous(limits = c(-3, 3), labels = NULL) +
    scale_y_continuous(limits = c(ifelse(type == "Contact", -2, 0), 5), labels = NULL) +
    labs(title = "", x = "", y = "", color = "Pitch Type, Result", shape = "") #ifelse(missing(plot.title), ifelse(nrow(data) == 0, "", paste(mode(data$Pitcher), "vs.", mode(data$Batter))), plot.title)
  
  
  g <- ggplotly(gg, tooltip = "text") %>%# source = 'loc', 
    # add_segments(x = -3, xend = 3, y = 0, yend = 0, showlegend = F) %>%
    # add_segments(x = seq(-gap * 7, gap * 5, by = gap) + gap / 2, xend = seq(-gap * 7, gap * 5, by = gap) + gap / 2, y = ifelse(type == "Contact", -1.15, 1.3), yend = 4, color = I('black'), line = list(dash = 'dash'), showlegend = F) %>%
    # add_text(x = seq(-gap * 6, gap * 6, by = gap), y = rep(ifelse(type == "Contact", -1, 1.45), 13), text = c(3:1, 1:7, 1:3), color = I(c(rep("purple", times = 3), rep("pink", times = 7), rep("purple", times = 3))), showlegend = F) %>%
    # add_text(x= -2.5, y= 1.45, text = "<b>Balls off:", showlegend = F) %>%
    
    layout(title = "",#ifelse(missing(plot.title), ifelse(nrow(data) == 0, "", paste(mode(data$Pitcher), "vs.", mode(data$Batter))), plot.title)
           xaxis = list(title = '', range = c(-4, 4), visible = F, fixedrange = fixaxes), yaxis = list(title = '', range = c(-0.5, 5), showgrid = F, visible = F, fixedrange = fixaxes),
           images = list(
             source = raster2uri(vector.ifelse(tests = as.logical(c(lefty, !lefty)), yeses = list(lhb.pic, rhb.pic), if.none = list())),
             xref = "x", yref = "y",  x = -4, y = -.5, sizex = 8, sizey = 5.5, #x = 1.5, y = 0, sizex = 1.5, sizey = 5
             sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below"
           ), dragmode = dragmode)

  layout(title = "",#ifelse(missing(plot.title), ifelse(nrow(data) == 0, "", paste(mode(data$Pitcher), "vs.", mode(data$Batter))), plot.title)
         xaxis = list(title = '', range = c(-4, 4), visible = F, fixedrange = fixaxes), yaxis = list(title = '', range = c(-0.5, 5), showgrid = F, visible = F, fixedrange = fixaxes),
         images = list(
           source = raster2uri(vector.ifelse(tests = as.logical(c(lefty, !lefty)), yeses = list(lhb.pic, rhb.pic), if.none = list())),
           xref = "x", yref = "y", x = -3, y = 0, sizex = 1.5, sizey = 5,
           sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below"
         ), dragmode = dragmode)
  

  return(g)
  
}


generic.pitch.tag <- function(data){
  
  ## Intentional balls
  
  if(nrow(data) == 0){
    stop("no data")
  }
  
  if(any(!is.element(c("HorzBreak", "RelSpeed", "PitcherThrows"), colnames(data)))){
    stop("'HorzBreak', 'RelSpeed' and/or 'PitcherThrows' were not found in the data")
  }
  
  if(!is.element("AutoPitchType", colnames(data))){
    warning("'AutoPitchType' was not found in the data so it was added")
  }
  
  if(any(!is.element(data$PitcherThrows, c("Left", "Right")))){
    warning("Not all PitcherThrows were 'Left' or 'Right'")
    # should eventually deduce based on L/R (if high velo)
  }
  
  data$AutoPitchType <- c("Und")
  data$RelSpeed <- as.numeric(as.character(data$RelSpeed))
  data$HorzBreak <- as.numeric(as.character(data$HorzBreak))
  
  
  pitch.tag.boxes <- read.csv(file = "data/3 Pitch Tag Boxes.csv", stringsAsFactors = F)
  
  data <- cbind(data, Index = c(1:nrow(data)))
  missingdata <- data[which(is.na(data$RelSpeed) | is.na(data$HorzBreak) | !is.element(substr(data$PitcherThrows, 1, 1), c("L", "R"))),]
  data <- data[which(!is.na(data$RelSpeed) & !is.na(data$HorzBreak) & is.element(substr(data$PitcherThrows, 1, 1), c("L", "R"))),]
  l <- data[which(substr(data$PitcherThrows, 1, 1) == "L"),]
  data <- data[which(substr(data$PitcherThrows, 1, 1) != "L"),]
  
  hb <- 5
  
  if(nrow(data) == 0){
    if(nrow(l) > 0){
      data <- l
      l <- data.frame()
    } else{
      stop("no non-missing data found")
    }
  }
  
  for(pside in c("Right", "Left")){
    #for(pside in c("Right")){
    
    if(nrow(data) > 0){
      #print(pside)
      #next
    }
    data[which(in.out(bnd = as.matrix(as.data.frame(pitch.tag.boxes[which(pitch.tag.boxes$Pitch == "FB"), c(1,2)])), x = as.matrix(as.data.frame(data[, c("RelSpeed", "HorzBreak")])))), "AutoPitchType"] <- c("FB")
    data[which(in.out(bnd = as.matrix(as.data.frame(pitch.tag.boxes[which(pitch.tag.boxes$Pitch == "CH"), c(1,2)])), x = as.matrix(as.data.frame(data[, c("RelSpeed", "HorzBreak")])))), "AutoPitchType"] <- c("CH")
    data[which(in.out(bnd = as.matrix(as.data.frame(pitch.tag.boxes[which(pitch.tag.boxes$Pitch == "BB"), c(1,2)])), x = as.matrix(as.data.frame(data[, c("RelSpeed", "HorzBreak")])))), "AutoPitchType"] <- c("BB")
    
    
    
    if(pside == "Right"){
      data[which(data$AutoPitchType == "Und"), "AutoPitchType"] <- ifelse(data[which(data$AutoPitchType == "Und"), "RelSpeed"] >= 88, "FB", ifelse(data[which(data$AutoPitchType == "Und"), "HorzBreak"] >= 5, "CH", "BB"))
      pitch.tag.boxes$y <- pitch.tag.boxes$y * -1
      if(nrow(l) == 0){
        indind <- which(colnames(data) == "Index")
        data <- rbind(data, missingdata)
        return(data[order(data$Index, decreasing = F), -indind])
      }
      r <- data
      data <- l
    } else{
      data[which(data$AutoPitchType == "Und"), "AutoPitchType"] <- ifelse(data[which(data$AutoPitchType == "Und"), "RelSpeed"] >= 88, "FB", ifelse(data[which(data$AutoPitchType == "Und"), "HorzBreak"] <= -5, "CH", "BB"))
    }
  }
  indind <- which(colnames(data) == "Index")
  
  data <- rbind(data, missingdata, r)
  
  return(data[order(data$Index, decreasing = F), -indind])
  
}


color.by.value <- function(values, cols, interval, min.val, replacena = NA, printnas = F){
  
  #values[which(values == 1)] <- c(.99)
  values[which(values <= min.val)] <- min.val
  values[which(values > (min.val + interval * (length(cols) - 1)))] <- c(min.val + interval * (length(cols) - 1)) - interval / 100
  values <- round_any(values, interval, floor)
  withcols <- data.frame(Value = as.numeric(values), id = c(1:length(values)))
  withcols$Value <- as.character(withcols$Value)
  key <- data.frame(Value = seq(min.val, min.val + interval * (length(cols) - 1), by = interval), Col = cols)
  withcols <- merge(withcols, key, by = "Value", all.x = T, all.y = F)
  withcols$Col <- as.character(withcols$Col)
  
  if(printnas){
    print(withcols[which(is.na(withcols$Col)), "Value"])
  }
  withcols[which(is.na(withcols$Col)), "Col"] <- c(replacena)
  #return(withcols)
  return(as.character(withcols[order(withcols$id, decreasing = F), "Col"]))
}


  ## Fix PitchofPA from Warmups -- 

# data <- data[order(data$GameID, data$PitchNo),]
# data[which(data$PitchCall %in% c("Undefined") & c(T, data$Balls[-1] == data$Balls[-nrow(data)]) & c(T, data$Strikes[-1] == data$Strikes[-nrow(data)])), "PitchCall"] <- c("Warmup")
# data[which(data$Strikes < 2), "NewPitchofPA"] <- rowSums(data[which(data$Strikes < 2), c("Balls", "Strikes")]) + 1
# data[which(data$Strikes == 2),"NewPitchofPA"] <- 0
# data[which(data$PitchCall == "Warmup"),"NewPitchofPA"] <- NA
# for(row in which(data$NewPitchofPA == 0)) data[row, "NewPitchofPA"] <- data[row - 1, "NewPitchofPA"] + 1
# data$key <- paste(data$PlayID, leading0(data$NewPitchofPA))
# 350 increments > 2, 90% match to PitchofPA
  #reset.cumsum(cumsum(as.numeric(data$PitchCall != "Warmup")), as.numeric(data$Balls == 0 & data$Strikes == 0))

# want to use 0-0 counts, make sure none of them are in the middle of an AB, ~900 ABs have multiple 0-0 counts, ~ 33 games


# load("data/Batted Ball Models.RData")
# scale.bear
# scale.laev
# 
# data[which(!is.na(data$Bearing) & data$ExitSpeed < 6), "ExitSpeed"] <- scale.bear["ExitSpeed", "scale"] * data[which(!is.na(data$Bearing) & data$ExitSpeed < 6), "ExitSpeed"] + scale.bear["ExitSpeed", "center"]
# data[which(is.na(data$Bearing) & data$ExitSpeed < 6), "ExitSpeed"] <- scale.laev["ExitSpeed", "scale"] * data[which(is.na(data$Bearing) & data$ExitSpeed < 6), "ExitSpeed"] + scale.laev["ExitSpeed", "center"]


avg.ev.la <- function(gameids){
  data[which(data$GameID %in% gameids & data$PitchCall == "InPlay" & data$BatterTeam %in% c("NOR_TAR", "NOR_TAR2")),c("Date", "Batter", "Pitcher", "Inning", "ExitSpeed", "Angle")] %>% 
    group_by(Batter) %>% summarise("Avg Exit Velo" = mean(pmax(65, ExitSpeed, na.rm = T)), "Avg Launch Angle" = mean(Angle, na.rm = T), "Balls In Play With TM Data" = sum(!is.na(ExitSpeed)))
}

fix.warmup.pitchofpa <- function(game){
  game[which(game$PitchCall == "Undefined") , "PitchofPA"] <- 0
  game[which(game$Balls == 0 & game$Strikes == 0 & game$PitchCall != "Undefined") , "PitchofPA"] <- 1

  for(row in 2: nrow(game)){
    if(game[row, "PitchofPA"] != 0 & game[row, "PitchofPA"] != 1){
      game[row,"PitchofPA"] <- game[row -1, "PitchofPA"] + 1
    }
  }
    
  return(game)
}
    




add.predicted.columns <- function(data, p.view = T, livebp = F, scrim = F, opponent = ""){
  
  assert_that(livebp | scrim | opponent != "")
  
  coords <- read.csv(file = "data/Batter LAEV Visual Coords.csv", stringsAsFactors = F)
  
  if(endsWith(mode(data$Date), "/19")) data$Date <- paste0(str_sub(data$Date, 1, -3), "2019")
  
  data <- tag.with.df(data)
  
  data <- givetmdataids(data, livebp = livebp, scrim = scrim)
  
  # derive opponent from GameID
 
  data %<>% mutate(
    #Game = paste(Date, " - ", ifelse(scrim, "Scrimmage", ifelse(livebp, "Live BP", opp))),
                   yyyymmdd = paste0("20", str_sub(as.Date(Date, format = "%m/%d/%Y"), 3, -1)),
                   key = paste(PlayID, PitchofPA),
                   PlateLocSide_Catcher = PlateLocSide * -1,
                   XCoord = c(cos(Angle * pi / 180) * ExitSpeed * ifelse(BatterSide == "Left", -1, 1)), 
                   YCoord = c(sin(Angle * pi / 180) * ExitSpeed), 
                   XLand = c(cos((90 - Bearing) * pi / 180) * Distance), 
                   YLand = c(sin((90 - Bearing) * pi / 180) * Distance)) %>%
    as.data.frame()
  
  for(contact.type in c("barrel", "solid", "F&B", "topped", "under")){
    data[which(!is.na(data$XCoord)), contact.type] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == contact.type),c("X", "Y")]), 
                                                                        as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  }
  names(data)[which(names(data) == "F&B")] <- "F.B"
  ####  null exit data for foul balls?
  data %<>% mutate(XCoord = XCoord * ifelse(BatterSide == "Left", -1, 1),
                   weak = as.numeric(ExitSpeed < 60),
                   InPlay = as.numeric(PitchCall == "InPlay" & PlayResult != "Undefined"), 
                   AB_End = c(PitchCall == "HitByPitch" | InPlay | KorBB %in% c("Strikeout", "Walk")),
                   ContactType = ifelse(as.logical(barrel), "Barrel", ifelse(as.logical(solid), "Solid", ifelse(as.logical(`F.B`), "F&B", ifelse(as.logical(weak), "Weak", ifelse(as.logical(topped), "Topped", ifelse(as.logical(under), "Under", NA_character_)))))),
                   Good_Contact = ifelse(is.na(ContactType), NA_integer_, as.numeric(ContactType %in% c("Barrel", "Solid", "F&B"))),
                   Bases = ifelse(PlayResult == "Single", 1, ifelse(PlayResult == "Double", 2, ifelse(PlayResult == "Triple", 3, ifelse(PlayResult == "HomeRun", 4, ifelse(!InPlay, NA_integer_, 0))))),
                   Hit = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))),
                   Reach = ifelse(!AB_End, NA_integer_, as.numeric(Hit | PitchCall == "HitByPitch" | KorBB == "Walk")),
                   X1B = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult == "Single")), 
                   X2B = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult == "Double")),
                   X3B = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult == "Triple")), 
                   HR = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult == "HomeRun")),
                   SH = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult == "Sacrifice" & HitType == "Bunt")),
                   Error = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult == "Error")), 
                   Out = ifelse(!InPlay, NA_integer_, as.numeric(PlayResult %in% c("Out", "Sacrifice", "FieldersChoice") & !SH)),
                   K = ifelse(AB_End, as.numeric(KorBB == "Strikeout"), NA_integer_), 
                   BB = ifelse(AB_End, as.numeric(KorBB == "Walk"), NA_integer_), 
                   HBP = ifelse(AB_End, as.numeric(PitchCall == "HitByPitch"), NA_integer_),
                   Count = paste(Balls, Strikes, sep = "-"),
                   Pitch_Result = ifelse(KorBB != "Undefined", KorBB, 
                                         ifelse(PitchCall == "InPlay", ifelse(PlayResult == "Undefined", "In Play (unknown result)", paste0(PlayResult, " (", HitType, ")")),
                                                gsub(gsub(gsub(PitchCall, pattern = "HitByPitch", replacement = "HBP"), pattern = "([A-Z][a-z]+)([A-Z][a-z]+)", replacement = "\\2 \\1"), pattern= "^Ball ", replacement = ""))), 
                   AB_Result = ifelse(AB_End, Pitch_Result, NA),
                   Hit_TM_Text = paste("Contact Type:", ContactType, "<br>Launch Angle:", round(Angle), "<br>Exit Velo:", round(ExitSpeed), "<br>Result:", PlayResult, 
                                       "<br>Date:", Date, "<br>Inning:", Inning, "<br>Count:", Count, "<br>Distance:", round(Distance)),
                   Pitch_TM_Text = paste("Result:", Pitch_Result, "<br>Pitch:", TaggedPitchType, "<br>Count:", Count, "<br>Date:", Date, "<br>Batter:", Batter, "<br>Inning:", Inning),
                   mvmt.x = (HorzBreak / 12) * ifelse(p.view, 1, -1), mvmt.z = (2.525 + InducedVertBreak / 12),
                   Hit_No_Hit = ifelse(data$PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), "Hit", ifelse(data$PlayResult == "Undefined", NA_character_, "No Hit")),
                   Tru3 = ifelse(KorBB != "Undefined", KorBB, ifelse(PlayResult == "HomeRun", "Home Run", ifelse(PitchCall == "InPlay", "In Play", NA_character_))),
                   Hard_Hit = ifelse(Good_Contact, "Hard Hit", "Not Hard Hit"), Reach = ifelse(Reach, "Reached Base", "Didn't Reach")
  ) %>%
    fill(AB_Result, .direction = "up")
    # The only flaw is ABs with no AB_End = TRUE (KorBB always undefined, PitchCall never InPlay or HitByPitch)
  data <- applybbexpoutcomes(data)
  data <- applybbtype(data)
  data <- apply.exp.called.strike(data)
  
  data$Scrimmage <- ifelse(startsWith(data$GameID, "1NTRSQ"), "Scrimmage", 
                           ifelse(startsWith(data$GameID, "L1V3BP"), "Live BP", "Season"))
  
  ## Times thru the order
  data$NewPitchofPA <- data$PitchofPA
  for(pitcher in unique(data$Pitcher)){
    data[which(data$Pitcher == pitcher), "Batters_Faced"] <- cumsum(as.numeric(data[which(data$Pitcher == pitcher), "AB_End"]))
  }
  for(batter in unique(data$Batter)){
    data[which(data$Batter == batter), "Times_In_The_Box"] <- c(1, head(1 + cumsum(as.numeric(data[which(data$Batter == batter), "AB_End"])), -1))
  }
  # data[which(data$Times_In_The_Box == 0), "Times_In_The_Box"] <- 1
  data[which(data$Batters_Faced == 0), "Batters_Faced"] <- 1
  
  data$Game <- paste(data$Date, ifelse(scrim, "Scrimmage", ifelse(livebp, "Live BP", opponent)), sep = " - ")
  
  data[which(data$Batter == "Caulfield, Tom"), "Batter"] <- "Caufield, Tom"
  
  # data[which(data$PitchCall %in% c("StrikeCalled", "StrikeSwinging") & data$Strikes == 2), "KorBB"] <- "Strikeout"
  # data[which(data$PitchCall %in% c("BallCalled", "BallIntentional") & data$Balls == 3), "KorBB"] <- "Walk"
  
  return(data)
}


# write.csv(game3.pred[which(game3.pred$PitchCall != "Undefined"),c("Batter", "Pitcher", "Inning", "Top.Bottom", "Balls", "Strikes", "Outs", "PitchCall", "AutoPitchType", "RelSpeed")], 
#           file = "Xavier at UNC Game 3 2.17.19.csv", row.names = F)

# c(1, head(1 + cumsum(AB_End), -1))

# write.csv(data, file = "data/uncall.csv", row.names = F)
# render.dashboard()
# render_pitcher_app()


