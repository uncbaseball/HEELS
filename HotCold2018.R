# ps <- dbGetQuery(tarconn, "SELECT DISTINCT(`Pitcher`) FROM `UNCall` WHERE RIGHT(`Date`, 1) = 8 AND `PitcherTeam` = 'NOR_TAR' AND `Pitcher` NOT IN('Bukauskas, J.B.', 'Gay, Trevor')")
# data <- dbGetQuery(tarconn, paste("SELECT * FROM `UNCall` WHERE RIGHT(`Date`, 1) = 8 AND `Pitcher` IN('", gsub(x = paste(ps[,1], collapse = "', '", sep = ""), pattern = "O'", replacement = "O''"), "')", sep = ""))


filter <- dplyr::filter
mutate <- dplyr::mutate
summarise <- dplyr::summarise
group_by <- dplyr::group_by


pitcher.hot.cold <- function(data, plot = F, save.as = "pdf", save.dir, save = F){
  
  ## data should be pitchtagged
  
  ## create a new variable ag from the variable data
  ag <- data %>%
    ## filter out pitches whose 1. PitchType is Undefined, 2. BatterSide is invalid (not Left or Right) or is 3. Location is more than 8 balls from down the middle (4.5 from the black) horizontally or more than 1.85 feet from the middle height of the zone (1 ft from the edges)
    filter(TaggedPitchType != "Undefined" & BatterSide %in% c("Left", "Right") & abs(PlateLocSide) < (8 * gap) & abs(PlateLocHeight - 2.55) < 1.85 & (Strikes < 2 | PitchCall != "FoulBall")) %>%
    ## add new columns: Horz and Vert give which row/column that pitch's ZONE is, PLH is added for convenience -- the distance from a middle height pitch, TwoStrikes (did the count have 2 strikes), regular.season (was it a scrimmage)
    mutate(Horz = ifelse(abs(PlateLocSide) < (2 * gap), 0, ifelse(abs(PlateLocSide) < (5 * gap), sign(PlateLocSide), 2 * sign(PlateLocSide))),
           PLH = PlateLocHeight - 2.55,
           Vert = ifelse(abs(PLH) < 0.45, 0, ifelse(abs(PLH) < 1.15, sign(PLH), sign(PLH) * 2)),
           TwoStrikes = as.numeric(Strikes == 2),
           reg.season = as.numeric(!is.element(BatterTeam, c("NOR_TAR", "NOR_TAR2")))) %>%
    ## group the data by pitcher, pitch type, batter hand, 2 strikes (or not), pitch location zone
    group_by(Pitcher, TaggedPitchType, BatterSide, TwoStrikes, Horz, Vert) %>%
    ## for each of those groups calculate the following metrics: strike percentage, extra base hits percentage, regular season games count 3x scrimmages
    summarise(StrikesPer = weighted.mean(x = as.numeric(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall")), w = 1 + 0.5 * reg.season),
              XBHPer = weighted.mean(x = as.numeric(PlayResult %in% c("Double", "Triple", "HomeRun")), w = 1 + 0.5 * reg.season), n = n()) %>%
    ## make the data a data.frame (not tibble)
    as.data.frame()
  
  
  ## Same as above but not grouping by locations just pitch type and 2 strikes
  pag <- data %>%
    filter(TaggedPitchType != "Undefined" & BatterSide %in% c("Left", "Right") & abs(PlateLocSide) < (8 * gap) & abs(PlateLocHeight - 2.55) < 1.85 & (Strikes < 2 | PitchCall != "FoulBall")) %>%
    mutate(TwoStrikes = as.numeric(Strikes == 2),
           reg.season = as.numeric(BatterTeam != "NOR_TAR")) %>%
    group_by(Pitcher, TaggedPitchType, BatterSide, TwoStrikes) %>%
    summarise(StrikesPer = weighted.mean(x = as.numeric(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall")), w = 1 + 0.5 * reg.season),
              XBHPer = weighted.mean(x = as.numeric(PlayResult %in% c("Double", "Triple", "HomeRun")), w = 1 + 0.5 * reg.season), n = n()) %>%
    as.data.frame()
  # regress to mean and coloring
  
  ## Combine the two groupings -- combine 
  ag <- merge(ag, pag, by = c("Pitcher", "BatterSide", "TaggedPitchType", "TwoStrikes"), suffixes = c(".Loc", ".All"), all = T)
  
  ag$Reg.StrikesPer.Loc <- ifelse(ag$n.Loc >= 15, ag$StrikesPer.Loc, ag$StrikesPer.Loc * ag$n.Loc / 15 + (15 - ag$n.Loc) / 15 * ag$StrikesPer.All)
  
  if(plot) plot.pitcher.hot.cold(ag, save.dir = save.dir, save.as = save.as, save = save)
  
  return(ag)
}

plot.pitcher.hot.cold <- function(ag, date.range = "", save.as = "pdf", filename = paste0(" Hot Cold Zones"), save.dir, save = F){
  colfun <- colorRampPalette(c("red", "coral1", "white", "darkslategray1", "blue"))
  colset <- colfun(20)
  save.fxn <- get(save.as)
  # pdf(file = paste0(tar.path, "/Weekly Reports/Hot Cold Zones ", ifelse(length(unique(ag$Pitcher)) == 1, unique(ag$Pitcher), ""), Sys.Date(), ".pdf"), onefile = T, width = 9.15, height = 6.85)
  for(p in unique(ag$Pitcher)){
    message(p)
    if(!missing(save.as) & save) save.fxn(paste0(save.dir, "/", gsub(paste0(p, filename, ".", save.as), pattern = " ", replacement = "_")), bg = "lightblue", width = 9.15, height = 6.85)
    pdata <- ag[which(ag$Pitcher == p),]
    p <- psplit(p)
    pdata <- cbind(pdata, Cols = color.by.value(values = pdata$StrikesPer.Loc, cols = colset, interval = unname(quantile(pdata$Reg.StrikesPer.Loc, probs = c(.95))) / 20, min.val = 0))
    #quartz(width = 9.15, height = 6.85)
    par(mfrow = c(2,length(unique(pdata$TaggedPitchType))), mar = c(.5, 0, 2, 0))
    for(bside in c("Left", "Right")){
      for(k2 in c(0, 1)){
        plot.pitch.zones(data = pdata[which(pdata$TaggedPitchType == "FB" & pdata$TwoStrikes == k2 & pdata$BatterSide == bside),], sidecol = "BatterSide", catcherview = F)
        if(!k2){
          title(main = paste("\n", p, "Strike % vs.", paste(substr(bside, 1, 1), "HB", sep = ""), "\n\nFB", ifelse(k2, "2 Strikes", "0/1 Strikes"), sep = " "))
        } else{
          title(main = paste("\n\n\nFB", ifelse(k2, "2 Strikes", "0/1 Strikes")))
        }
        plot.pitch.zones(data = pdata[which(pdata$TaggedPitchType == "CH" & pdata$TwoStrikes == k2 & pdata$BatterSide == bside),], sidecol = "BatterSide", catcherview = F)
        title(main = paste("\n\n\nCH", ifelse(k2, "2 Strikes", "0/1 Strikes")))
        for(pitch in unique(pdata[which(!is.element(pdata$TaggedPitchType, c("FB", "CH"))),"TaggedPitchType"])){
          plot.pitch.zones(data = pdata[which(pdata$TaggedPitchType == pitch & pdata$TwoStrikes == k2 & pdata$BatterSide == bside),], sidecol = "BatterSide", catcherview = F)
          title(main = paste("\n\n\n", ifelse(pitch == "CU", "CB", pitch), ifelse(k2, "2 Strikes", "0/1 Strikes")))
        }
      }
    }
    if(save) dev.off()
  }
  # dev.off()
}

# plot.pitcher.hot.cold(pitcher.hot.cold(data))

##################################      FUNCTIONS      ######################################

plot.pitch.zones <- function(data, sidecol = "BatterSide.y", var = "Reg.StrikesPer.Loc", catcherview = T){
  require(png)

  if(nrow(data) == 0){
    plot(0, 0, type = "n", axes = F, xlab = "", ylab = "")
    return()
  }
  
  bside <- mode(data[,sidecol])
  
  real.strikezones <- read.csv(file = "data/Real Strike Zone.csv", stringsAsFactors = F)
  
  plot(0, 0, type = "n", xlim = c(-3, 3), ylim = c(0, 6), axes = F, xlab = "", ylab = "")
  if(bside == "Left"){
    if(catcherview){
      lhb.pic <- readPNG("data/LHB Catcher View.png")
      rasterImage(lhb.pic, ifelse(catcherview, 1.5, -2.5), 0, ifelse(catcherview, 2.75, -1.75), 6)
      real.strikezones$PlateLocSide <- real.strikezones$PlateLocSide * -1
    } else{
      lhb.pic <- readPNG("data/Release Point Pics/LHB Pitcher View.png")
      rasterImage(lhb.pic, -2.75, 0, -1.5, 6)
    }
    
  } else if(bside == "Right"){
    if(catcherview){
      real.strikezones$PlateLocSide <- real.strikezones$PlateLocSide * -1
      rhb.pic <- readPNG("data/RHB Catcher View.png")
      rasterImage(rhb.pic, ifelse(catcherview, -2.75, 1.75), 0, ifelse(catcherview, -1.5, 2.4), 6)
    } else{
      rhb.pic <- readPNG("data/Release Point Pics/RHB Pitcher View.png")
      rasterImage(rhb.pic, 1.5, 0, 2.75, 6)
    }
  } else{
    stop("bside must be one of 'Left' 'Right'")
  }
  
  data$left <- ifelse(data$Horz == -2, -8 * gap, ifelse(data$Horz == -1, -5 * gap, ifelse(data$Horz == 0, -2 * gap, ifelse(data$Horz == 1, 2 * gap, 5 * gap))))
  data$right <- data$left + ifelse(data$Horz == 0, 4, 3) * gap
  data$bot <- ifelse(data$Vert < 0, 0.7 * (data$Vert + 3), ifelse(data$Vert > 0, 2.3 + 0.7 * data$Vert, 2.1))
  data$top <- data$bot + ifelse(data$Vert == 0, .9, .7)
  
  for(row in c(1:nrow(data))){
    rect(xleft = data[row, "left"], ybottom = data[row, "bot"], xright = data[row, "right"], ytop = data[row, "top"], border = "black", col = as.character(data[row, "Cols"]))
  }
  
  rsz <- real.strikezones[which(real.strikezones$BatterSide == bside),]
  points(rbind(rsz, rsz[1,]), type = "l", col = "green", lwd = 2)
  points(strikezone, type = "l", col = "yellow")
  
  for(row in c(1:nrow(data))){
    text(mean(as.vector(unlist(data[row,c("left", "right")]))), mean(as.vector(unlist(data[row,c("bot", "top")]))), paste0(gsub(formatC(data[row, var] * ifelse(var == "Reg.StrikesPer.Loc", 100, 1), format = "f", digits = ifelse(var == "RegSLG", 3, 0)), pattern = "0\\.", replacement = "."), ifelse(var == "Reg.StrikesPer.Loc", "%", "")), font = 2)
  }
  
}



###############################   HITTERS   ###################################

# batters <- dbGetQuery(tarconn, "SELECT DISTINCT(`Batter`) FROM `UNCall` WHERE `BatterTeam` = 'NOR_TAR' AND `PitchCall` != 'Undefined' AND RIGHT(`Date`, 1) = 8")
# data <- dbGetQuery(tarconn, paste("SELECT * FROM `UNCall` WHERE `BatterTeam` = 'NOR_TAR' AND RIGHT(`Date`, 1) = 8 OR (RIGHT(`Date`, 1) = 7 AND LEFT(`Date`, 2) IN('10', '11', '9/')) AND `Batter` IN('", paste(batters[,1], collapse = "', '"), "')", sep = ""))

batter.hot.cold <- function(data, switch = c("Semper, Earl", "Serretti, Danny"), plot = F, save = F, save.as = "pdf", save.dir){
  
  
  ## data should be autopitchtyped
  # data <- applybbexpoutcomes(data)
  
  undef.switch <- which(data$Batter %in% switch & data$BatterSide == "Undefined")
  if(length(undef.switch) > 0) data[undef.switch, "BatterSide"] <- ifelse(data[undef.switch, "PitcherThrows"] == "Left", "Right", ifelse(data[undef.switch, "PitcherThrows"] == "Right", "Left", "Undefined"))
  data[which(data$Batter %in% switch), "Batter"] <- paste0(data[which(data$Batter %in% switch), "Batter"], "_", substr(data[which(data$Batter %in% switch), "BatterSide"], 1, 1))
  
  data <- data[which(!is.element(data$Batter, switch) | data$BatterSide %in% c("Right", "Left")),]
  
  pag <- data %>%
    filter(PitchCall %in% c("InPlay") & abs(PlateLocSide) < (8 * gap) & abs(PlateLocHeight - 2.55) < 1.85) %>%
    mutate(TwoStrikes = as.numeric(Strikes == 2),
           reg.season = as.numeric(BatterTeam != "NOR_TAR"),
           Fastball = as.numeric(AutoPitchType == "FB")) %>%
    group_by(Batter, AutoPitchType, TwoStrikes) %>%
    summarise(BatterSide = mode(BatterSide), AvgSLG = weighted.mean(x = ExpSLG, w = 1 + 0.5 * reg.season, na.rm = T), n = n()) %>%
    as.data.frame()
  
  
  ag <- data %>%
    filter(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall") & abs(PlateLocSide) < (8 * gap) & abs(PlateLocHeight - 2.55) < 1.85) %>%
    #filter(PlayResult %in% c("Out", "Single", "Double", "Triple", "HomeRun") & abs(PlateLocSide) < (8 * gap) & abs(PlateLocHeight - 2.55) < 1.85) %>%
    mutate(Horz = ifelse(abs(PlateLocSide) < (2 * gap), 0, ifelse(abs(PlateLocSide) < (5 * gap), sign(PlateLocSide), 2 * sign(PlateLocSide))),
           PLH = PlateLocHeight - 2.55,
           Vert = ifelse(abs(PLH) < 0.45, 0, ifelse(abs(PLH) < 1.15, sign(PLH), sign(PLH) * 2)),
           TwoStrikes = as.numeric(Strikes == 2),
           reg.season = as.numeric(BatterTeam != "NOR_TAR"),
           Fastball = as.numeric(AutoPitchType == "FB")) %>%
    full_join(., pag, by = c("Batter", "AutoPitchType", "TwoStrikes")) %>%
    group_by(Batter, BatterSide.y, AutoPitchType, TwoStrikes, Horz, Vert, AvgSLG) %>%
    #summarise(BatterSide = mode(BatterSide), HitSLG = weighted.mean(x = as.numeric(PlayResult == "Single") + 2 * as.numeric(PlayResult == "Double") + 3 * as.numeric(PlayResult == "Triple") + 4 * as.numeric(PlayResult == "HomeRun"), w = 1 + 0.5 * reg.season, na.rm = T), n = n()) %>%
    summarise(ExpSLG = weighted.mean(x = ifelse(PitchCall %in% c("StrikeSwinging", "FoulBall"), 
                                                ifelse(as.logical(TwoStrikes), 0, AvgSLG - ifelse(PitchCall == "StrikeSwinging", .1, .05)), 
                                                ExpSLG), w = 1 + 0.5 * reg.season, na.rm = T), n = n()) %>%
    as.data.frame()
  
  ag[which(ag$Batter == "Brandenburg, Kip"), "BatterSide"] <- c("Right")
  
  # regress to mean and coloring
  
  
  #ag <- merge(ag, pag, by = c("Batter", "BatterSide", "AutoPitchType", "TwoStrikes"), suffixes = c(".Loc", ".All"), all = T)
  #ag[which(is.na(ag$ExpSLG.Loc)),"ExpSLG.Loc"] <- c(0.3)
  ag$RegSLG <- ifelse(ag$n >= 5, ag$ExpSLG, ag$ExpSLG * ag$n / 5 + (5 - ag$n) / 15 * ag$AvgSLG)
  
  
  if(plot) plot.batter.hot.cold(ag, save.dir = save.dir, save.as = save.as, save = save)
  
  return(ag)
}

plot.batter.hot.cold <- function(ag, date.range = "", save.as = "pdf", filename = paste0(" Hot Cold Zones"), save.dir, save = F){
  
  colfun <- colorRampPalette(c("blue", "darkslategray1", "white", "coral1", "red"))
  colset <- colfun(20)
   
  save.fxn <- get(save.as)
  
  # pdf(file = paste0(tar.path, "/Weekly Reports/Hot Cold Zones ", Sys.Date(), ".pdf"), onefile = T, width = 9.15, height = 6.85)
  for(p in unique(ag$Batter)){
    message(p)
    pdata <- ag[which(ag$Batter == p & !is.na(ag$AutoPitchType) & !is.na(ag$RegSLG)),]
    
    pdata$Horz <- pdata$Horz * -1
    if(!missing(save.as) & save) save.fxn(paste0(save.dir, "/", gsub(paste0(p, filename, ".", save.as), pattern = " ", replacement = "_")), bg = "lightblue", width = 9.15, height = 6.85)
    p <- psplit(p)
    pdata <- cbind(pdata, Cols = color.by.value(values = pdata$RegSLG, cols = colset, interval = unname(quantile(pdata$RegSLG, probs = c(.95), na.rm = T)) / 20, min.val = 0))
    #quartz(width = 9.15, height = 6.85)
    
    par(mfrow = c(2,3), mar = c(.5, 0, 2.5, 0))
    for(k2 in c(0, 1)){
      plot.pitch.zones(data = pdata[which(pdata$AutoPitchType == "FB" & pdata$TwoStrikes == k2),], var = "RegSLG")
      if(k2 == 0)
        title(main = paste("\n\n", p, "Slugging % (Bases Per AB)\n", date.range), ylab = paste(ifelse(k2, "Less Than", ""), "2 Strikes"), font.lab = 2)#\nvs. Fastball\n|\n\\/
       
      ## Add date range
      
      text(0.2, 5, paste("Fastballs with", ifelse(k2, "2", "0/1"), "Strikes"), font = 2, cex = 1.5, col = "lightblue")
      
      plot.pitch.zones(data = pdata[which(pdata$AutoPitchType == "CH" & pdata$TwoStrikes == k2),], var = "RegSLG")
      text(0.2, 5, paste("Changeups with", ifelse(k2, "2", "0/1"), "Strikes"), font = 2, cex = 1.5, col = "lightblue")
      
      plot.pitch.zones(data = pdata[which(pdata$AutoPitchType == "BB" & pdata$TwoStrikes == k2),], var = "RegSLG")
      text(0.2, 5, paste("Breaking Balls with", ifelse(k2, "2", "0/1"), "Strikes"), font = 2, cex = 1.5, col = "lightblue")
      
      
    }
    if(save) dev.off()
    #quartz.save(file = paste(p, "hot cold zones.pdf"), type = "pdf")
    #graphics.off()
  }
  
  # dev.off()  
}


## Explanation -- regression, colors, slugging %

## Show number of pitches seen, % swung at
  # could be 2, first colored by num seen, second by swing% (can regress colors by SS)



update.hot.cold <- function(){
  data <- read.csv(file = "data/uncall.csv", stringsAsFactors = F)
  # hs$Name[-c(1, 14, 18, 19)])
  batter.hot.cold(data %>% filter(Batter %in% c("Maniglia, Casey", "Elliott, Austin", "Casparius, Ben")), plot = T, save = T, save.dir = "/Users/micahdaley-harris/Desktop/TAR/micahdh.github.io/Hot_Cold_Zones/Hitter")
  
  pitcher.hot.cold(data %>% filter(Pitcher %in% ps$Name[-1]), plot = T, save = T, save.dir = "/Users/micahdaley-harris/Desktop/TAR/micahdh.github.io/Hot_Cold_Zones/Pitcher")
  
}





#################   WHERE ARE PITCHES THROWN    ###################

# lgdat <- dbGetQuery(tarconn, "SELECT * FROM `LgTm` WHERE `League` = 'ACC' AND RIGHT(`Date`, 1) >= 6")
# lgdat <- generic.pitch.tag(data = lgdat)
# 
# lgag <- lgdat %>%
#   filter(abs(PlateLocSide) < (8 * gap) & abs(PlateLocHeight - 2.55) < 1.85) %>%
#   mutate(Horz = ifelse(abs(PlateLocSide) < (2 * gap), 0, ifelse(abs(PlateLocSide) < (5 * gap), sign(PlateLocSide), 2 * sign(PlateLocSide))),
#          PLH = PlateLocHeight - 2.55,
#          Vert = ifelse(abs(PLH) < 0.45, 0, ifelse(abs(PLH) < 1.15, sign(PLH), sign(PLH) * 2)),
#          TwoStrikes = as.numeric(Strikes == 2)) %>%
#   group_by(AutoPitchType, Horz, Vert) %>%
#   #summarise(BatterSide = mode(BatterSide), HitSLG = weighted.mean(x = as.numeric(PlayResult == "Single") + 2 * as.numeric(PlayResult == "Double") + 3 * as.numeric(PlayResult == "Triple") + 4 * as.numeric(PlayResult == "HomeRun"), w = 1 + 0.5 * reg.season, na.rm = T), n = n()) %>%
#   summarise(LHB01k = length(which(BatterSide == "Left" & Strikes < 2)), LHB2k = length(which(BatterSide == "Left" & Strikes == 2)), 
#             RHB01k = length(which(BatterSide == "Right" & Strikes < 2)), RHB2k = length(which(BatterSide == "Right" & Strikes == 2))) %>%
#   as.data.frame()
#   
# for(col in c(4:7)){
#   lgag[,c(col)] <- round_any(100 * lgag[,c(col)] / sum(lgag[,c(col)]), f = round, accuracy = 0.5)
# }
# 
# 
# for(s in c("L", "R")){
#   quartz(width = 9.15, height = 6.85)
#   par(mfrow = c(2,3), mar = c(.5, 0, 2.5, 0))
#   sdata <- lgag[,c(1:3, which(startsWith(colnames(lgag), s)))]
#   sdata$Horz <- sdata$Horz * -1
#   plot.pitch.zones(var = paste0(s, "HB01k"), data = cbind(sdata[which(sdata$AutoPitchType == "FB"), ], BatterSide.y = ifelse(s == "L", "Left", "Right"), Cols = color.by.value(values = sdata[which(sdata$AutoPitchType == "FB"),paste0(s, "HB01k")], cols = colset, interval = unname(quantile(sdata[,paste0(s, "HB01k")], probs = c(.95), na.rm = T)) / 20, min.val = 0)))
#   title(main = paste0("% of FBs to ", s, "HB in the ACC\nLess than 2 Strikes --->"))
#   plot.pitch.zones(var = paste0(s, "HB01k"), data = cbind(sdata[which(sdata$AutoPitchType == "CH"),], BatterSide.y = ifelse(s == "L", "Left", "Right"), Cols = color.by.value(values = sdata[which(sdata$AutoPitchType == "CH"),paste0(s, "HB01k")], cols = colset, interval = unname(quantile(sdata[,paste0(s, "HB01k")], probs = c(.95), na.rm = T)) / 20, min.val = 0)))
#   title(main = "\n% of CHs\n|\n\\/")
#   plot.pitch.zones(var = paste0(s, "HB01k"), data = cbind(sdata[which(sdata$AutoPitchType == "BB"),], BatterSide.y = ifelse(s == "L", "Left", "Right"), Cols = color.by.value(values = sdata[which(sdata$AutoPitchType == "BB"),paste0(s, "HB01k")], cols = colset, interval = unname(quantile(sdata[,paste0(s, "HB01k")], probs = c(.95), na.rm = T)) / 20, min.val = 0)))
#   title(main = "\n% of BBs\n|\n\\/")
#   
#   plot.pitch.zones(var = paste0(s, "HB2k"), data = cbind(sdata[which(sdata$AutoPitchType == "FB"), ], BatterSide.y = ifelse(s == "L", "Left", "Right"), Cols = color.by.value(values = sdata[which(sdata$AutoPitchType == "FB"),paste0(s, "HB2k")], cols = colset, interval = unname(quantile(sdata[,paste0(s, "HB01k")], probs = c(.95), na.rm = T)) / 20, min.val = 0)))
#   title(main = paste0("2 Strikes --->"))
#   plot.pitch.zones(var = paste0(s, "HB2k"), data = cbind(sdata[which(sdata$AutoPitchType == "CH"),], BatterSide.y = ifelse(s == "L", "Left", "Right"), Cols = color.by.value(values = sdata[which(sdata$AutoPitchType == "CH"),paste0(s, "HB2k")], cols = colset, interval = unname(quantile(sdata[,paste0(s, "HB01k")], probs = c(.95), na.rm = T)) / 20, min.val = 0)))
#   plot.pitch.zones(var = paste0(s, "HB2k"), data = cbind(sdata[which(sdata$AutoPitchType == "BB"),], BatterSide.y = ifelse(s == "L", "Left", "Right"), Cols = color.by.value(values = sdata[which(sdata$AutoPitchType == "BB"),paste0(s, "HB2k")], cols = colset, interval = unname(quantile(sdata[,paste0(s, "HB01k")], probs = c(.95), na.rm = T)) / 20, min.val = 0)))
#   
#   quartz.save(file = paste0(s, "HB League Pitch Locations.pdf"), type = "pdf")
#   graphics.off()
# }
# 
# 
# 
# 
# 
# 
# 
