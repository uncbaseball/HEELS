####################    Generate BP Breakdown   ########################

bpbreakdown <- function(data, game = F, scout = F, unconly = T){
  
  require(nnet)
  require(mgcv)
  require(png)
  commasplit <- function(string){
    split <- strsplit(string, ", ", fixed = TRUE)
    
    return(paste(split[[1]][2], split[[1]][1]))
  }
  od <- getwd()
  coords <- read.csv(file = "data/Batter LAEV Visual Coords.csv", stringsAsFactors = F)
  
  boshcoords <- read.csv("data/Boshamer Outline Points.csv", stringsAsFactors = F)
  
  load("data/Batted Ball Models.RData")
  
  real.zone <- read.csv(file = "data/Real Strike Zone.csv", stringsAsFactors = F)
  
  lhb.pic <- readPNG("data/LHB Catcher View.png")
  rhb.pic <- readPNG("data/RHB Catcher View.png")  
  
  
  data <- slide.trackman.columns(data)
  
  data <- data[which(data$Batter != "Aker, Cole"),] # usually means warmup pitch

  ## This was to remove sac bunts but it removed topped balls too
  # if(!game){
  #   data <- data[which(is.na(data$Distance) | is.na(data$ExitSpeed) | data$Distance >= 25 | data$ExitSpeed >= 70),]
  # }
  
  data <- data[order(data$Time),]
  data <- cbind(data, XCoord = c(cos(data$Angle * pi / 180) * data$ExitSpeed), YCoord = c(sin(data$Angle * pi / 180) * data$ExitSpeed), 
                XLand = c(cos((90 - data$Bearing) * pi / 180) * data$Distance), YLand = c(sin((90 - data$Bearing) * pi / 180) * data$Distance))
  data[which(!is.na(data$XLand))[which(!in.out(as.matrix(boshcoords), as.matrix(data[which(!is.na(data$XLand)),c("XLand", "YLand")])) & data[which(!is.na(data$XLand)), "Distance"] > 320)],"PlayResult"] <- c("HomeRun")
  data <- cbind(data, Barrel = c(0), Solid = c(0), FnB = c(0), Weak = c(0), Topped = c(0), Under = c(0))
  data[which(!is.na(data$XCoord)),"Barrel"] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == "barrel"),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  data[which(!is.na(data$XCoord)),"Solid"] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == "solid"),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  data[which(!is.na(data$XCoord)),"FnB"] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == "F&B"),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  data[which(!is.na(data$XCoord)),"Weak"] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == "weak"),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  data[which(!is.na(data$XCoord)),"Topped"] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == "topped"),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  data[which(!is.na(data$XCoord)),"Under"] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == "under"),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
  data <- cbind(data, Type = ifelse(as.logical(data$Barrel), "Barrel", ifelse(as.logical(data$Solid), "Solid", ifelse(as.logical(data$FnB), "F&B", ifelse(as.logical(data$Topped), "Topped", ifelse(as.logical(data$Under), "Under", ifelse(as.logical(data$Weak), "Weak", "None")))))), 
                LAEVcol = ifelse(as.logical(data$Barrel), "red", ifelse(as.logical(data$Solid), "lightpink", ifelse(as.logical(data$FnB), "coral1", 
                            ifelse(as.logical(data$Under), "lightskyblue", ifelse(as.logical(data$Weak), "palegoldenrod", ifelse(as.logical(data$Topped), "darkolivegreen3", "black")))))),
                  PAnum = c(0), PApch = c(16), PitchResCol = ifelse(data$PitchCall == "InPlay", "blue", 
                                                                    ifelse(is.element(data$PitchCall, c("BallCalled", "BallIntentional")), "forestgreen",  
                                                                           ifelse(data$PitchCall == "StrikeCalled", "red", 
                                                                                  ifelse(is.element(data$PitchCall, c("StrikeSwinging", "FoulBall")), "darkred", "black")))), stringsAsFactors = F)
  
  if(game){
    fulldata <- data[which(data$PitchCall != "Undefined"),]
    ag <- aggregate(fulldata$PitchofPA, list(fulldata$Inning, fulldata$Top.Bottom, fulldata$PAofInning), min)
    for(row in which(ag$x > 1)){
      fulldata[which(fulldata$Inning == ag[row, 1] & fulldata$Top.Bottom == ag[row, 2] & fulldata$PAofInning == ag[row,3]),"PitchofPA"] <- c(fulldata[which(fulldata$Inning == ag[row, 1] & fulldata$Top.Bottom == ag[row, 2] & fulldata$PAofInning == ag[row,3]),"PitchofPA"] - ag[row,4] + 1)
    }
  } else{
    fulldata <- data
  }
  
  pic <- readPNG("data/Bosh Overhead Cropped on Home Plate.png")
  limage <- readPNG("data/LHB LAEV Visual.png")
  rimage <- readPNG("data/RHB LAEV Visual.png")
  strikezone <- data.frame(x = c(-0.7083, -0.7083, 0.7083, 0.7083, -0.7083), z = c(1.75, 3.42, 3.42, 1.75, 1.75))
  
  batters <- vector.ifelse(tests = unconly, yeses = list(unique(fulldata[which(is.element(fulldata$BatterTeam, c("NOR_TAR", "NOR_TAR2", "Undefined"))),"Batter"])), if.none = unique(fulldata$Batter))
  batters <- batters[which(batters != "")]
  #quartz(width = 8.5, height = 12)
  # pdf(file = paste0("/users/micahdaley-harris/desktop/tar/batter/batter reports/", ifelse(game, " Game Breakdown", " BP Breakdown"), gsub("/", ".", unique(data$Date)[1]), ".pdf"), onefile = T, width = 8.5, height = 12)
  for(batter in sort(batters)){
    
    print(batter)
    data <- fulldata[which(fulldata$Batter == batter),]
    hitdata <- data[which(!is.na(data$Angle)),]
    if(game){
      abnum <- 1
      abshapes <- c(15, 16, 17, 18, 1, 8, 11, 4)
      for(ab in unique(paste(data$Inning, data$PAofInning))){
        data[which(paste(data$Inning, data$PAofInning) == ab),"PAnum"] <- abnum
        data[which(paste(data$Inning, data$PAofInning) == ab),"PApch"] <- abshapes[abnum]
        abnum <- abnum + 1
      }
      hitdata <- data[which(data$PitchCall == "InPlay" & !is.na(data$Angle)),]
    }
    
    
    #quartz(width = 8.5, height = 12)
    layout(matrix(c(1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4), 2, 12, byrow = TRUE), heights = c(0.75, 1.25))
    ## Summary ##

    if(length(which(hitdata$PlayResult != "HomeRun")) > 0){
      scaleddata <- cbind(hitdata[which(hitdata$PlayResult != "HomeRun"),], BatsL = c(as.numeric(batter %in% c("Riley, Brandon", "McGee, Ashton", "Busch, Michael", "Inclan, Clemente", "Hesterlee, Jackson", "Zarate, Angel"))))
      scaleddata[which(!is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "Bearing")] <- as.data.frame(scale(scaleddata[which(!is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "Bearing")], center = scale.bear[c("Angle", "ExitSpeed", "Bearing"), "center"], scale = scale.bear[c("Angle", "ExitSpeed", "Bearing"), "scale"]))
      scaleddata[which(is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "BatsL")] <- as.data.frame(scale(scaleddata[which(is.na(scaleddata$Bearing)), c("Angle", "ExitSpeed", "BatsL")], center = scale.laev[c("Angle", "ExitSpeed", "BatsL"), "center"], scale = scale.laev[c("Angle", "ExitSpeed", "BatsL"), "scale"]))
    
    if(batter %in% c("Riley, Brandon", "McGee, Ashton", "Busch, Michael", "Inclan, Clemente", "Hesterlee, Jackson", "Zarate, Angel")){
      fulloutcomes <- rbind(predict(lhb.bb.nnet, scaleddata[which(!is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "Bearing")]), predict(laev.bb.nnet, scaleddata[which(is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "BatsL")]))
    } else{
      fulloutcomes <- rbind(predict(rhb.bb.nnet, scaleddata[which(!is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "Bearing")]), predict(laev.bb.nnet, scaleddata[which(is.na(scaleddata$Bearing)),c("Angle", "ExitSpeed", "BatsL")]))
    }
    
    fulloutcomes <- as.data.frame(fulloutcomes)
    
    fulloutcomes[,"Double"] <- c(rowSums(fulloutcomes[,c("Double", "Triple")]))
    fulloutcomes <- fulloutcomes[,-c(which(colnames(fulloutcomes) == "Triple"))]
    
    outcomesums <- colSums(fulloutcomes)
    outcomes <- c(rep.int("2B", floor(outcomesums["Double"])), rep.int("Out", floor(outcomesums["Out"])), rep.int("1B", floor(outcomesums["Single"])), rep.int("HR", floor(outcomesums["HomeRun"])))
    outcomesums <- outcomesums - floor(outcomesums)
    names(outcomesums) <- c("2B", "HR", "Out", "1B")
    outcomesums <- outcomesums[order(outcomesums, decreasing = T)]
    
    if(sum(outcomesums) >= 1){
      outcomes <- c(outcomes, names(outcomesums)[c(1:sum(outcomesums))])
    } else{
      outcomes <- c(outcomes, names(outcomesums)[which.max(outcomesums)])
    }
    
    } else{
      scaleddata <- data.frame()
      outcomes <- c()
    }
    
    outcomes <- c(outcomes, rep.int("HR", length(which(hitdata$PlayResult == "HomeRun"))))
    #print(outcomes)
    # outcomes <- c(sample(c("1B", "2B", "HR", "Out"), length(which(hitdata$Type == "Barrel" & hitdata$PlayResult != "HomeRun")), prob = c(0, 5.7, 68.2, 26.1), replace = T),
    #               sample(c("1B", "2B", "HR", "Out"), length(which(hitdata$Type == "Solid" & hitdata$PlayResult != "HomeRun")), prob = c(0.4, 24.4, 34.5, 40.7), replace = T),
    #               sample(c("1B", "2B", "HR", "Out"), length(which(hitdata$Type == "F&B" & hitdata$PlayResult != "HomeRun")), prob = c(56.7, 13.9, 0.1, 29.3), replace = T),
    #               sample(c("1B", "2B", "HR", "Out"), length(which(hitdata$Type == "Topped" & hitdata$PlayResult != "HomeRun")), prob = c(24.2, 1.7, 0, 74.1), replace = T),
    #               sample(c("1B", "2B", "HR", "Out"), length(which(hitdata$Type == "Under" & hitdata$PlayResult != "HomeRun")), prob = c(11.4, 6.4, 1.8, 80.4), replace = T),
    #               sample(c("1B", "2B", "HR", "Out"), length(which(hitdata$Type == "Weak" & hitdata$PlayResult != "HomeRun")), prob = c(17.4, 0, 0, 82.6), replace = T), 
    #               rep.int("HR", length(which(hitdata$PlayResult == "HomeRun"))))

    plot(0, 0, type = "n", axes = F, xlab = "", ylab = "")
    title(main = commasplit(batter), cex.main = 2)
    
    ba <- length(which(is.element(outcomes, c("1B", "2B", "HR")))) / length(outcomes)
    slg <- (length(which(outcomes == "1B")) + length(which(outcomes == "2B")) * 2 + length(which(outcomes == "HR")) * 4) / length(outcomes)
    text(0, 1, paste("2OPS:", formatC((ba * 1.7 + slg) / 3, digits = 3, format = "f")), cex = 2, col = "red")
    text(0, 0.625, "2OPS combines power (SLG%) and\nreaching base (OBP%) into 1 number\n\nThe numbers are like OBP\nso a .400 2OPS is as good as a .400 OBP\nso think about it like that", cex = 0.85, col = "purple")
    text(0, 0.25, paste("BA/SLG: ", formatC(ba, digits = 3, format = "f"), "/", formatC(slg, digits = 3, format = "f"), sep = ""), cex = 1.5, col = "blue")
    text(0, 0, paste("EXP. RESULTS\n", "1B:", length(which(outcomes == "1B")), "2B:", length(which(outcomes == "2B")), "HR:", length(which(outcomes == "HR")), "Out:", length(which(outcomes == "Out"))), cex = 1.1)
         
    ## Field Viz
    plot(x = 0, y = 0, type = "n", xlim = c(-249, 249), ylim = c(0, 456.5), axes = FALSE, xlab = "", ylab = "")
    title(main = unique(data$Date)[1], cex.main = 2)
    rasterImage(pic, xleft = -249, xright = 249, ybottom = 0, ytop = 456.5)
    points(hitdata$XLand, hitdata$YLand, pch = 16, cex = 2.3, col = hitdata$LAEVcol)
    if(length(which(data$Distance >= 340)) > 0){text(data[which(data$Distance >= 340),"XLand"], data[which(data$Distance >= 340),"YLand"], round(data[which(data$Distance >= 340),"Distance"]), pos = ifelse(data[which(data$Distance >= 340),"Bearing"] < 35, 4, 2), col = "red")}
    #if(length(which(data$PlayResult == "HomeRun")) > 0){text(data[which(data$PlayResult == "HomeRun"),"XLand"], data[which(data$PlayResult == "HomeRun"),"YLand"], "*", pos = 3, col = "green")}
    if(game){if(nrow(hitdata) > 0){text(hitdata$XLand, hitdata$YLand, hitdata$PAnum, col = "black")}} else{text(hitdata$XLand, hitdata$YLand, which(!is.na(data$Angle)), col = "black")}
    

    ## LA EV Viz
    
    batterpic <- tryCatch({readPNG(paste0("data/Batter Contact Pics/", commasplit(batter), ".png"))}, error = function(error){return(NULL)})
    prepic <- tryCatch({readPNG(paste0("data/Batter Contact Pics/", commasplit(batter), " Pre Swing.png"))}, error = function(error){return(NULL)})
    if(is.element(batter, c("Riley, Brandon", "McGee, Ashton", "Busch, Michael", "Inclan, Clemente", "Hesterlee, Jackson", "Zarate, Angel", "Miller, Brian", "Lynn, Tyler"))){
      lefty <- T
      plot(0, 0, type = "n", xlim = c(-115, 90), ylim = c(-115, 115), asp=1, axes=F,xlab="",ylab="")
      rasterImage(limage, -115,-115,0,115)
      if(!is.null(batterpic)){
        rasterImage(batterpic, 2.3, -46, 110, 52.15)
      }
      
      ind <- -1
      ind2 <- 0.5
      text(-42 * ind, -70, labels = "Barrel: 68% HR / 26% Out / 6% 2B/3B", cex = 0.6, col = "red")
      text(-42 * ind, -79, "Solid: 40% Out / 35% HR / 25% 2B/3B", col = "deeppink", cex = 0.6)
      text(-51 * ind, -88, "Flares & Burners: 57% 1B / 29% Out / 14% 2B/3B", col = "coral1", cex = 0.6)
      text(-43 * ind, -97, "Topped: 74% Out / 24% 1B / 2% 2B/3B", col = "forestgreen", cex = 0.6)
      text(-49 * ind, -106, "Under: 80% Out / 11% 1B / 7% 2B/3B / 2% HR", col = "dodgerblue3", cex = 0.6)
      text(-32 * ind, -115, "Weak: 82% Out / 18% 1B", col = "goldenrod3", cex = 0.6)
      rect(-94 * ind, -120, -8 * ind, -65)
      text(-92, 60, "2Bs & HRs", cex = 1.6, srt = 55, col = "white")
      text(-40, 75, "POP OUTS\n&\nFLY OUTS", col = "black", cex = 1.9)
      text(-69, -33, "ROLLOVERS/GROUNDOUTS", srt = -25, cex = 1.7)
      #text(70, 24, "WEAK\nLINE DRIVES", srt = -35, cex = 0.8, col = "cyan")
      text(-90, 18.5, "SINGLES", cex = 1.7, col = "black")
      
      arrows(x0 = -7.5, y0 = 89, y1 = 113, col = "red")
      text(10, 109, "Exit Velo\n90+ mph", col = "red")
      
      arrows(x0 = -16, y0 = 97, y1 = 113, col = "white")
      text(-30, 102, "Exit Velo\n100+ mph", col = "white", cex = 0.9)
      
      points(c(seq(0, -100, by = -20), -115), rep(0, 7), pch = "l", type = "o", col = "black")
      text(c(seq(0, -100, by = -20), -115) - 6, rep(-1, 7), paste(c(seq(0, 100, by = 20), 115), "\nmph"), col = c(rep("white", 6), "black"))
      
      text(20, 20, "Launch\nAngle", col = "green", cex = 1.4)
      arrows(21, 20, -20, col = "green")
      
      text(30, 2, "Exit\nVelo", col = "black", cex = 1.4)
      arrows(20, 2, 0, col = "black")

      
    } else{
      lefty <- F
      plot(0, 0, type = "n", xlim = c(-90, 115), ylim = c(-115, 115), asp=1, axes=F,xlab="",ylab="")
      rasterImage(rimage, 0,-115,115,115)
      if(!is.null(batterpic)){
        rasterImage(batterpic, -105, -46, -2.3, 52.15)
      }
      ind <- 1
      ind2 <- 0
      text(-60, -70, labels = "Barrel: 68% HR / 26% Out / 6% 2B/3B", cex = 0.6, col = "red")
      text(-60, -79, "Solid: 40% Out / 35% HR / 25% 2B/3B", col = "deeppink", cex = 0.6)
      text(-51, -88, "Flares & Burners: 57% 1B / 29% Out / 14% 2B/3B", col = "coral1", cex = 0.6)
      text(-60, -97, "Topped: 74% Out / 24% 1B / 2% 2B/3B", col = "forestgreen", cex = 0.6)
      text(-54, -106, "Under: 80% Out / 11% 1B / 7% 2B/3B / 2% HR", col = "dodgerblue3", cex = 0.6)
      text(-70.5, -115, "Weak: 82% Out / 18% 1B", col = "goldenrod3", cex = 0.6)
      rect(-94, -120, -8, -65)
      text(92, 60, "2Bs & HRs", cex = 1.6, srt = -55, col = "white")
      text(40, 75, "POP OUTS\n&\nFLY OUTS", col = "black", cex = 1.9)
      text(69, -33, "ROLLOVERS/GROUNDOUTS", srt = 25, cex = 1.7)
      #text(70, 24, "WEAK\nLINE DRIVES", srt = -35, cex = 0.8, col = "cyan")
      text(90, 18.5, "SINGLES", cex = 1.7, col = "black")
      
      arrows(x0 = 7.5, y0 = 89, y1 = 113, col = "red")
      text(-10, 109, "Exit Velo\n90+ mph", col = "red")
      
      arrows(x0 = 16, y0 = 97, y1 = 113, col = "white")
      text(30, 102, "Exit Velo\n100+ mph", col = "white", cex = 0.9)
      
      points(c(seq(0, 100, by = 20), 115), rep(0, 7), pch = "l", type = "o", col = "black")
      text(c(seq(0, 100, by = 20), 115) + 6, rep(1, 7), paste(c(seq(0, 100, by = 20), 115), "\nmph"), col = c(rep("white", 6), "black"))
      
      text(-20, 20, "Launch\nAngle", col = "green", cex = 1.4)
      arrows(-10, 20, 21, col = "green")
      
      text(-30, 2, "Exit\nVelo", col = "black", cex = 1.4)
      arrows(-20, 2, 0, col = "black")
      
    }
    
    points(hitdata$XCoord * ind, hitdata$YCoord, pch=19, cex = 2.3, col = "black")
    
    text(102 * ind + ind2, 81, labels = paste(length(which(hitdata$Type == "Barrel")), " Barrels (", round(100 * length(which(hitdata$Type == "Barrel")) / nrow(hitdata)), "%)", sep = ""), col = "red")
    text(96 * ind + ind2, 93, labels = paste(length(which(hitdata$Type == "Solid")), " Solid Contact (", round(100 * length(which(hitdata$Type == "Solid")) / nrow(hitdata)), "%)", sep = ""), col = "deeppink")
    text(64 * ind, 27, labels = paste(length(which(hitdata$Type == "F&B")), " Flares &\nBurners (", round(100 * length(which(hitdata$Type == "F&B")) / nrow(hitdata)), "%)", sep = ""), col = "black")
    text(26 * ind, -68, labels = paste(length(which(hitdata$Type == "Topped")), " Topped (", round(100 * length(which(hitdata$Type == "Topped")) / nrow(hitdata)), "%)", sep = ""), col = "darkgreen")
    text(22 * ind, 99, labels = paste(length(which(hitdata$Type == "Under")), " Under (", round(100 * length(which(hitdata$Type == "Under")) / nrow(hitdata)), "%)", sep = ""), col = "blue")
    text(19 * ind, -10, labels = paste(length(which(hitdata$Type == "Weak")), " Weak (", round(100 * length(which(hitdata$Type == "Weak")) / nrow(hitdata)), "%)", sep = ""), col = "goldenrod3")
    if(game){if(nrow(hitdata) > 0){text(hitdata$XCoord * ind, hitdata$YCoord, hitdata$PAnum, col = c("orange", "white", "green"))}} else{text(hitdata$XCoord * ind, hitdata$YCoord, which(!is.na(data$Angle)), col = c("orange", "white", "green"))}
    
    ## Strikezone Viz
    
    plot(strikezone$x, strikezone$z, type = "l", xlim = c(ifelse(game, -3, -2), ifelse(game, 3, 2)), ylim = c(ifelse(game, 0, 1), ifelse(game, 9, 5)), axes = F, col = "red", main = "", xlab = "", ylab = "")
    
    axis(side = 1, labels = c("", ""), at = c(-1, 1) * ifelse(game, 3, 2))
    
    gap <- 17 / 12 / 7
    
    
    if(lefty | batter == "Semper, Earl"){
      text(seq(-gap * 6, gap * 6, by = gap), rep(1.45, 13), c(3:1, 7:1, 1:3), col = c("purple", "green", "hotpink", "blue"))
      if(!is.null(prepic)){
        rasterImage(prepic, ifelse(game, 1.5, 1), ifelse(game, 0, 1), ifelse(game, 2.75, 2), ifelse(game, 6, 4))
      } else if(!is.null(batterpic)){
        rasterImage(batterpic, ifelse(game, 1.5, 1), ifelse(game, 0, 1), ifelse(game, 2.75, 2), ifelse(game, 6, 4))
      }
    } else{
      text(seq(-gap * 6, gap * 6, by = gap), rep(1.45, 13), c(3:1, 1:7, 1:3), col = c("purple", "green", "hotpink", "blue"))
      if(!is.null(prepic)){
        rasterImage(prepic, ifelse(game, -2.75, -2), ifelse(game, 0, 1), ifelse(game, -1.5, -1), ifelse(game, 6, 4))
      } else if(!is.null(batterpic)){
        rasterImage(batterpic, ifelse(game, -2.75, -2), ifelse(game, 0, 1), ifelse(game, -1.5, -1), ifelse(game, 6, 4))
      }
    }
    
    text(-2.5, 1.45, "Balls off:")
    
    segments(x0 = seq(-gap * 7, gap * 5, by = gap) + gap / 2, x1 = seq(-gap * 7, gap * 5, by = gap) + gap / 2, y0 = 1.3, y1 = 4, lty = 2)
    rz <- real.zone[which(real.zone$BatterSide == ifelse(lefty, "Left", "Right")),]
    points(rz[c(1:nrow(rz), 1),1] * -1, rz[c(1:nrow(rz), 1), 2], type = "l", lwd = 1.3, col = "hotpink")
    
    if(game){
      points(-1 * data$PlateLocSide, data$PlateLocHeight, pch = data$PApch, 
             col = data$PitchResCol, cex = 2.4)
      if(scout){
        ps <- aggregate(data$Pitcher, list(data$PAnum), mode)
        legend("topright", legend = paste("PA", ps[,1], "-", ps[,2], sep = " "), col = "black", pch = abshapes[c(1:(abnum - 1))])
      } else{
        legend("topright", legend = c("PA 1", "PA 2", "PA 3", "PA 4", "PA 5", "PA 6", "PA 7", "PA 8")[c(1:(abnum - 1))], col = "black", pch = abshapes[c(1:(abnum - 1))])
      }
      
      legend("topleft", legend = c("In Play", "Ball", "Called Strike", "Swinging Strike/Foul Ball"), pch = 16, col = c("blue", "forestgreen", "red", "darkred"), cex = ifelse(scout, 1, 1.4))
      text(data$PlateLocSide * -1, data$PlateLocHeight, data$PitchofPA, col = "orange", cex = 0.9)
    }else{
      legend("topleft", legend = "Pitch Taken/Fouled/Trackman missed it", pch = 1, col = "green")
      points(-1 * hitdata$PlateLocSide, hitdata$PlateLocHeight, pch = hitdata$PApch, col = hitdata$LAEVcol, cex = 2.4)
      text(hitdata$PlateLocSide * -1, hitdata$PlateLocHeight, which(!is.na(data$Angle)), col = "black", cex = 0.9)
      points(-1 * data[which(is.na(data$Angle)),"PlateLocSide"], data[which(is.na(data$Angle)),"PlateLocHeight"], col = "green", cex = 1)
      
    }
    
    Sys.sleep(4.5)
    # setwd(paste(substr(getwd(), 1, gregexpr("/", getwd(), fixed = TRUE)[[1]][3]), "Desktop/TAR/Batter/Batter Reports", sep = ""))
    #quartz.save(file = paste(batter, ifelse(game, " Game Breakdown", " BP Breakdown"), gsub("/", ".", unique(data$Date)[1]), ".pdf", sep = ""), type = "pdf")

  }
  dev.off()
  graphics.off()
  setwd(od)
}

medlaev <- function(data, last15 = T, csv = F){
  
  load("data/Batted Ball Models.RData")
  
  if(grepl("/", data[1,1])){
    data[,c(2:ncol(data))] <- data[,c(1:(ncol(data) - 1))]
    data[,1] <- c(1:nrow(data))
  }
  
  data <- data[order(data$Time, decreasing = T),]
  
  data <- data[which(abs(data$Bearing) <= 45),]
  
  data <- cbind(data, LBearing = c(0))
  
  df <- data.frame()
  
  for(batter in unique(data[which(data$Batter != ""),"Batter"])){
    
    bdat <- data[which(data$Batter == batter),]
    
    if(batter %in% c("Riley, Brandon", "McGee, Ashton", "Busch, Michael", "Inclan, Clemente", "Hesterlee, Jackson", "Zarate, Angel")){
      bdat$LBearing <- bdat$Bearing
    }
    
    if(last15){
      bdat <- bdat[c(1:min(15, nrow(bdat))),]
    }
    
    df <- rbind(df, data.frame(Batter = batter, ExitVelo = round(median(bdat$ExitSpeed, na.rm = T), digits = 1), LaunchAngle = round(median(bdat$Angle, na.rm = T), digits = 1), "2OPS" = round(mean((1.7 * unname(predict(obpmod, bdat)) + unname(predict(slgmod, bdat))) / 3, na.rm = T), digits = 3)))
    
  }
  
  names(df)[4] <- "2OPS"
  
  if(csv){
    od <- getwd()
    setwd(paste(substr(getwd(), 1, gregexpr("/", getwd(), fixed = TRUE)[[1]][3]), "Desktop/TAR/Batter/Batter Reports", sep = ""))
    write.csv(df, file = paste("Avg EV-LA-2OPS ", gsub("/", ".", unique(data$Date)[1]), ".csv", sep = ""), row.names = F)
  }
  
  return(df)
  
}


# boxplot(as.formula("Angle ~ Batter"), data = rbind(lib1, lib2), horizontal = T, ylab = "")
# daxis(side = 3, at = seq(par("usr")[4], par("usr")[2], length.out = length(x)), labels = x, cex = 0.5)
# x <- sort(unique(rbind(lib1, lib2)$Batter))
# x <- x[which(x != "")]
# text(par("usr")[1] + .2 * diff(par("usr")[c(1,3)]), seq(par("usr")[2], par("usr")[4], length.out = length(x)), labels = x)
# 
# 
# 
# data <- libbp
# ggplot()



spraychart <- function(data, plot.title = "", color = "PlayResult", shape = "circle-dot", plotly = T, basegraphics = F){
  
  if(is.data.frame(data) && any(!is.element(c("XLand", "YLand"), colnames(data)))) data <- cbind(data, XLand = c(cos((90 - data$Bearing) * pi / 180) * data$Distance), YLand = c(sin((90 - data$Bearing) * pi / 180) * data$Distance))
  # if(!is.element("key", colnames(data))) data$key <- as.character(row.names(data))
  
  if(is.data.frame(data)) data <- data[which(data$PitchCall == "InPlay" & data$PlayResult != "Undefined"),]
  
  result.order <- c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "Undefined")
  
  #data[order(match(data$PlayResult, result.order)),]
  # gg <- ggplot(data, aes(XLand, YLand, label = ifelse(Distance >= 340, round(Distance), ""), # took out key = key
  #                  text = paste("Date:", Date,
  #                               "<br>Inning:", Inning,
  #                               "<br>Count:", paste(Balls, Strikes, sep = "-"),
  #                               "<br>Result:", PlayResult,
  #                               "<br>Launch Angle:", round(Angle),
  #                               "<br>Exit Velo:", round(ExitSpeed),
  #                               "<br>Distance:", round(Distance)))) +
  #   if(!missing(shape)){geom_point(aes(color = get(color), shape = get(shape)), name = color)} +
  #   if(missing(shape)){geom_point(aes(color = get(color)), name = paste(color, shape))} +
  #   
  #   scale_x_continuous(limits = c(-249, 249), labels = NULL, breaks = c(-300, 300)) +
  #   scale_y_continuous(limits = c(0, 456.5), labels = NULL, breaks = c(-10, 500)) +
  #   geom_label(color = "red") +
  #   labs(title = plot.title, x = "", y = "", color = paste(color, shape, sep = ", "), shape = "") 
  # 
  # g <- ggplotly(gg, tooltip = "text", source = 'spraychart') %>%
  #   layout(xaxis = list(range = c(-249, 249), visible = F), yaxis = list(range = c(0, 456.5), visible = F),
  #     images = list(
  #     source = raster2uri(pic),
  #     xref = "x", yref = "y", x = -249, y = 0, sizex = 249 * 2, sizey = 456.5, 
  #     sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below"
  #   ), dragmode = "lasso")
  
  if(plotly){
    g <-  plot_ly(data = data, x = ~XLand, y = ~YLand, type = "scatter", color = ~get(color), #hoverinfo = "none", shape = ifelse(shape == "circle-dot", shape, ~get(shape)),
              text = ~Hit_TM_Text) %>%
        layout(xaxis = list(range = c(-249, 249), visible = F), yaxis = list(range = c(0, 456.5), visible = F),
               images = list(
                 source = raster2uri(readPNG("data/Bosh Overhead Cropped on Home Plate.png")),
                 xref = "x", yref = "y", x = -249, y = 0, sizex = 249 * 2, sizey = 456.5, 
                 sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below"
               ), dragmode = "lasso")
  return(g %>% highlight("plotly_selected"))
  }
      
  
  # g <- plot_ly(x = ~c(1:10), y = ~c(10:1))

  # return(htmlwidgets::onRender(g, "
  #                                function(el, x) {
  #                                 el.on('plotly_click', function(data) {
  #                                 alert('Closest point clicked:\n\n');
  #                                 }
  #                                }
  #                                "))# %>%


# for(var i=0; i < data.points.length; i++){
#   var pts = '';
#   pts = 'x = '+data.points[i].x +'\ny = '+
#     data.points[i].y.toPrecision(4) + '\n\n';
# }
# alert('Closest point clicked:\n\n'+pts);

  if(basegraphics){
    pic <- readPNG("data/Bosh Overhead Cropped on Home Plate.png")
    plot(x = 0, y = 0, type = "n", xlim = c(-249, 249), ylim = c(0, 456.5), axes = FALSE, xlab = "", ylab = "")
    title(main = plot.title, cex.main = 2)
    rasterImage(pic, xleft = -249, xright = 249, ybottom = 0, ytop = 456.5)
    points(data$XLand, data$YLand, pch = 16, cex = 2.3, col = data$LAEVcol)
  }
  # if(length(which(data$Distance >= 340)) > 0)text(data[which(data$Distance >= 340),"XLand"], data[which(data$Distance >= 340),"YLand"], round(data[which(data$Distance >= 340),"Distance"]), pos = ifelse(data[which(data$Distance >= 340),"Bearing"] < 35, 4, 2), col = "red")
  # if(game){if(nrow(hitdata) > 0){text(hitdata$XLand, hitdata$YLand, hitdata$PAnum, col = "black")}} else{text(hitdata$XLand, hitdata$YLand, which(!is.na(data$Angle)), col = "black")}
  # 
}
#spraychart(data[1:100,])

laev.visual <- function(data, batter, game = T, point.cex = 2.3, pitch.number = T){
  
  coords <- read.csv(file = "data/Batter LAEV Visual Coords.csv", stringsAsFactors = F)
  
  par(mar = c(.8,.8,.8,.8))
  ## Prepare the data
  if(class(data) == "data.frame" && any(!is.na(data$Angle)) && any(!is.element(c("XCoord", "YCoord", "XLand", "YLand", "ContactType"), colnames(data)))){#any(!is.element(c("ContactType", "LAEVcol", "XCoord", "YCoord"), colnames(data)))){
  data <- data[which(!is.na(data$Angle)),]
  data <- cbind(data, XCoord = c(cos(data$Angle * pi / 180) * data$ExitSpeed), YCoord = c(sin(data$Angle * pi / 180) * data$ExitSpeed), 
                XLand = c(cos((90 - data$Bearing) * pi / 180) * data$Distance), YLand = c(sin((90 - data$Bearing) * pi / 180) * data$Distance))
  data[which(!is.na(data$XLand))[which(!in.out(as.matrix(boshcoords), as.matrix(data[which(!is.na(data$XLand)),c("XLand", "YLand")])) & data[which(!is.na(data$XLand)), "Distance"] > 320)],"PlayResult"] <- c("HomeRun")
  data <- data[which(!is.na(data$XCoord)),]
  data <- cbind(data, Barrel = c(0), Solid = c(0), FnB = c(0), Weak = c(0), Topped = c(0), Under = c(0))
  
  for(contact.type in c("barrel", "solid", "F&B", "weak", "topped", "under")){
    data[,contact.type] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == contact.type),c("X", "Y")]), as.matrix(data[,c("XCoord", "YCoord")])))
  }
  
  data <- cbind(data, Type = ifelse(as.logical(data$barrel), "Barrel", ifelse(as.logical(data$solid), "Solid", ifelse(as.logical(data$`F&B`), "F&B", ifelse(as.logical(data$topped), "Topped", ifelse(as.logical(data$under), "Under", ifelse(as.logical(data$weak), "Weak", "None")))))), 
                LAEVcol = ifelse(as.logical(data$barrel), "red", ifelse(as.logical(data$solid), "lightpink", ifelse(as.logical(data$`F&B`), "coral1", 
                            ifelse(as.logical(data$under), "lightskyblue", ifelse(as.logical(data$weak), "palegoldenrod", ifelse(as.logical(data$topped), "darkolivegreen3", "black")))))), stringsAsFactors = F)
  
  }
  validate_that(length(unique(data$Batter)) == 1, msg = "Multiple batters were found in the data, the most common one will be used")
  if(missing(batter)) batter <- mode(data$Batter)
  
  limage <- readPNG("data/LHB LAEV Visual.png")
  rimage <- readPNG("data/RHB LAEV Visual.png")
  
  batterpic <- tryCatch({readPNG(paste0("data/Batter Contact Pics/", commasplit(batter), ".png"))}, error = function(error){return(NULL)})
  prepic <- tryCatch({readPNG(paste0("data/Batter Contact Pics/", commasplit(batter), " Pre Swing.png"))}, error = function(error){return(NULL)})
  
  if(nrow(data) > 0 && mode(data$BatterSide) == "Left"){
    lefty <- T
    plot(0, 0, type = "n", xlim = c(-115, 90), ylim = c(-115, 115), asp=1, axes=F,xlab="",ylab="")
    rasterImage(limage, -115,-115,0,115)
    if(!is.null(batterpic)){
      rasterImage(batterpic, 2.3, -46, 110, 52.15)
    }
    
    ind <- -1
    ind2 <- 0.5
    text(5, -70, labels = "Barrel: 68% HR / 26% Out / 6% 2B/3B", cex = 0.6, col = "red", pos = 4)
    text(5, -79, "Solid: 40% Out / 35% HR / 25% 2B/3B", col = "deeppink", cex = 0.6, pos = 4)
    text(5, -88, "Flares & Burners: 57% 1B / 29% Out / 14% 2B/3B", col = "coral1", cex = 0.6, pos = 4)
    text(5, -97, "Topped: 74% Out / 24% 1B / 2% 2B/3B", col = "forestgreen", cex = 0.6, pos = 4)
    text(5, -106, "Under: 80% Out / 11% 1B / 7% 2B/3B / 2% HR", col = "dodgerblue3", cex = 0.6, pos = 4)
    text(5, -115, "Weak: 82% Out / 18% 1B", col = "goldenrod3", cex = 0.6, pos = 4)
    rect(4, -120, 129, -65)
    text(-92, 60, "Barrel\n2Bs & HRs\nSolid Contact", cex = 1, srt = 55, col = "black")
    text(-92, 60, "      \n2Bs & HRs\nSolid Contact", cex = 1, srt = 55, col = "white")
    text(-40, 75, "Under\nPOP OUTS\n&\nFLY OUTS", col = "black", cex = 1.25)
    text(-69, -33, "Topped\nGROUNDOUTS/ROLLOVERS", srt = -25, cex = 1.1)
    text(-30, -20, "Weak\nDRIBBLERS", cex = 1.1, col = "black")
    text(-90, 20, "Flare/Burner\nSINGLES", cex = 1.1, col = "black")
    
    # arrows(x0 = -7.5, y0 = 89, y1 = 113, col = "red")
    # text(10, 109, "Exit Velo\n90+ mph", col = "red")
    # 
    # arrows(x0 = -16, y0 = 97, y1 = 113, col = "white")
    # text(-30, 102, "Exit Velo\n100+ mph", col = "white", cex = 0.9)
    
    points(c(seq(0, -100, by = -20), -115), rep(0, 7), pch = "l", type = "o", col = "black")
    text(c(seq(0, -100, by = -20), -115) - 0, rep(-1, 7), paste(c(seq(0, 100, by = 20), 115), "\nmph"), col = c(rep("white", 6), "black"))
    
    # text(20, 20, "Launch\nAngle", col = "green", cex = 1.4)
    # arrows(21, 20, -20, col = "green")
    
    text(30, 2, "Exit\nVelo", col = "black", cex = 1.4)
    arrows(20, 2, 0, col = "black")
    
  } else{
    lefty <- F
    plot(0, 0, type = "n", xlim = c(-90, 115), ylim = c(-115, 115), asp=1, axes=F,xlab="",ylab="")
    rasterImage(rimage, 0,-115,115,115)
    if(!is.null(batterpic)){
      rasterImage(batterpic, -105, -46, -2.3, 52.15)
    }
    ind <- 1
    ind2 <- 0
    text(-126, -70, labels = "Barrel: 68% HR / 26% Out / 6% 2B/3B", cex = 0.6, col = "red", pos = 4)
    text(-126, -79, "Solid: 40% Out / 35% HR / 25% 2B/3B", col = "deeppink", cex = 0.6, pos = 4)
    text(-126, -88, "Flares & Burners: 57% 1B / 29% Out / 14% 2B/3B", col = "coral1", cex = 0.6, pos = 4)
    text(-126, -97, "Topped: 74% Out / 24% 1B / 2% 2B/3B", col = "forestgreen", cex = 0.6, pos = 4)
    text(-126, -106, "Under: 80% Out / 11% 1B / 7% 2B/3B / 2% HR", col = "dodgerblue3", cex = 0.6, pos = 4)
    text(-126, -115, "Weak: 82% Out / 18% 1B", col = "goldenrod3", cex = 0.6, pos = 4)
    rect(-128, -120, -4, -65)
    text(92, 60, "Barrel\n2Bs & HRs\nSolid Contact", cex = 1, srt = -55, col = "black")
    text(92, 60, "      \n2Bs & HRs\nSolid Contact", cex = 1, srt = -55, col = "white")
    text(40, 75, "Under\nPOP OUTS\n&\nFLY OUTS", col = "black", cex = 1.25)
    text(69, -33, "Topped\nROLLOVERS/GROUNDOUTS", srt = 25, cex = 1.1)
    text(30, -20, "Weak\nDRIBBLERS", cex = 1.1, col = "black")
    text(90, 20, "Flare/Burner\nSINGLES", cex = 1.1, col = "black")
    
    # arrows(x0 = 7.5, y0 = 89, y1 = 113, col = "red")
    # text(-10, 109, "Exit Velo\n90+ mph", col = "red")
    # 
    # arrows(x0 = 16, y0 = 97, y1 = 113, col = "white")
    # text(30, 102, "Exit Velo\n100+ mph", col = "white", cex = 0.9)
    
    points(c(seq(0, 100, by = 20), 115), rep(0, 7), pch = "l", type = "o", col = "black")
    text(c(seq(0, 100, by = 20), 115) + 6, rep(1, 7), paste(c(seq(0, 100, by = 20), 115), "\nmph"), col = c(rep("white", 6), "black"))
    
    # text(-20, 20, "Launch\nAngle", col = "green", cex = 1.4)
    # arrows(-10, 20, 21, col = "green")
    
    text(-30, 2, "Exit\nVelo", col = "black", cex = 1.4)
    arrows(-20, 2, 0, col = "black")
    
    
    
  }
  
  points(data$XCoord * ind, data$YCoord, pch=19, cex = point.cex, col = "black")
  points(data$XCoord * ind, data$YCoord, cex = point.cex, col = c("orange", "red", "pink", "green", "purple", "white"))
  
  
  
  # text(102 * ind + ind2, 81, labels = paste(length(which(data$Type == "Barrel")), " Barrels (", round(100 * length(which(data$Type == "Barrel")) / nrow(data)), "%)", sep = ""), col = "red")
  # text(96 * ind + ind2, 93, labels = paste(length(which(data$Type == "Solid")), " Solid Contact (", round(100 * length(which(data$Type == "Solid")) / nrow(data)), "%)", sep = ""), col = "deeppink")
  # text(64 * ind, 27, labels = paste(length(which(data$Type == "F&B")), " Flares &\nBurners (", round(100 * length(which(data$Type == "F&B")) / nrow(data)), "%)", sep = ""), col = "black")
  # text(26 * ind, -68, labels = paste(length(which(data$Type == "Topped")), " Topped (", round(100 * length(which(data$Type == "Topped")) / nrow(data)), "%)", sep = ""), col = "darkgreen")
  # text(22 * ind, 99, labels = paste(length(which(data$Type == "Under")), " Under (", round(100 * length(which(data$Type == "Under")) / nrow(data)), "%)", sep = ""), col = "blue")
  # text(19 * ind, -10, labels = paste(length(which(data$Type == "Weak")), " Weak (", round(100 * length(which(data$Type == "Weak")) / nrow(data)), "%)", sep = ""), col = "goldenrod3")

  # text(102 * ind + ind2, 81, labels = "Barrels", col = "red")
  # text(96 * ind + ind2, 93, labels = "Solid Contact", col = "deeppink")
  # text(64 * ind, 27, labels = "Flares &\nBurners", col = "black")
  # text(26 * ind, -68, labels = "Topped", col = "darkgreen")
  # text(22 * ind, 99, labels = "Under", col = "blue")
  # text(19 * ind, -10, labels = "Weak", col = "goldenrod3")
  
  if("AB_Num" %in% colnames(data) & !is.element("PAnum", colnames(data))) data$PAnum <- data$AB_Num
  if(game){
    if(nrow(data) > 0) text(data$XCoord * ind, data$YCoord, data$PAnum, col = c("orange", "white", "green"))
  } else if(pitch.number){
    text(data$XCoord * ind, data$YCoord, which(!is.na(data$Angle)), col = c("orange", "white", "green"))
  }
  
  
}


laev.visual.ly <- function(data, batter, game = F, lefty, abnum){
  
  ## Load coordinates and background visuals
  coords <- read.csv(file = "data/Batter LAEV Visual Coords.csv", stringsAsFactors = F)
  limage <- readPNG("data/LAEV LHB Labeled.png")
  rimage <- readPNG("data/LAEV RHB Labeled.png")
  
  ## Assign missing variables
  if(missing(batter)){
    assert_that(is.data.frame(data), msg = "batter not specified and data is not a data.frame")
    batter <- mode(data$Batter)
  }
  
  if(missing(lefty)){
    assert_that(is.data.frame(data), msg = "lefty not specified and data is not a data.frame")
    lefty <- (mode(data$BatterSide) == "Left")
  }
  
  if(missing(abnum)){
    assert_that(is.data.frame(data), msg = "abnum was not specified and data is not a data.frame")
    abnum <- "ABNum" %in% data
  }
  
  ## Load batter visuals
  batterpic <- tryCatch({readPNG(paste0("data/Batter Contact Pics/", commasplit(batter), ".png"))}, error = function(error){return(NULL)})
  prepic <- tryCatch({readPNG(paste0("data/Batter Contact Pics/", commasplit(batter), " Pre Swing.png"))}, error = function(error){return(NULL)})

  
  ## Prepare the data -- skip this step if the input data already has these columns (could be a crosstalk SharedData object)
  if(is.data.frame(data) && any(!is.na(data$Angle)) && any(!is.element(c("XCoord", "YCoord", "XLand", "YLand", "ContactType", "Hit_TM_Text"), colnames(data)))){#any(!is.element(c("ContactType", "LAEVcol", "XCoord", "YCoord"), colnames(data)))){
    data %<>% filter(!is.na(Angle)) %>%
      mutate(XCoord = c(cos(Angle * pi / 180) * ExitSpeed), YCoord = c(sin(Angle * pi / 180) * ExitSpeed),
                  XLand = c(cos((90 - Bearing) * pi / 180) * Distance), YLand = c(sin((90 - Bearing) * pi / 180) * Distance)) %>%
      filter(!is.na(XCoord)) %>%
      as.data.frame()

    data[which(!is.na(data$XLand) & data$PlayResult == "Undefined")[which(!in.out(as.matrix(boshcoords), as.matrix(data[which(!is.na(data$XLand) & data$PlayResult == "Undefined"),c("XLand", "YLand")])) & data[which(!is.na(data$XLand) & data$PlayResult == "Undefined"), "Distance"] > 320)],"PlayResult"] <- c("HomeRun")

    for(contact.type in c("barrel", "solid", "F&B", "topped", "under")){
      data[which(!is.na(data$XCoord)), contact.type] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == contact.type),c("X", "Y")]), as.matrix(data[which(!is.na(data$XCoord)),c("XCoord", "YCoord")])))
    }
    data[which(!is.na(data$XCoord)),"weak"] <- as.numeric(data[which(!is.na(data$XCoord)),"ExitSpeed"] < 60)

    data %<>% mutate(ContactType = ifelse(as.logical(barrel), "Barrel", ifelse(as.logical(solid), "Solid", ifelse(as.logical(`F&B`), "F&B", ifelse(as.logical(weak), "Weak", ifelse(as.logical(topped), "Topped", ifelse(as.logical(under), "Under", "None")))))),
                  LAEVcol = ifelse(as.logical(barrel), "red", ifelse(as.logical(solid), "lightpink", ifelse(as.logical(`F&B`), "coral", ifelse(as.logical(weak), "tan",                                                                                                         ifelse(as.logical(under), "lightskyblue", ifelse(as.logical(topped), "darkgreen", "black")))))),
                  Hit_TM_Text = paste("Contact Type:", ContactType, "<br>Launch Angle:", round(Angle), "<br>Exit Velo:", round(ExitSpeed), "<br>Result:", PlayResult,
                                      "<br>Date:", Date, "<br>Inning:", Inning, "<br>Count:", paste(Balls, Strikes, sep = "-"), "<br>Distance:", round(Distance))) %>%
      rbind.fill(data.frame(XCoord = c(ifelse(lefty, -1, 1) * .95 * c(90, 75, 65, 70, 20, 25)), YCoord = c(82, 60, 25, -70, 95, 15),
                            Hit_TM_Text = c("<b>Barrel</b><br>68% HR<br>26% Out<br>  6% 2B/3B", "<b>Solid Contact</b><br>40% Out<br>35% HR<br>25% 2B/3B",
                                            "<b>Flares & Burners</b><br>57% 1B<br>29% Out<br>14% 2B/3B", "<b>Topped</b><br>74% Out<br>24% 1B<br>  2% 2B/3B",
                                            "<b>Under</b><br>80% Out<br>11% 1B<br>  7% 2B/3B<br>  2% HR", "<b>Weak</b><br>82% Out<br>18% 1B"))) %>%
      as.data.frame()

    validate_that(length(unique(data$Batter)) == 1, msg = "Multiple batters were found in the data, the most common one will be used")
  }

  batpic.lst <- list()
  if(lefty){
    # data$XCoord <- data$XCoord * -1
    if(!is.null(batterpic)){
      batpic.lst <- list(source = raster2uri(batterpic), x = 0, y = -46, sizex = 107.7, sizey = 98.15, layer = "below", 
                         sizing = "stretch", xanchor = "left", yanchor = "bottom", xref = "x", yref = "y")
    }

  } else{
    if(!is.null(batterpic)){
      batpic.lst <- list(source = raster2uri(batterpic), x = -105, y = -46, sizex = 105, sizey = 98.15, 
                         layer = "below", sizing = "stretch", xanchor = "left", yanchor = "bottom", xref = "x", yref = "y")
    }

  }
  
  la.ev.cols <- c("Barrel" = "red", "Solid" = "lightpink", "F&B" = "coral", "Weak" = "tan", "Under" = "lightblue", "Topped" = "darkgreen")
  
  if(abnum){
    p <- plot_ly(x = ~XCoord, y = ~YCoord, data = data, source = "laev", symbol = as.factor(~AB_Num), showlegend = T, hoverlabel = list(bgcolor = ~LAEVcol), hoverinfo = "text", text = ~Hit_TM_Text)
  } else{
    p <- plot_ly(x = ~XCoord, y = ~YCoord, data = data, color = ~ContactType, source = "laev", showlegend = T, hoverlabel = list(bgcolor = ~LAEVcol), hoverinfo = "text", text = ~Hit_TM_Text)
  }

  p %>%
  add_annotations(showarrow = F, text = paste0("<b>", c("Barrel", "Solid\nContact", "Flares &\nBurners", "Topped", "Under", "Weak")), x = ~c(ifelse(lefty, -1, 1) * c(90, 75, 65, 70, 20, 25)), y = ~c(82, 60, 25, -70, 95, 15), textfont = list(size = 16), showlegend = F, hoverinfo = "none") %>%
  # add_markers(text = c("Barrel: 68% HR<br>26% Out<br>6% 2B/3B", "Solid: 40% Out<br>35% HR<br>25% 2B/3B",  "Flares & Burners: 57% 1B<br>29% Out<br>14% 2B/3B", "Topped: 74% Out<br>24% 1B<br>2% 2B/3B", "Under: 80% Out<br>11% 1B<br>7% 2B/3B<br>2% HR", "Weak: 82% Out<br>18% 1B"), hoverinfo = "text", showlegend = F, x = ~c(ifelse(lefty, -1, 1) * c(90, 75, 65, 70, 20, 25)), y = ~c(82, 60, 25, -70, 95, 15)) %>%
  add_annotations(yref='paper',xref="paper",y=1.05,x=1.1, text="AB Number",showarrow=F) %>%
  layout(title = "Contact Quality", xaxis = list(visible = F, range = c(ifelse(lefty, -115, -90), ifelse(lefty, 90, 115))), yaxis = list(visible = F, range = c(-115, 115)),
         images = list(list(
           source = raster2uri(vector.ifelse(c(lefty, !lefty, F), list(limage, rimage), if.none = list())),
           xref = "x", yref = "y", x = ifelse(lefty, -115, 0), y = -115, sizex = 115, sizey = 230,
           sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below"
         ), batpic.lst), dragmode = "lasso")  %>%
    highlight("plotly_selected")

  # p
  # if(game){if(nrow(data) > 0){text(data$XCoord * ind, data$YCoord, data$PAnum, col = c("orange", "white", "green"))}} else{text(data$XCoord * ind, data$YCoord, which(!is.na(data$Angle)), col = c("orange", "white", "green"))}
  
}


hitter.stats.from.tm <- function(data){
  round2 <- function(x)round(x, digits = 2)
  
  ## banner at the top w/ just the numbers?
  
  ## poy, 1st team, 3rd team, starter
  ## poy is 99th %ile
  ## 1st team is (num 1st teamers / all players) %ile
  ## same for 3rd team
    ## minimum PA
  ## starters is (100 - percent.of.sample.that.start)%ile
  
  gls <- data.frame(start = c(1.6, 2.56, 1.9, 0.22, 3.29, 4),
             all.acc = c(2.95, 3.93, 4.49, 1.28, 1.49, 5),
             first.team = c(3.21, 4.18, 5.37, 1.55, 1.16, 6),
             poy = c(3.67, 4.88, 6.96, 2.38, 0.68, 7)
              )
#"#bdbdbd", 
  add.targets <- function(x, goals = gls, colors = c("#319236", "#4c51f7", "#9d4dbb", "#f3af19")) geom_errorbar(aes(ymin = goals[,x], ymax = goals[,x]), color = colors[x], size = 2)
  
  data %>%
    filter(PitchCall != "Undefined" & BatterTeam %in% c("NOR_TAR", "NOR_TAR2") & !startsWith(Batter, "Jerz")) %>%
    mutate(AB_End = c(PitchCall %in% c("InPlay", "HitByPitch") | PlayResult != "Undefined" | KorBB %in% c("Strikeout", "Walk")),
           ExpOBP = replace(ExpOBP, is.na(ExpOBP) & PitchCall == "InPlay", ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0)), 
           ExpSLG = replace(ExpSLG, is.na(ExpSLG) & PitchCall == "InPlay", ifelse(PlayResult == "Single", 1, ifelse(PlayResult == "Double", 2, ifelse(PlayResult == "Triple", 3, ifelse(PlayResult == "HomeRun", 4, 0)))))) %>%
    #group_by(Batter) %>%
    summarise(PA = sum(AB_End), K = sum(KorBB == "Strikeout"), BB = sum(KorBB == "Walk"), HBP = sum(PitchCall == "HitByPitch"), Sac = sum(PlayResult == "Sacrifice" & HitType == "Bunt"), H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")), 
              `1B` = sum(PlayResult == "Single"), "1B w/ Normal Defense" = sum(Pred_1B + as.numeric(PlayResult == "Single" & is.na(Pred_1B)), na.rm = T), `2B` = sum(PlayResult == "Double"), "2B w/ Normal Defense" = sum(Pred_2B + as.numeric(PlayResult == "Double" & is.na(Pred_2B)), na.rm = T), 
              `3B` = sum(PlayResult == "Triple"), "3B w/ Normal Defense" = sum(Pred_3B + as.numeric(PlayResult == "Triple" & is.na(Pred_3B)), na.rm = T), HR = sum(PlayResult == "HomeRun"),
              XBH = sum(PlayResult %in% c("Double", "Triple", "HomeRun")), Bases = sum(PlayResult == "Single") + sum(PlayResult == "Double") * 2 + sum(PlayResult == "Triple") * 3 + sum(PlayResult == "HomeRun") * 4, 
              Pitches_Seen = n(),
              Avg_Exit_Velo = mean(pmax(75, ExitSpeed), na.rm = T), Max_Exit_Velo = max(ExitSpeed, na.rm = T),
              Bad_Contact = sum(ContactType %in% c("Weak", "Topped", "Under")), Good_Contact = sum(ContactType %in% c("Barrel", "F&B", "Solid")),
              H_Defense_Excluded = sum(ExpOBP, na.rm = T), XBH_Defense_Excluded = sum(ExpSLG, na.rm = T)) %>%
    mutate(AB = PA - BB - HBP - Sac, BBper = BB / PA * 10, Kper = K / PA * 10, BA = H / AB * 10, SLG = Bases / AB * 10, OBP = (H + BB + HBP) / PA * 10, `2OPS` = (1.7 * OBP + SLG) / 3 * 10, 
           Good_Contact_10 = Good_Contact / (Good_Contact + Bad_Contact) * 10, `BA Defense Excluded` = H_Defense_Excluded / AB, `OBP Defense Excluded` = (BB + HBP + H_Defense_Excluded) / PA, 
           `SLG Defense Excluded` = XBH_Defense_Excluded / AB, Defense_Excluded_2OPS = (`OBP Defense Excluded` * 1.7 + `SLG Defense Excluded`) / 3, `Pitches Per PA` = Pitches_Seen / PA) %>%
    mutate_at(vars(matches("OBP|SLG|BA|OPS|per", ignore.case = F)), funs(round2)) %>%
    mutate_at(vars(-matches("OBP|SLG|BA|Batter|OPS|Pit|per", ignore.case = F)), funs(round)) %>% 
    
    select(BA, OBP, SLG, BBper, Kper, Good_Contact_10) %>% # HR, bases per H
    t() %>% as.data.frame() %>%
    mutate(Stat = factor(rownames(.), levels = c("BA", "OBP", "SLG", "BBper", "Kper", "Good_Contact_10"), ordered = T, labels = c("Batting Avg", "On Base %", "Slugging %", "Walks per 10 PA", "K per 10 PA", "% of Contact That's Well Hit")), 
           Value = as.numeric(as.character(V1))) %>%
  
    ggplot(., aes(Stat, Value, label = Value)) +
      geom_col() +
      sapply(c(1:4), add.targets) +
      geom_label() +
      scale_y_continuous(limits = c(0, 10)) +
      #scale_x_discrete(breaks = c("Batting Avg", "On Base %", "Slugging %")) +
      labs(title = "Stats", x = "", y = "")
    
  
}


####  RENDER REPORTS  ####

main.dir <- "/users/micahdaley-harris/desktop/tar/micahdh.github.io"
render_hitter_game_report <- function(data, auto.tag = F, unc.only = T){
  graphics.off()
  if(unc.only) data <- data[which(data$BatterTeam %in% c("NOR_TAR", "NOR_TAR2")),]
  if(auto.tag) data$TaggedPitchType <- generic.pitch.tag(data)$AutoPitchType
  thedate <- gsub(pattern = "/", replacement = "-", x = mode(data$Date))
  data$Batter <- gsub(data$Batter, pattern= "Caulfield", replacement = "Caufield")
  hitters <- unique(data[which(data$PitchCall != "Undefined"), "Batter"])
  if(!dir.exists(paste0(main.dir, "/Post_Game_Reports/Hitter/", thedate))) dir.create(paste0(main.dir, "/Post_Game_Reports/Hitter/", thedate))
  for(b in hitters){
    rmarkdown::render("Batter_Report.Rmd", output_file = paste0(main.dir, "/Post_Game_Reports/Hitter/", thedate, "/", b, " Hitter Report ", thedate, ".html"), 
                      params = list(game = data[which(data$Batter == b),], allplayers = hitters, practice = F))
  }
}

render_hitter_bp_report <- function(data, unc.only = T){
  
  graphics.off()
  if(unc.only) data <- data[which(data$BatterTeam %in% c("NOR_TAR", "NOR_TAR2")),]
  
  thedate <- gsub(pattern = "/", replacement = "-", x = mode(data$Date))
  data$Batter <- gsub(data$Batter, pattern= "Caulfield", replacement = "Caufield")
  hitters <- unique(data[which(!is.element(data$Batter, "")),"Batter"])
  if(!dir.exists(paste0(main.dir, "/Practice_Reports/Hitter/", thedate))) dir.create(paste0(main.dir, "/Practice_Reports/Hitter/", thedate))
  for(b in hitters){
    data[which(data$Batter == b), "BatterSide"] <- ifelse(b %in% roster[which(substr(roster$B.T, 1, 1) == "L"), "Name"], "Left", "Right")
    rmarkdown::render("Batter_Report.Rmd", output_file = paste0(main.dir, "/Practice_Reports/Hitter/", thedate, "/", b, " Hitter Report ", thedate, ".html"), 
                      params = list(game = data[which(data$Batter == b),], allplayers = hitters, practice = T))
  }
  
  
}



player.laev.daterange <- function(data){
  
  data <- data[which(!is.na(data$ContactType) & data$PlayResult != "Undefined"),]
  
  for(batter in unique(data$Batter)){
    rmarkdown::render("Launch Angle Exit Velo Page.Rmd", output_file = paste0(gsub(batter, pattern = ", ", replacement = "_"), "_LA_EV.html"), output_dir = paste0(main.dir, "/LA_EV"), params = list(bdat = data[which(data$Batter == batter),]))
  }
  
}




# data 28638

###########   Dashboard     ################
# os$Name[-c(1, 6, 14, 15, 18, 19)]

render.dashboard <- function(hitters, coach = T, videodrops = T){
  if(coach) rmarkdown::render("Hitting_Coach.Rmd", output_file = "Hitting_Coach.html", output_dir = paste0(main.dir, "/Hitters"))
  alldata <- data
  if(missing(hitters)) hitters <- hs$Name[-c(1, 2, 4, 5, 6, 14, 15, 18, 19, 23)]
  
  if(videodrops){
    batterx <- data9 %>% 
      filter(yyyymmdd > "2019-02-17" & PitchCall != "Undefined" & Scrimmage == "Season" & endsWith(Date, "9") & startsWith(BatterTeam, "NOR_TAR")) %>% #yyyymmdd > "2019-02-17" # GameID == "UMLUNC030219"
      mutate(ab_string = paste(paste("AB", Times_In_The_Box), AB_Result, sep = " - "), pitch_string = paste(paste("Pitch", PitchofPA), Pitch_Result, sep = " - "),
             HS_video_link = paste0("https://s3.amazonaws.com/unchitterhighspeedvideo/", GameID, paste0("_", substr(BatterSide, 1, 1), "F/") , key, ".mp4"),
             TV_video_link = paste0("https://s3.amazonaws.com/publicuncsynergyvideo/", GameID, "/", key, ".mp4")
      ) %>%
      arrange(desc(yyyymmdd), Inning, PitchofPA) %>%
      select(Player = Batter, Game, ab_string, pitch_string, HS_video_link, TV_video_link)
  }
  
  for(hitter in hitters){
    print(hitter)
    pdata <- alldata %>%
      filter(!is.element(PitchCall, c("Undefined", "Warmup")) & Batter == hitter)
    
    print(nrow(pdata))
    rmarkdown::render("Hitter_App.Rmd", output_file = paste0(hitter, ".html"),
                      output_dir = paste0(main.dir, "/Hitters"), params = list(batter = hitter, data = pdata))
    if(videodrops) videodropdown(batterx %>% filter(Player == hitter))
  }
  
}

render.dashboard.current <- function(hitters, coach = F){
  if(coach) rmarkdown::render("Hitting_Coach.Rmd", output_file = "Hitting_Coach.html", output_dir = paste0(main.dir, "/Hitters"))
  alldata <- data9
  if(missing(hitters)) hitters <- hs$Name[-c(1, 2, 4, 5, 6, 14, 15, 18, 19, 23)]
  for(hitter in hitters){
    print(hitter)
    pdata <- alldata %>%
      filter(!is.element(PitchCall, c("Undefined", "Warmup")) & Batter == hitter)
    print(nrow(pdata))
    rmarkdown::render("Hitter_App.Rmd", output_file = paste0(hitter, ".html"),
                      output_dir = paste0(main.dir, "/Hitters/2019"), params = list(batter = hitter, data = pdata))
  }
}

