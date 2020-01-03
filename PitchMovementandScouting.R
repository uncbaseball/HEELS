############################    Pitch Tagging and Classification    #########################


generic.pitch.tag <- function(data){
  
  ## Intentional balls
  
  require(mgcv)
  if(any(!is.element(c("HorzBreak", "RelSpeed", "PitcherThrows"), colnames(data)))){
    stop("'HorzBreak', 'RelSpeed' and/or 'PitcherThrows' were not found in the data")
  }
  
  if(!is.element("AutoPitchType", colnames(data))){
    warning("'AutoPitchType' was not found in the data so it was added")
  }
  
  if(length(which(!is.element(data$PitcherThrows, c("Left", "Right")))) > 0){
    warning("bad stuff")
    # should eventually deduce based on L/R (if high velo)
  }
  
  data$AutoPitchType <- c("Und")
  
  pitch.tag.boxes <- read.csv(file = paste(substr(getwd(), 1, gregexpr("/", getwd(), fixed = TRUE)[[1]][3]), "Desktop/TAR/Pitcher/3 Pitch Tag Boxes.csv", sep = ""), stringsAsFactors = F)
  
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
    
    
    print(pside)
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





tag.with.df <- function(data, alldf = readRDS("data/Latest UNC Pitch Tagging DF.rds")){
  ## Will need to use PitcherTeam and Date down the road
  
  data$TaggedPitchType <- c("Undefined")
  for(p in unique(data$Pitcher)){
    df <- alldf[which(alldf$Pitcher == p),]
    for(pitch in unique(df$Pitch)){
      ## tag it with df
      pitch.rows <- which(data$Pitcher == p)#c(1:nrow(data))
      p.df <- df[which(df$Pitch == pitch),]
      
      for(row in c(1:nrow(p.df))){
        
        if(p.df[row, "MinMax"] == "min" & length(pitch.rows) > 0){
          pitch.rows <- pitch.rows[which(data[pitch.rows, as.character(p.df[row, "Var"])] > p.df[row, "Val"])]
        } else if(p.df[row, "MinMax"] == "max" & length(pitch.rows) > 0){
          pitch.rows <- pitch.rows[which(data[pitch.rows, as.character(p.df[row, "Var"])] < p.df[row, "Val"])]
        }
        
      }
      if(length(pitch.rows) > 0){
        data[pitch.rows, "TaggedPitchType"] <- as.character(pitch)
      }
    }
  }
  
  return(data)
}

tag.pitches <- function(data, alldf = readRDS("data/Latest UNC Pitch Tagging DF.rds"), url, p = mode(data$Pitcher), remove.warmups = F, xvar = "HorzBreak", yvar = "RelSpeed", show.starting.tags = T){
  
  ## There's now a PitcherTeam and Date column but they aren't used
    # new dates are entered when a new line is drawn (not a change) --> question is are we overwriting or copying
    
  
  if(!missing(url)) require(RCurl)
  
  click <- function()as.matrix(as.data.frame(locator(n = 1)))
  
  assert_that("PitchCall" %in% colnames(data))
  
  par(mfrow = c(1,1))
  
  data <- data[which(data$PitchCall != ifelse(remove.warmups, "Undefined", "")),]
  
  
  if("PitchCall" %in% colnames(data)) data <- data[which(data$PitchCall != "BallIntentional"),]
  
  if(!is.element("TaggedPitchType", colnames(data))) data <- cbind(data, TaggedPitchType = c("Undefined"))
  
  assertthat::validate_that(length(unique(data$Pitcher)) == 1)

  allvars <- c("RelSpeed", "HorzBreak", "InducedVertBreak")
  
  alldata <- data
  for(p in unique(data$Pitcher)){
    data <- alldata[which(alldata$Pitcher == p),]
    df <- alldf[which(alldf$Pitcher == p),]
    not.done <- T
    
    while(not.done){
      if(!missing(url)){
        data <- read.csv(textConnection(getURL(url)))
        if(any(grepl("/", data$PitchNo))){
          data[,c(2:ncol(data))] <- data[,c(1:(ncol(data) - 1))]
          data[,1] <- c(1:nrow(data))
        }
        if(!missing(p)) data <- data[which(data$Pitcher == p),]
      } 
      
      if(nrow(df) > 0 | !show.starting.tags){
        data <- tag.with.df(data = data, alldf = df)
      }
      
      
      plot(data[,c(xvar, yvar)], ylim = c(min(data[,yvar], na.rm = T), max(data[,yvar], na.rm = T) + .1 * diff(range(data[,yvar], na.rm = T))), 
           pch = ifelse(data$PitchCall == "Undefined", 8, 16), col = ifelse(data$TaggedPitchType %in% c("FB", "FT"), "blue", ifelse(data$TaggedPitchType == "CH", "green", ifelse(data$TaggedPitchType %in% c("BB", "CU"), "red", ifelse(data$TaggedPitchType %in% c("BB2", "SL", "FC"), "orange", "black")))))
      text(par("usr")[1], par("usr")[3], p, pos = 4, font = 2)
      done.button <- make.button(button.order = 1, tot.buttons = 5, txt = "Done")
      restart.button <- make.button(button.order = 2, tot.buttons = 5, txt = "Restart")
      change.var.button <- make.button(button.order = 3, tot.buttons = 5, txt = "Change Axes")
      change.button <- make.button(button.order = 4, tot.buttons = 5, txt = "Change")
      remove.button <- make.button(button.order = 5, tot.buttons = 5, txt = "Remove")
      
      
      for(i in c(1:nrow(df))){ 
        
        if(nrow(df) > 0 && df[i, "Var"] == xvar){
          text(df[i, "Val"], mean(par("usr")[c(3, 4)]), i); abline(v = df[i, "Val"], col = ifelse(df[i, "Var"] %in% c("FB", "FT"), "blue", ifelse(df[i, "Pitch"] == "CH", "green", ifelse(df[i, "Pitch"] %in% c("BB", "CU"), "red", ifelse(df[i, "Pitch"] %in% c("BB2", "SL", "FC"), "orange", "black"))))) 
        } else if(nrow(df) > 0 && df[i, "Var"] == yvar){ 
          text(mean(par("usr")[c(1, 2)]), df[i, "Val"], i); abline(h = df[i, "Val"], col = ifelse(df[i, "Var"] %in% c("FB", "FT"), "blue", ifelse(df[i, "Pitch"] == "CH", "green", ifelse(df[i, "Pitch"] %in% c("BB", "CU"), "red", ifelse(df[i, "Pitch"] %in% c("BB2", "SL", "FC"), "orange", "black")))))
        }
      }
      
      pt <- click()
      
      if(in.out(bnd = as.matrix(done.button), x = pt)){
        not.done <- F
      } else if(in.out(bnd = as.matrix(change.var.button), x = pt)){
        clear.buttons()
        #message("which button do you want to change")
        xvar.button <- make.button(button.order = 1, tot.buttons = 2, txt = xvar)
        yvar.button <- make.button(button.order = 2, tot.buttons = 2, txt = yvar)
  
        xvar1 <- xvar
        yvar1 <- yvar
        
        var.to.ch <- ""
        while(var.to.ch == ""){
          pt2 <- click()
          var.to.ch <- ifelse(in.out(bnd = as.matrix(xvar.button), pt2), xvar, ifelse(in.out(bnd = as.matrix(yvar.button), pt2), yvar, ""))
        }
        
        ## Once we use more than 3 variables on this will be a selection
        newvar <- allvars[which(!is.element(allvars, c(xvar, yvar)))]
        
        xvar <- ifelse(var.to.ch == xvar, newvar, xvar)
        yvar <- ifelse(var.to.ch == yvar, newvar, yvar)
        
        
        #clear.buttons()
        #message(what do you want the new variable to be)
        # for(new.var in allvars[which(allvars != var)]){
        #   assign(paste(new.var, "button", sep = "."), make.button(button.order = which(allvars[which(allvars != var)] == new.var), tot.buttons = length(allvars[which(allvars != var)], txt = new.var)))
        # }
        
        
        
      } else if(in.out(bnd = as.matrix(restart.button), x = pt)){
        #### Restart  ####
        df <- data.frame()
      } else if(in.out(bnd = as.matrix(change.button), x = pt)){
        #### Change row ####
        loc <- locator(n = 1)
        ind <- which.min(abs(df$Val - loc[[1]]) * ifelse(df$Var == xvar, 1, 0) + abs(df$Val - loc[[2]]) * ifelse(df$Var == yvar, 1, 0) + 100 * !is.element(df$Var, c(xvar, yvar)))
        clear.buttons()
        make.button(1, 1, txt = ind)
        df[ind, "Val"] <- locator(n = 1)[[ifelse(df[ind, "Var"] == xvar, 1, 2)]]
      } else if(in.out(bnd = as.matrix(remove.button), x = pt)){
        #### Remove row ####
        loc <- locator(n = 1)
        ind <- which.min(abs(df$Val - loc[[1]]) * ifelse(df$Var == xvar, 1, 0) + abs(df$Val - loc[[2]]) * ifelse(df$Var == yvar, 1, 0) + 100 * !is.element(df$Var, c(xvar, yvar)))
        clear.buttons()
        make.button(1, 1, txt = ind)
        df <- df[-ind,]
      } else{
        #### Add row  ####
        clear.buttons()
        #text(labels = "Which variable is this parameter for?")
        xvar.button <- make.button(button.order = 1, tot.buttons = 2, txt = xvar)
        yvar.button <- make.button(button.order = 2, tot.buttons = 2, txt = yvar)
        
        var <- NA
        while(is.na(var)){
          pt2 <- click()
          var <- ifelse(in.out(bnd = as.matrix(xvar.button), x = pt2), xvar, ifelse(in.out(bnd = as.matrix(yvar.button), x = pt2), yvar, NA))
        }
  
        clear.buttons()
        min.but <- make.button(button.order = 1, tot.buttons = 2, txt = "Min")
        max.but <- make.button(button.order = 2, tot.buttons = 2, txt = "Max")
        
        minmax <- NA
        while(is.na(minmax)){
          pt3 <- click()
          minmax <- ifelse(in.out(bnd = as.matrix(min.but), x = pt3), "min", ifelse(in.out(bnd = as.matrix(max.but), x = pt3), "max", NA))
        }
        #text(labels = "Which pitch is this parameter for?")
        #text("click anywhere else to cancel")
        clear.buttons()
        fb <- make.button(button.order = 1, tot.buttons = 6, txt = "FB")
        ft <- make.button(button.order = 2, tot.buttons = 6, txt = "FT")
        ch <- make.button(button.order = 3, tot.buttons = 6, txt = "CH")
        bb1 <- make.button(button.order = 4, tot.buttons = 6, txt = "BB")
        cu <- make.button(button.order = 5, tot.buttons = 6, txt = "CU")
        
        bb2 <- make.button(button.order = 6, tot.buttons = 6, txt = "SL")
        
        pt4 <- click()
        
        # button for FB/CH/BB 1/BB 2
        
        pitch <- ifelse(in.out(bnd = as.matrix(fb), x = pt4), "FB", ifelse(in.out(bnd = as.matrix(ft), x = pt4), "FT", ifelse(in.out(bnd = as.matrix(ch), x = pt4), "CH", ifelse(in.out(bnd = as.matrix(bb1), x = pt4), "BB", ifelse(in.out(bnd = as.matrix(cu), x = pt4), "CU", ifelse(in.out(bnd = as.matrix(bb2), x = pt4), "SL", NA))))))
        
        if(!is.na(pitch)) df <- rbind(df, data.frame(Pitch = pitch, Var = var, MinMax = minmax, Val = ifelse(var == xvar, pt[1,1], pt[1,2]), Pitcher = p, PitcherTeam = mode(data[which(data$Pitcher == p), "PitcherTeam"]), yyyymmdd = paste0("20", str_sub(as.Date(mode(data[which(data$Pitcher == p), "Date"]), format = "%m/%d/%Y"), 3, -1))))
      }
    }
    alldata[which(alldata$Pitcher == p),"TaggedPitchType"] <- data$TaggedPitchType
    alldf <- rbind(alldf[which(alldf$Pitcher != p),], df)
  }
  if(yn("Save the tagging DF over the old one?")) saveRDS(unique(alldf), file = "data/Latest UNC Pitch Tagging DF.rds")
  
  return(list(alldata, alldf))

}






classify.pitch.movement <- function(x, data, min.pitches = 2, tot = "", rate.teams = unique(data$BatterTeam)){
  
  ## require at least 2 pitches, regress so that pitches are thrown down the middle
  
  if(!is.element(tot, c("", "Tot."))){
    stop("tot must be one of '', 'Tot.'")
  }
  
  if(missing(x)){
    data <- data[which(!is.element(data$TaggedPitchType,c("Undefined", "IntentionalBall", "Und")) & data$RelSpeed > 60),which(names(data) != "Last.Pitch")]
    if(any(!is.element(c("RelSide", "RelHeight", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax0", "ay0", "az0"), names(data)))){
      data <- add9p.to.lgtm(data)
    }
    x <- data %>%
      group_by(Pitcher, TaggedPitchType) %>%
      #summarise(PitcherThrows = mode(PitcherThrows), Velo = mean(RelSpeed, na.rm = T), HorzBreak = mean(RelSide - PlateLocSide, na.rm = T), InducedVertBreak = mean(RelHeight - PlateLocHeight, na.rm = T)) %>%
      summarise(PitcherThrows = mode(PitcherThrows), VeloMin = round(mean(RelSpeed, na.rm = T) - ifelse(n() == 1, 0, sd(RelSpeed, na.rm = T))), Velo = mean(RelSpeed, na.rm = T), VeloMax = round(mean(RelSpeed, na.rm = T) + ifelse(n() == 1, 0, sd(RelSpeed, na.rm = T))), 
                HorzBreak = mean(HorzBreak, na.rm = T), InducedVertBreak = mean(InducedVertBreak, na.rm = T), Tot.HorzBreak = mean(RelSide - PlateLocSide, na.rm = T), Tot.InducedVertBreak = mean(RelHeight - PlateLocHeight, na.rm = T), 
                x0 = mean(x0, na.rm = T), y0 = mean(y0, na.rm = T), z0 = mean(z0, na.rm = T), vx0 = mean(vx0, na.rm = T), vy0 = mean(vy0, na.rm = T), vz0 = mean(vz0, na.rm = T), ax = mean(ax0, na.rm = T), ay = mean(ay0, na.rm = T), az = mean(az0, na.rm = T), 
                LHBrate = length(which(startsWith(BatterSide, "L") & BatterTeam %in% rate.teams)), RHBrate = length(which(startsWith(BatterSide, "R") & BatterTeam %in% rate.teams)), n = length(which(BatterTeam %in% rate.teams))) %>%
      mutate(VeloRange = paste(VeloMin, VeloMax, sep = " - ")) %>%
      filter(n >= min.pitches) %>%
      #select(-n) %>%
      as.data.frame()
    for(p in unique(x$Pitcher)){
      x[which(x$Pitcher == p), "LHBrate"] <- round(100 * x[which(x$Pitcher == p), "LHBrate"] / sum(x[which(x$Pitcher == p), "LHBrate"]))
      x[which(x$Pitcher == p), "RHBrate"] <- round(100 * x[which(x$Pitcher == p), "RHBrate"] / sum(x[which(x$Pitcher == p), "RHBrate"]))
    }
    #x <- x[which(x$LHBrate > 5 | x$RHBrate > 5),]
  }
  
  if(any(!is.element(x$PitcherThrows, c("Left", "Right")))){
    warning("PitcherThrows values other than 'Left' and 'Right' were found")
  }
  
  x$TaggedPitchType <- as.character(x$TaggedPitchType)
  
  x <- cbind(x, AutoPitchType = ifelse(x$TaggedPitchType %in% c("FB", "CH"), x$TaggedPitchType, ifelse(x$TaggedPitchType %in% c("FT", "SI", "CT"), "FB", "BB")))
  
  lg.avgs <- read.csv(file = "/Users/micahdaley-harris/Desktop/TAR/Statcast & Trackman/League Average Movement.csv", stringsAsFactors = F)
  
  x <- merge(x, lg.avgs, by = c("AutoPitchType", "PitcherThrows"), all.x = T, all.y = F)
  
  x <- cbind(x, horz.sd.from.mean = (x[,paste(tot, "HorzBreak", sep = "")] - x[,paste("Avg.", tot, "HorzBreak", sep = "")]) / x[,paste("Sd.", tot, "HorzBreak", sep = "")],
             vert.sd.from.mean = (x[,paste(tot, "InducedVertBreak", sep = "")] - x[,paste("Avg.", tot, "InducedVertBreak", sep = "")]) / x[,paste("Sd.", tot, "InducedVertBreak", sep = "")])
  ## Flip LHP
  x$HorzBreak <- x$HorzBreak * ifelse(x$PitcherThrows == "Left", -1, 1)
  x$horz.sd.from.mean <- x$horz.sd.from.mean * ifelse(x$PitcherThrows == "Left", -1, 1)
  
  # x <- cbind(x, horz.dir = ifelse(sign(x$HorzBreak) == 1, "run", "cut"),
  #            vert.dir = ifelse(sign(x$InducedVertBreak) == 1, "flat", "sink"))
  # 
  # x <- cbind(x, horz.qual2 = ifelse(x$TaggedPitchType %in% c("FB", "CH"), 
  #                                   ifelse(x$horz.dir == "cut", "", ifelse(x$horz.sd.from.mean > 0, "high", "low")),
  #                                          ifelse(x$horz.dir == "run", "", ifelse(x$horz.sd.from.mean < 0, "high", "low"))))
  # x$horz.dir <- paste(x$horz.qual2, x$horz.dir)
  
  x <- cbind(x, horz.dir = base::ifelse(x$TaggedPitchType == "BB", base::ifelse(x$HorzBreak > 0, "run", "cut"), base::ifelse(x$horz.sd.from.mean > 0, "run", "cut")), vert.dir = base::ifelse(x$InducedVertBreak > 0, "flat", "sink"))

  # x$horz.dir <- NA
  # x[which(x$TaggedPitchType == "BB" & x$HorzBreak > 0 | x$TaggedPitchType != "BB" & x$horz.sd.from.mean > 0), "horz.dir"] <- "run"
  # x[which(x$TaggedPitchType == "BB" & x$HorzBreak <= 0 | x$TaggedPitchType != "BB" & x$horz.sd.from.mean < 0), "horz.dir"] <- "cut"
  # 
  # x$vert.dir <- NA
  # x[which(x$InducedVertBreak >= 0), "vert.dir"] <- c("flat")
  # x[which(x$InducedVertBreak < 0), "vert.dir"] <- c("sink")
  
  
  x$horz.sd.from.mean <- round_any(x = abs(x$horz.sd.from.mean), f = ceiling, accuracy = 0.5)
  x$vert.sd.from.mean <- round_any(x = abs(x$vert.sd.from.mean), f = ceiling, accuracy = 0.5)
  x[which(x$horz.sd.from.mean > 2.5), "horz.sd.from.mean"] <- c(2.5)
  x[which(x$vert.sd.from.mean > 2.5), "vert.sd.from.mean"] <- c(2.5)
  
  quals <- data.frame(sd.from.mean = seq(0.5, by = 0.5, length.out = 5), qualifier = c("no", "some", "a lot of", "elite", "unreal"))
  x <- merge(x, quals, by.x = "horz.sd.from.mean", by.y = "sd.from.mean", all.x = T, all.y = F)
  x <- merge(x, quals, by.x = "vert.sd.from.mean", by.y = "sd.from.mean", all.x = T, all.y = F, suffixes = c(".horz", ".vert"))
  x <- cbind(x, cut.run = ifelse(x$qualifier.horz == "no", "straight", paste(x$qualifier.horz, x$horz.dir)), ride.sink = ifelse(x$qualifier.vert == "no", "flat", paste(x$qualifier.vert, x$vert.dir)))
  
  ## Flip LHP Back
  x$HorzBreak <- x$HorzBreak * ifelse(x$PitcherThrows == "Left", -1, 1)
  x$horz.sd.from.mean <- x$horz.sd.from.mean * ifelse(x$PitcherThrows == "Left", -1, 1)
  
  if(tot == ""){
    return(x[,-grep(pattern = "Tot", x = names(x))])
  }
  return(x)
  
}

arsenal.excel <- function(x, starters = "", team = ""){
  team <- ifelse(team == "" & "PitcherTeam" %in% colnames(x), mode(x$PitcherTeam), team)
  for(col in which(colnames(x) != "n")){x[,col] <- as.character(x[,col])}
  x <- x %>% arrange(Pitcher, desc(n)) %>% dplyr::select(Pitcher, TaggedPitchType, VeloRange, LHBrate, RHBrate, n, qualifier.horz, horz.dir, qualifier.vert, vert.dir) %>% as.data.frame()
  x <- rbind(c("STARTERS", rep("", ncol(x) - 1)), x[which(x$Pitcher %in% starters),], c("RELIEVERS", rep("", ncol(x) - 1)), x[which(!is.element(x$Pitcher, starters)),])
  ret <- data.frame()
  for(p in unique(x$Pitcher)){
    ret <- rbind(ret, rep("", ncol(x)), x[which(x$Pitcher == p)[order(as.numeric(x[which(x$Pitcher == p),"n"]), decreasing = T)],])
    ret[which(ret$Pitcher == p)[-1],"Pitcher"] <- c("")
  }
  write.csv(ret, file = paste0(tar.path, "/team scouting/", team, " Pitch Arsenals.csv"), row.names = F)
  return(ret)
}


add9p.to.lgtm <- function(data){
  vars <- c("RelSide", "RelHeight", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax0", "ay0", "az0")
  oldnames <- names(data)
  for(var in vars){
    if(var == "y0"){
      data <- cbind(data, y0 = c(50))
    } else{
      if(!exists(var)){
        load("/Users/micahdaley-harris/Desktop/TAR/Statcast & Trackman/LG TM to 9 param.RData")
      }
      data <- cbind(data, unname(predict(get(var), data)))
    }
  }
  
  names(data) <- c(oldnames, vars)
  
  return(data)
}
  
pfx.to.tm <- function(data, from = "pfx"){
  assertthat::assert_that(from %in% c("pfx", "tm"))
  if(!exists("HorzBreak.from.pfx")){
    load(paste0(tar.path, "/Statcast & Trackman/Pfx to TM.RData"))
  }
  if(from == "pfx"){
    vars <- c("HorzBreak", "InducedVertBreak")
  } else if(from == "tm"){
    vars <- c("pfxx", "pfxz")
  }
  
  for(var in vars){
    data <- cbind(data, unname(predict(get(paste(tolower(var), "from", from, sep = ".")), data)))
  }
  names(data)[c((ncol(data) - 1):ncol(data))] <- vars
  return(data)
  
}

# atm <- dbGetQuery(tarconn, "SELECT * FROM `alltm`")
# pfxx.from.tm <- lm("pfxx ~ HorzBreak", data = atm)
# pfxz.from.tm <- lm("pfxx ~ HorzBreak", data = atm)
# horzbreak.from.pfx <- lm("HorzBreak ~ pfxx", data = atm)
# inducedvertbreak.from.pfx <- lm("InducedVertBreak ~ pfxz", data = atm)
# save(pfxx.from.tm, pfxz.from.tm, horzbreak.from.pfx, inducedvertbreak.from.pfx, file = paste0(tar.path, "/Statcast & Trackman/Pfx to TM.RData"))



movement.circle <- function(ag = data.frame(), pitches = unique(ag$TaggedPitchType), showunc = T, col = "black",
                            textcolumn = "InitialsNumber", plot.title = "", add = F){
  uncpitchmov <- read.csv(file = "UNC Pitch Movement.csv", stringsAsFactors = F) %>% mutate(col = "cornflowerblue")
  agp <- ag %>% mutate(col = col)
  
  if(showunc) agp <- rbind.data.frame(uncpitchmov, agp)
  
  agp %<>% filter(TaggedPitchType %in% pitches) 
  
  symbols(agp$HorzBreak, agp$InducedVertBreak, circles = rep(1, nrow(agp)), inches = rep(.15, nrow(agp)), add = add, 
          xlab = "Cut/Run", ylab = "Hop/Sink", main = plot.title, fg = agp$col)
  
  text(agp$HorzBreak, agp$InducedVertBreak, as.character(agp$InitialsNumber), cex = .6, font = 2, col = col) # [, textcolumn]
  abline(v = 0)
}




