######################    Pitcher Report 2018     ####################


#' Lay out panels in a grid with different scales
#'
#' `facet_grid_sc` is a variant of `facet_grid`
#' @inheritParams ggplot2::facet_grid
#' @param scales A list of two elements (`x` and `y`). Each element can be either
#' `"fixed"` (scale limits shared across facets), `"free"` (with varying limits per facet), or
#'  a named list, with a different scale for each facet value. Previous scale values
#'  (`"fixed"`, `"free_x"`, `"free_y"`, `"free"` are accepted but soft-deprecated).
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' library(scales)
#' # Custom scales per facet:
#'  mydf <- data.frame(
#'    Subject = rep(c("A", "B", "C", "D"), each = 3),
#'    Magnitude = rep(c("SomeValue", "Percent", "Scientific"), times = 4),
#'    Value=c(c(170,0.6,2.7E-4),
#'            c(180, 0.8, 2.5E-4),
#'            c(160, 0.71, 3.2E-4),
#'            c(159, 0.62, 3E-4)))
#'
#'  scales_y <- list(
#'    Percent = scale_y_continuous(labels=percent_format()),
#'    SomeValue = scale_y_continuous(),
#'    Scientific = scale_y_continuous(labels=scientific_format())
#'  )
#'
#'  ggplot(mydf) +
#'    geom_point(aes(x=Subject, y=Value)) +
#'    facet_grid_sc(rows = vars(Magnitude), scales = list(y = scales_y))
#'
facet_grid_sc <- function(rows = NULL, cols = NULL, scales = "fixed",
                          space = "fixed", shrink = TRUE,
                          labeller = "label_value", as.table = TRUE,
                          switch = NULL, drop = TRUE, margins = FALSE,
                          facets = NULL) {
  # `facets` is soft-deprecated and renamed to `rows`
  if (!is.null(facets)) {
    rows <- facets
  }
  # Should become a warning in a future release
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  
  if (is.list(scales)) {
    free <- list(
      x = identical(scales$x, "free") || is.list(scales$x),
      y = identical(scales$y, "free") || is.list(scales$y)
    )
  } else {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
  }
  
  custom_scales <- list(x = NULL, y = NULL)
  if (is.list(scales)) {
    # A different scale per facet:
    if (is.list(scales$x)) {
      if (is.null(names(scales$x))) {
        stop("Custom facet scales for x should be named according to facet column values", call. = FALSE)
      }
      custom_scales$x <- scales$x
    }
    if (is.list(scales$y)) {
      if (is.null(names(scales$y))) {
        stop("Custom facet scales for y should be named according to facet row values", call. = FALSE)
      }
      custom_scales$y <- scales$y
    }
  }
  
  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )
  
  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }
  
  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)
  n <- length(facets_list)
  if (n > 2L) {
    stop("A grid facet specification can't have more than two dimensions", call. = FALSE)
  }
  if (n == 1L) {
    rows <- rlang::quos()
    cols <- facets_list[[1]]
  } else {
    rows <- facets_list[[1]]
    cols <- facets_list[[2]]
  }
  
  # Check for deprecated labellers
  labeller <- ggplot2:::check_labeller(labeller)
  
  ggplot2::ggproto(NULL, FacetGridScales,
                   shrink = shrink,
                   params = list(rows = rows, cols = cols, margins = margins,
                                 scales = custom_scales,
                                 free = free, space_free = space_free, labeller = labeller,
                                 as.table = as.table, switch = switch, drop = drop)
  )
}

#' ggproto facet
#'
#' @export
FacetGridScales <- ggplot2::ggproto(
  "FacetGridScales", ggplot2::FacetGrid,
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(params$scales$x)) {
      facet_x_names <- as.character(layout[[names(params$cols)]])
      scales$x <- lapply(params$scales$x[facet_x_names], function(x) x$clone())
    } else if (!is.null(x_scale)) {
      scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
    }
    if (!is.null(params$scales$y)) {
      facet_y_names <- as.character(layout[[names(params$rows)]])
      scales$y <- lapply(params$scales$y[facet_y_names], function(x) x$clone())
    } else if (!is.null(y_scale)) {
      scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
    }
    scales
  }
)



pitcherreport <- function(data, save = F, type = "pdf", teams = c("NOR_TAR", "NOR_TAR2")){
  
  require(plyr); require(dplyr)
  require(lubridate)
  require(png)
  
  mode <- function(x){rev(names(sort(table(x))))[1]}
  psplit <- function(x){strsplit(x = x, split = ", ")[[1]][1]}
  
  
  # if(!exists("pitchtagging")){
  #   source("Pitch Tagging.r")
  # }
  # 
  # if(!exists("releasepointgraph")){
  #   source("Release Point Graphs.r")
  # }  
  
  cols <- c("purple", "red", "orange", "dodgerblue", "pink", "forestgreen")
  lhb.pic <- readPNG(paste(substr(getwd(), 1, gregexpr("/", getwd(), fixed = TRUE)[[1]][3]), "Desktop/TAR/Release Point Pics/LHB Pitcher View.png", sep = ""))
  rhb.pic <- readPNG(paste(substr(getwd(), 1, gregexpr("/", getwd(), fixed = TRUE)[[1]][3]), "Desktop/TAR/Release Point Pics/RHB Pitcher View.png", sep = ""))
  
  
  # slide trackman cols
  data <- data[which(data$PitchCall != "Undefined"),]
  if(length(which(is.element(teams, c("NOR_TAR", "NOR_TAR2")))) > 0){
    data <- pitchtagging(data)
  }
  
  if(length(which(!is.element(teams, c("NOR_TAR", "NOR_TAR2")))) > 0){
    data[which(!is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))),"TaggedPitchType"] <- generic.pitch.tag(data[which(!is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))),])$AutoPitchType
    print(table(data$TaggedPitchType))
  }
  data$Time <- paste("1-1-1", data$Time) ## note there will be mistakes if some pitches were thrown before noon/midnight and some after
  data <- data[order(data$PitchNo, decreasing = F),]
  data <- cbind(data, Strike = as.numeric(!is.element(data$PitchCall, c("BallCalled", "BallIntentional", "HitByPitch"))),
                Count = paste(data$Balls, data$Strikes, sep = "-"), TimeElapsed = c(NA, as.numeric(difftime(data[c(2:nrow(data)), "Time"], data[c(1:(nrow(data) - 1)), "Time"]))))
  data[which(!is.element(data$PitchCall, c("BallCalled", "StrikeCalled", "StrikeSwinging")) | c(data$Batter[-c(1)] != data$Batter[-c(nrow(data))], F)) + 1, "TimeElapsed"] <- NA
  # if the last pitch was hit or the last pitch was a different batter
  data[which(data$Strikes == 2 & data$PitchCall == "FoulBall"), "Strike"] <- NA
  
  
  pitchers <- unique(data[which(data$PitcherTeam %in% teams), "Pitcher"])
  
  if(length(which(is.element(teams, c("NOR_TAR", "NOR_TAR2")))) > 0){
    alldata <- pitchtagging(dbGetQuery(tarconn, paste("SELECT * FROM `UNCall` WHERE `PitcherTeam` IN('NOR_TAR', 'NOR_TAR2') AND `Pitcher` IN('", gsub(pattern = "O'", replacement = "O''", ignore.case = F, x = paste(pitchers, collapse = "', '")), "')", sep = "")))
  }
  if(save & type == "pdf"){
    pdf(onefile = T, height = 7, width = 12, file = paste0("/users/micahdaley-harris/desktop/tar/pitcher reports/", mode(data$AwayTeam), " ", "Pitcher Report ", gsub(x = mode(data$Date), pattern = "/", replacement = "."), ".pdf"))
  } else{
    quartz(height = 7, width = 12)
  }
  
  for(pitcher in sort(pitchers)){
    par(mfrow = c(3, 2), mar = c(2, 2, 3, 1))
    #layout(matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7, 7), 3, 6, byrow = TRUE), heights = c(1, 1, 1))
    
    pdata <- data[which(data$Pitcher == pitcher),]
    
    
   
    #### STRIKE PERCENTAGE BY PITCH   #######
    overall.strike <- mean(pdata$Strike, na.rm = T)
    pitch.type.strikes <- pdata %>%
      filter(!is.element(TaggedPitchType, c("BallIntentional", "Undefined"))) %>%
      group_by(TaggedPitchType) %>%
      dplyr::summarise(Strike = mean(Strike, na.rm = T), n = n()) %>%
      arrange(desc(n)) %>%
      as.data.frame()
    
    pts <- rbind(data.frame(TaggedPitchType = "All", Strike = overall.strike, n = nrow(pdata)), pitch.type.strikes)
    bp <- barplot(100 * pts[,2], names.arg = pts[,1], xlab = "Pitch Type", ylab = "Strike %", main = paste(psplit(pitcher), "Strike % by Pitch Type"), col = cols)
    text(bp, pts[,1], labels = paste(round(pts[,"Strike"] * 100), "%\n", pts[,3], "thrown"), pos = 3, xpd = NA)
    abline(h = c(55, 60, 65))
    
    
    #### STRIKE PERCENTAGE BY COUNT   #######
    
    count.strikes <- pdata %>%
      filter(!is.element(TaggedPitchType, c("BallIntentional"))) %>%
      group_by(Count) %>%
      dplyr::summarise(Strike = mean(Strike, na.rm = T), n = n()) %>%
      as.data.frame()
    
    rownames(count.strikes) <- count.strikes[,1]
    count.strikes <- count.strikes[c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2"),]
    
    bp2 <- barplot(100 * count.strikes[,2], names.arg = count.strikes[,1], xlab = "Count", ylab = "Strike %", main = paste(psplit(pitcher), "Strike % by Count"), col = cols)
    text(bp2, count.strikes[,1], labels = count.strikes[,"n"], pos = 3, xpd = NA)
    abline(h = c(55, 60, 65))
    
    
    
    #### RELEASE POINT   #######
    #releasepointgraph(data1 = alldata[which(alldata$Pitcher == pitcher),], data2 = pdata, pitcher = pitcher, axistitles = F)
    releasepointgraph(data1 = pdata, pitcher = pitcher, ball.strike = T, axistitles = F)
    
    
    #### VELO BY INNING   #######
    velo.by.inning <- pdata %>%
      dplyr::filter(!is.element(TaggedPitchType, c("BallIntentional", "Undefined"))) %>%
      dplyr::group_by(Inning, TaggedPitchType) %>%
      dplyr::summarise(RelSpeed = mean(RelSpeed, na.rm = T)) %>%
      dplyr::arrange(Inning, desc(RelSpeed)) %>%
      as.data.frame()
    
    plot(0, 0, type = "n", xlim = c(1, max(velo.by.inning$Inning)), xlab = "Inning", ylab = "Velo (mph)", ylim = c(60, 100), main = paste(psplit(pitcher), "Velo by Inning"))
    pitch.color <- data.frame(TaggedPitchType = "Undefined", PitchColor = "black")
    colind <- 1
    for(pitch in unique(velo.by.inning$TaggedPitchType)){
      points(velo.by.inning[which(velo.by.inning$TaggedPitchType == pitch),c("Inning", "RelSpeed")], type = "o", col = cols[colind], pch = 16)
      pitch.color <- rbind(pitch.color, data.frame(TaggedPitchType = pitch, PitchColor = cols[colind]))
      colind <- colind + 1
    }
    
    firstinn <- velo.by.inning[which(velo.by.inning$Inning == min(velo.by.inning$Inning)),]
    lastinn <- velo.by.inning[which(velo.by.inning$Inning == max(velo.by.inning$Inning)),]
    
    max.vel <- pdata[which.max(pdata$RelSpeed),]
    
    points(max.vel$Inning, max.vel$RelSpeed, pch = 4, col = "pink")
    
    text(lastinn$Inning - .3, lastinn$RelSpeed, round(lastinn$RelSpeed, digits = 1), pos = 3)
    if(mode(firstinn$Inning) != mode(lastinn$Inning)){
      text(firstinn$Inning + .1, firstinn$RelSpeed, round(firstinn$RelSpeed, digits = 1), pos = 3)
    }
    
    
    text(max.vel$Inning, max.vel$RelSpeed, round(max.vel$RelSpeed, digits = 1), pos = 3)
    
    legend("bottomleft", legend = c(as.character(pitch.color[c(2:nrow(pitch.color)), "TaggedPitchType"]), "Max Velo"), col = as.character(c(pitch.color[c(2:nrow(pitch.color)), "PitchColor"]), "pink"), pch = c(rep(16, nrow(pitch.color) - 1), 4), pt.cex = 0.8, cex = 0.5)
    
    ####  Hot/Cold Strike Zone  ####
    # L/R color by pitch type, show strike 3s and extra base hits
    
    strike3dat <- pdata[which(pdata$Strikes == 2 & pdata$PitchCall %in% c("StrikeCalled", "StrikeSwinging")),]
    strike3dat <- merge(strike3dat, pitch.color, by = "TaggedPitchType", all.x = T, all.y = F)
    xbhdat <- pdata[which(pdata$PlayResult %in% c("Double", "Triple", "HomeRun")),]
    xbhdat <- merge(xbhdat, pitch.color, by = "TaggedPitchType", all.x = T, all.y = F)
    strike3dat$PlateLocSide <- strike3dat$PlateLocSide - 2
    xbhdat$PlateLocSide <- xbhdat$PlateLocSide + 2

    plot(c(0, 0), c(-1, 7), type = "l", xlim = c(-4, 4), ylim = c(0, 6), xlab = "", ylab = "", axes = F, main = paste(psplit(pitcher), "Pitcher View", "\nStrike 3s                    Extra Base Hits"))
    
    axis(1, at = c(-4:-1, 1:4), labels = c(-2:1, -1:2))
    axis(2, at = c(0:6), labels = c(0:6))
    
    legend("topleft", legend = c("Called Strike 3", "Swinging Strike 3"), pch = c(8, 4), pt.cex = 0.8, cex = 0.5)
    rect(-8.5 / 12 - 2, 1.75, 8.5 / 12 - 2, 3.43, border = "hotpink", col = NULL, lwd = 2)
    
    legend("topright", legend = c("Home Run", "Double/Triple"), pch = c(16, 15), pt.cex = 0.8, cex = 0.5)
    rect(-8.5 / 12 + 2, 1.75, 8.5 / 12 + 2, 3.43, border = "hotpink", col = NULL, lwd = 2)
    
    legend("bottom", cex = 0.5, legend = pitch.color[c(2:nrow(pitch.color), 1), "TaggedPitchType"], pch = 16, col = c(as.character(pitch.color[c(2:nrow(pitch.color), 1), "PitchColor"])), pt.cex = 1)
    
    points(strike3dat$PlateLocSide, strike3dat$PlateLocHeight, cex = 1.4, col = as.character(strike3dat$PitchColor), pch = ifelse(strike3dat$PitchCall == "StrikeCalled", 8, 4))
    
    if(length(which(strike3dat$BatterSide == "Left")) > 0){
      text(strike3dat[which(strike3dat$BatterSide == "Left"), c("PlateLocSide")], strike3dat[which(strike3dat$BatterSide == "Left"), c("PlateLocHeight")] + 0.1, "LHB", cex = 0.8)
    }
    
    points(xbhdat$PlateLocSide, xbhdat$PlateLocHeight, cex = 1.4, col = as.character(xbhdat$PitchColor), pch = ifelse(xbhdat$PitchCall == "HomeRun", 16, 15))
    
    if(length(which(xbhdat$BatterSide == "Left")) > 0){
      text(xbhdat[which(xbhdat$BatterSide == "Left"), c("PlateLocSide")], xbhdat[which(xbhdat$BatterSide == "Left"), c("PlateLocHeight")] + 0.1, "LHB", cex = 0.8)
    }
    
    
    ####  Pace/Rhythm   ####
    pdata <- pdata[order(pdata$PitchNo, decreasing = F),]
    pdata <- cbind(pdata, PitchCount = c(1:nrow(pdata)))
    new.inning <- pdata %>%
      group_by(Inning) %>%
      summarise(PitchNo = min(PitchNo, na.rm = T)) %>%
      as.data.frame()
    
    if(length(which(!is.na(pdata$TimeElapsed))) > 5){
      scatter.smooth(pdata$PitchCount, pdata$TimeElapsed, ylim = c(0, 60), lpars = list(col = "red"), xlab = "Pitch Count", ylab = "Seconds Between Pitches", main = paste(psplit(pitcher), "Pace/Rhythm"))
    } else{
      plot(pdata$PitchCount, pdata$TimeElapsed, ylim = c(0, 60), xlab = "Pitch Count", ylab = "Seconds Between Pitches", main = paste(psplit(pitcher), "Pace/Rhythm"))
    }
    
    new.inn <- pdata[which(pdata$PitchNo %in% new.inning$PitchNo), "PitchCount"]
    abline(v = new.inn, col = "blue")
    text(new.inn + 5, rep(max(pdata$TimeElapsed, na.rm = T) - 10, length(new.inn)) + rnorm(length(new.inn), mean = 4), paste("Inning:", c(1:length(new.inn))), col = "blue")
    
    # if(save){
    #   Sys.sleep(3.5)
    #   quartz.save(file = paste("/Users/micahdaley-harris/Desktop/TAR/Pitcher Reports/", pitcher, " ", gsub(x = pdata[1, "Date"], pattern = "/", replacement = "."), "Game Summary.", type, sep = ""), type = type)
    # }
    
  }
  
  if(save){
    dev.off()
  }
  
  #graphics.off()
  
}


pitcher.report.by.batter <- function(data){
  
  #data <- slide.trackman.columns(data)
  quartz(width = 12, height = 7)
  
  data <- data[which(data$PitchCall != "Undefined" & data$PitcherTeam %in% c("NOR_TAR", "NOR_TAR2")),]
  for(p in unique(data$Pitcher)){
    num <- 0
    par(mfrow = c(2, 3), mar = c(0, 0, 3, 0))
    pdata <- data[which(data$Pitcher == p),]
    for(b in unique(pdata$Batter)){
      if(num %% 6 == 0 & num > 0){
        Sys.sleep(3)
        quartz.save(file = paste("/Users/micahdaley-harris/Desktop/TAR/Pitcher Reports/", p, num, ".pdf", sep = ""), type = "pdf")
        
      }
      plot.pitches(data = pdata[which(pdata$Batter == b),], plot.title = paste(p, "vs.", b), p.view = T, star.fb = T, shape = "ABNum", give.axes = F)
      num <- num + 1
      
    }
    if(num %% 6 != 1){
      Sys.sleep(3)
      quartz.save(file = paste("/Users/micahdaley-harris/Desktop/TAR/Pitcher Reports/", p, num, ".pdf", sep = ""), type = "pdf")
    }
  }
  graphics.off()
}

# data <- dbGetQuery(tarconn, "SELECT * FROM `alltm` WHERE `PitcherTeam` = 'NOR_TAR' AND RIGHT(`Date`, 1) = 8")

#library(plotly)

inning.velo <- function(data, p = mode(data$Pitcher)){
  require(ggplot2); require(plotly)
  require(lubridate)
  require(assertthat)
  
  per90 <- tibble::tribble(
    ~PitcherThrows, ~AutoPitchType,    ~RelSpeed,
    "Left",           "BB", 82.979781325, 
    "Left",           "CH",  84.26995735, 
    "Left",           "FB",   91.1247524, 
    "Right",           "BB", 84.315738395, 
    "Right",           "CH",  84.20698938,  
    "Right",           "FB", 92.826312234 
  )
  
  data <- pitchtagging(data[which(data$Pitcher == p),])
  
  data$Date <- gsub(x = data$Date, pattern = "/1(7|8)$", replacement = "/201\\1")
  if(!any(grepl(x = data$Date, pattern = "-"))){
    data$Date <- mdy(data$Date)
  }

  
  ag <- data %>%
    filter(!is.na(RelSpeed) & TaggedPitchType != "Undefined") %>%
    group_by(Pitcher, TaggedPitchType, Inning) %>%
    summarise(AvgVelo = mean(RelSpeed), SdVelo = sd(RelSpeed), MaxVelo = max(RelSpeed)) %>%
    mutate(Lower = AvgVelo - SdVelo, Upper = AvgVelo + SdVelo) %>%
    mutate_if(is.numeric, funs(round(., digits = 1)))
  
  per90 <- cbind(per90[which(per90$PitcherThrows == mode(data$PitcherThrows) & per90$AutoPitchType == "FB"),], Inning = min(ag$Inning, na.rm = T))
  
  g <- ggplot(data = ag, aes(Inning, AvgVelo,
      text = paste(TaggedPitchType,
                   "<br>Avg:", AvgVelo,
                   "<br>Inning:", Inning,
                   "<br>Max", MaxVelo,
                   "<br>Range:", paste(Lower, Upper, sep = " - ")))) +
      geom_point(aes(y = AvgVelo, color = TaggedPitchType)) +
      geom_ribbon(aes(x = Inning, ymin = Lower, ymax = Upper, colour = TaggedPitchType)) +#, color = "grey50"
      geom_line(aes(y = AvgVelo)) +
      geom_point(mapping = aes(Inning, MaxVelo, color = "Max"), data = function(x)x[which(x$TaggedPitchType == "FB"),]) +
      geom_hline(aes(yintercept = RelSpeed),  data = per90, linetype = "dashed", color = "black", show.legend = F) +
      labs(xlab = "Inning", ylab = "Velo", title = paste(p, "Velo by Inning", sep = " - ")) +
      scale_y_continuous(limits = c(70,100)) +
#      scale_x_continuous(labels = function(x) format(x, "%d-%b")) +
      #facet_wrap( ~ Pitcher, ncol = 4) +
      scale_colour_discrete(name = "Velo Type") +
      theme(legend.position = "bottom") +
      ggtitle("Velo by Inning")
  g <- g + geom_text(aes(x = Inning, y = RelSpeed + 1, label = paste(ifelse(AutoPitchType == "BB", "\n\n", ""), "Elite", AutoPitchType)), data = per90, show.legend = F, inherit.aes = F)

    gg <- ggplotly(g, tooltip = "text", source = 'velo') #%>% add_segments(x = as.Date("2018-02-18"), y = 92.8, yend = 92.8, xend = as.Date("2018-06-28"))
    

  return(gg)
  
}


game.by.game.velo <- function(data, p = mode(data$Pitcher), min.year = this.year, min.month = 2, min.day = 1, no.scrims = T){
  require(ggplot2); require(plotly)
  require(lubridate)
  require(assertthat)
  
  per90 <- tibble::tribble(
    ~PitcherThrows, ~AutoPitchType,    ~RelSpeed,
    "Left",           "BB", 82.979781325, 
    "Left",           "CH",  84.26995735, 
    "Left",           "FB",   91.1247524, 
    "Right",           "BB", 84.315738395, 
    "Right",           "CH",  84.20698938,  
    "Right",           "FB", 92.826312234 
  )
  
  if(is.data.frame(data) && !is.element("yyyymmdd", names(data))){
    data <- pitchtagging(data[which(data$Pitcher == p),])
    data$yyyymmdd <- gsub(x = data$Date, pattern = "/1(7|8)$", replacement = "/201\\1")
    if(!any(grepl(x = data$yyyymmdd, pattern = "-"))){
      data$yyyymmdd <- mdy(data$yyyymmdd)
      validate_that(sum(is.na(data$yyyymmdd)) == 0)
    }
  }
  
  assert_that(min.year > 2012 & min.year <= this.year & min.month %in% c(1:12) & min.day %in% c(1:31))
  
  ag <- data %>%
    filter(!is.na(RelSpeed) & TaggedPitchType != "Undefined" & yyyymmdd >= paste(min.year, paste0(str_pad(string = c(min.month, 1), width = 2, side = "left", pad = 0), collapse = "-"), sep = "-") & !is.element(BatterTeam, vector.ifelse(no.scrims, list(c("NOR_TAR", "NOR_TAR2")), list(c())))) %>%
    group_by(Pitcher, TaggedPitchType, yyyymmdd) %>%
    summarise(AvgVelo = mean(RelSpeed), SdVelo = sd(RelSpeed), MaxVelo = max(RelSpeed), n = n()) %>%
    mutate(Lower = AvgVelo - SdVelo, Upper = AvgVelo + SdVelo, yyyymmdd = as.Date(yyyymmdd)) %>%
    mutate_if(is.numeric, funs(round(., digits = 1)))
  
  #per90 <- cbind(per90[which(per90$PitcherThrows == mode(data$PitcherThrows) & per90$AutoPitchType == "FB"),], yyyymmdd = min(ag$Date, na.rm = T))
  
  g <- ggplot(data = ag, aes(yyyymmdd, AvgVelo,
                             text = paste(TaggedPitchType,
                                          "<br>Avg:", AvgVelo,
                                          "<br>Date:", yyyymmdd,
                                          "<br>Max", MaxVelo,
                                          "<br>Range:", paste(Lower, Upper, sep = " - "),
                                          "<br># Thrown:", n))) +
    geom_point(aes(y = AvgVelo, color = TaggedPitchType)) +
    geom_ribbon(aes(x = yyyymmdd, ymin = Lower, ymax = Upper, colour = TaggedPitchType)) +#, color = "grey50"
    geom_line(aes(y = AvgVelo)) +
    geom_point(mapping = aes(yyyymmdd, MaxVelo, color = "Max"), data = function(x)x[which(x$TaggedPitchType == "FB"),]) +
    #geom_hline(aes(yintercept = RelSpeed),  data = per90, linetype = "dashed", color = "black", show.legend = F) +
    labs(xlab = "Date", ylab = "Velo", title = paste(p, "Velo by Date", sep = " - ")) +
    scale_y_continuous(limits = c(70,100)) +
    scale_x_date(labels = function(x) format(x, "%d-%b")) +
    #facet_wrap( ~ Pitcher, ncol = 4) +
    scale_colour_discrete(name = "Velo Type") +
    theme(legend.position = "bottom") +
    ggtitle("Velo by Game")
 # g <- g + geom_text(aes(x = Date, y = RelSpeed + 1, label = paste(ifelse(AutoPitchType == "BB", "\n\n", ""), "Elite", AutoPitchType)), data = per90, show.legend = F, inherit.aes = F)
  
  gg <- ggplotly(g, tooltip = "text", source = 'velo') #%>% add_segments(x = as.Date("2018-02-18"), y = 92.8, yend = 92.8, xend = as.Date("2018-06-28"))
  
  
  return(gg)
  
}

per90 <- tibble::tribble(
  ~PitcherThrows, ~AutoPitchType,    ~RelSpeed,    ~HorzBreak, ~InducedVertBreak,
  "Left",           "BB", 82.979781325,   14.66555425,      12.607887495,
  "Left",           "CH",  84.26995735, -6.5539927415,      19.314852645,
  "Left",           "FB",   91.1247524, -4.3954593557,      22.461836257,
  "Right",           "BB", 84.315738395, -0.1086137713,      12.131499649,
  "Right",           "CH",  84.20698938,   19.66848827,       18.06402667,
  "Right",           "FB", 92.826312234,  16.603804464,      22.461319432,
  "Left",           "BB",  70.83556362, -0.6410182055,     -10.957964565,
  "Left",           "CH", 74.768669135, -21.158667225,      0.9299778005,
  "Left",           "FB",  85.20353539, -17.705232998,      8.9347865799,
  "Right",           "BB", 73.067942162,  -15.71018453,     -10.235367435,
  "Right",           "CH",  75.97462283,   6.167286297,      -0.761111762,
  "Right",           "FB",  85.89435062,   2.897173184,       8.244466326
)

movement.plot <- function(data, remove.undefined = T){
  
  per90$Max <- rep(c(T, F), each = 6)
  lefty <- mode(data$PitcherThrows) == "Left"
  per90 <- per90[which(per90$PitcherThrows == ifelse(lefty, "Left", "Right")),]
  if(remove.undefined)data <- data[which(data$TaggedPitchType != "Undefined"),]
  g <- ggplot(data, aes(HorzBreak, InducedVertBreak, color = TaggedPitchType,
                        text = paste("Pitch =", TaggedPitchType,
                                     "<br>Velo =", round(RelSpeed),
                                     "<br>Horz Mvmnt =", round(HorzBreak, digits = 1),
                                     "<br>Vert Mvmnt =", round(InducedVertBreak, digits = 1)))) +
    geom_point() +
    #geom_text(aes(x = per90$HorzBreak, y = per90$InducedVertBreak, label = per90$AutoPitchType)) +
    scale_x_continuous(limits = c(-25, 25)) +
    scale_y_continuous(limits = c(-25, 25)) +
    labs(x = "Horizontal Movement (Pitcher View in inches)", y = "Vertical Movement (in inches)", title = "Pitch Movement", color = "Pitch Type")
  gg <- ggplotly(g, tooltip = "text", source = 'mvmnt') %>% 
    add_text(x = per90$HorzBreak, y = per90$InducedVertBreak, name = "Elite", text = per90$AutoPitchType, textfont = list(color = "pink")) %>%
    add_text(x = ~c(-25, 0, 25, 0), y = ~c(0, -25, 0, 25), text = c(ifelse(lefty, "Run", "Cut"), "Sink", ifelse(!lefty, "Run", "Cut"), "Ride"), showlegend = F, hoverinfo = "none") 
  return(gg)
}


movement.plot.plate <- function(data, lhb = F, lhp = F, p.view = T, dragmode = "lasso", fixaxes = T){
  
  
  
  
  #if(is.data.frame(data) && any(!is.element(c("mvmt.x", "mvmt.z"), colnames(data)))) data <- mutate(data, mvmt.x = (HorzBreak / 12) * ifelse(p.view, 1, -1), mvmt.z = (2.525 + InducedVertBreak / 12)) 
  
  make.elite.shape <- function(x, pside = "Right", pitch = "FB", color = "blue"){
    
    list(type = "rect", line = list(color = color, opacity = .2), name = pitch,
         x0 = min(x[which(x$PitcherThrows == pside & x$AutoPitchType == pitch),"HorzBreak"]) / 12, 
         x1 = max(x[which(x$PitcherThrows == pside & x$AutoPitchType == pitch),"HorzBreak"]) / 12, 
         y0 = 2.525 + min(x[which(x$PitcherThrows == pside & x$AutoPitchType == pitch),"InducedVertBreak"]) / 12, 
         y1 = 2.525 + max(x[which(x$PitcherThrows == pside & x$AutoPitchType == pitch),"InducedVertBreak"]) / 12, xref = "x", yref = "y")
  }
  
  plot_ly(data, x = ~mvmt.x, y = ~mvmt.z, type = "scatter", color = ~TaggedPitchType, mode = "markers", hoverinfo = "none") %>%
    layout(title = "<b>Pitch Movement</b><br><i>(Plate location of a pitch that starts middle-middle)</i>",
           xaxis = list(title = '', range = c(-3.25, 3.25), visible = F, fixedrange = fixaxes), yaxis = list(title = '', range = c(-0.5, 5), showgrid = F, visible = F, fixedrange = fixaxes),
           images = list(
             list(
               source = raster2uri(readPNG("data/NCAA Baseball.png")), xref = "x", yref = "y", x = -gap, y = 2.525 - gap,
               sizex = 2 * gap, sizey = 2 * gap, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "bottom"
             ),
             list(
             source = raster2uri(readPNG(paste0("data/", ifelse(lhb, "L", "R"), "HB ", ifelse(p.view, "P View ", ""), "Real Zone.png"))),
             xref = "x", yref = "y",  x = -4, y = -.5, sizex = 8, sizey = 5.5, #x = 1.5, y = 0, sizex = 1.5, sizey = 5
             sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below"
             )
           ),
           shapes = list(
             make.elite.shape(x = per90, pside = ifelse(lhp, "Left", "Right"), pitch = "FB", color = "blue"),
             make.elite.shape(x = per90, pside = ifelse(lhp, "Left", "Right"), pitch = "CH", color = "green"),
             make.elite.shape(x = per90, pside = ifelse(lhp, "Left", "Right"), pitch = "BB", color = "red")

           ),
           dragmode = dragmode)
  
}


control.plot <- function(data){
  
  require(reshape)
  require(facetscales)
  ag <- data %>%
    filter(PitchCall != "Undefined") %>%
    mutate(Strike = (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "InPlay")) | (PitchCall == "FoulBall" & Strikes < 2)) %>%
    group_by(yyyymmdd, Pitcher, Inning, PAofInning) %>%
    summarise(`1st Pitch K` = any(Strike & Strikes == 0 & Balls == 0), `2 Pitches 2 Ks` = any(Balls == 0 & Strikes == 2), `4 Pitches or Less` = as.numeric(n() <= 4), `K%` = "Strikeout" %in% KorBB, `BB%` = "Walk" %in% KorBB) %>%
    mutate(`K:BB` = ifelse(`BB%` == 0, 5, `K%` / `BB%`)) %>%
    group_by(yyyymmdd, Pitcher) %>%
    summarise_all(mean) %>%
    select(-c(Inning, PAofInning)) %>%
    as.data.frame()
  
  ag$yyyymmdd <- as.Date(ag$yyyymmdd)
  df.melted <- melt(ag[which(substr(ag$yyyymmdd, 4, 4) %endswith% "8"),-2], id = "yyyymmdd")
  
  scales_y <- list(`1st Pitch K` = scale_y_continuous(limits = c(0, 1), position = "right"),
                   `2 Pitches 2 Ks` = scale_y_continuous(limits = c(0, 1), position = "right"),
                   `4 Pitches or Less` = scale_y_continuous(limits = c(0, 1), position = "right"),
                   `K%` = scale_y_continuous(limits = c(0, 1), position = "right"),
                   `BB%` = scale_y_continuous(limits = c(0, 1), position = "right"),
                   `K:BB` = scale_y_continuous(limits = c(0, 5), position = "right"))
  
  g <- ggplot(df.melted, aes(yyyymmdd, value, color = variable,
                             text = paste0("Date: ", yyyymmdd,
                                          "<br>", variable, ": ", round(value, digits = 2)))) +
    geom_point() +
    facet_grid_sc(variable ~ ., scales = list(y = scales_y), switch = "y") +
    scale_x_date(labels = function(x) format(x, "%d-%b")) +
    labs(x = "Date", y = "") +
    guides(colour = guide_legend(title = "Stat", keywidth = .8))
    
  return(ggplotly(g, tooltip = "text", source = 'strikes'))
}
# data <- savedata
# data <- data[which(data$Pitcher == "Baum, Tyler" & data$Date %endswith% "8"),]
# data <- data[c(1:769),]
pace.graph <- function(data, after = T, pitch.results = c("BallCalled", "StrikeCalled", "StrikeSwinging"), remove.new.batters, mintime = 0, maxtime = 60){
  
  data$Time <- paste(gsub(data$Date, pattern = "([0-9]+)/([0-9]+)/(20|)([0-9]{2})", replacement = "\\2-\\1-\\4"), data$Time) ## note there will be mistakes if some pitches were thrown before noon/midnight and some after
  
  data <- cbind(data, Strike = as.numeric(!is.element(data$PitchCall, c("BallCalled", "BallIntentional", "HitByPitch"))),
                Count = paste(data$Balls, data$Strikes, sep = "-"), TimeElapsed = c(NA, as.numeric(difftime(data[c(2:nrow(data)), "Time"], data[c(1:(nrow(data) - 1)), "Time"]))))
  data[which(!is.element(data$PitchCall, pitch.results) | c(data$Batter[-c(1)] != data$Batter[-c(nrow(data))], F)) + 1, "TimeElapsed"] <- NA
  
  
  data <- data[order(data$PitchNo, decreasing = F),]
  for(game in unique(data$GameID)){
    data[which(data$GameID == game), "PitchCount"] <- c(1:sum(data$GameID == game))
  }
  
  gg <- ggplot(data, aes(PitchCount, TimeElapsed), 
               text = paste("Time Between =", TimeElapsed,
                            "<br>Pitch # =", PitchNo,
                            "<br>Pitch Result", gsub(x = PitchCall, pattern = "([a-z])([A-Z])", replacement = "\\1 \\2"))) +
    geom_smooth() +
    geom_point() +
    scale_y_continuous(limits = c(0, 60))
  
  
  pred <- expand.grid(PlateLocSide = seq(-2, 2, by = .1), PlateLocHeight = seq(0.5, 4.5, by = .1))
  library(tidyr)
  
  new.inning <- data %>%
    group_by(Inning) %>%
    summarise(PitchNo = min(PitchNo, na.rm = T)) %>%
    as.data.frame()
  
  if(length(which(!is.na(data$TimeElapsed))) > 5){
    scatter.smooth(data$PitchCount, data$TimeElapsed, ylim = c(0, 60), lpars = list(col = "red"), xlab = "Pitch Count", ylab = "Seconds Between Pitches", main = paste(psplit(pitcher), "Pace/Rhythm"))
  } else{
    plot(data$PitchCount, data$TimeElapsed, ylim = c(0, 60), xlab = "Pitch Count", ylab = "Seconds Between Pitches", main = paste(psplit(pitcher), "Pace/Rhythm"))
  }
  
  new.inn <- data[which(data$PitchNo %in% new.inning$PitchNo), "PitchCount"]
  abline(v = new.inn, col = "blue")
  text(new.inn + 5, rep(max(data$TimeElapsed, na.rm = T) - 10, length(new.inn)) + rnorm(length(new.inn), mean = 4), paste("Inning:", c(1:length(new.inn))), col = "blue")
  
}

heatmap.pitch.freq <- function(data){
  ag <- data[c(1:12),] %>%
    mutate(PLS = round(PlateLocSide, digits = 1), PLH = round(PlateLocHeight, digits = 1)) %>%
    complete(PLS = seq(-2, 2, by = .1), PLH = seq(0.5, 4.5, by = .1)) %>%
    group_by(PLS, PLH) %>%
    summarise(x = sum(value, na.rm = T))
  
  sm <- loess("x ~ PLS + PLH", data = ag, span = .05)
  
  #gg <- ggplot(as.data.frame(cbind(PLS = ag$PLS, PLH = ag$PLH, x = ag$x)), aes(PLS, PLH)) +   
  gg <- ggplot(as.data.frame(cbind(sm$x, x = sm$fitted)), aes(PLS, PLH)) + 
    geom_raster(aes(fill = x), interpolate = F) + 
    scale_fill_gradientn(colours = rev(rainbow(3, alpha = 1)), labels = NULL, name = "frequency") + 
    labs(x = "", y = "", title = "") +
    geom_rect(aes(xmin = min(strikezone$x), xmax = max(strikezone$x), ymin = min(strikezone$z), ymax = max(strikezone$z)), color = "black", alpha = 0)
  
  g <- ggplotly(gg, tooltip = c())
}





# game <- read.csv(file = paste0(tar.path, "/TAR Database/Scrimmage 9.2.18.csv"), stringsAsFactors = F)
render_pitcher_report <- function(game, pitchers = unique(game[which(game$PitchCall != "Undefined"), "Pitcher"]), teams = c("NOR_TAR", "NOR_TAR2")){
  
  graphics.off()
  
  game <- slide.trackman.columns(game)
  
  game <- game[which(game$PitchCall != "Undefined"),]
  # if(sum(is.element(params$teams, c("NOR_TAR", "NOR_TAR2")))) > 0){
  #   game <- pitchtagging(game)
  # }
  #
  # if(sum(!is.element(teams, c("NOR_TAR", "NOR_TAR2"))) > 0){
  #   game[which(!is.element(game$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))),"TaggedPitchType"] <- generic.pitch.tag(game[which(!is.element(game$PitcherTeam, c("NOR_TAR", "NOR_TAR2"))),])$AutoPitchType
  # }
  game$Time <- paste("1-1-1", game$Time) ## note there will be mistakes if some pitches were thrown before noon/midnight and some after
  game <- game[order(game$PitchNo, decreasing = F),]
  game <- cbind(game, Strike = as.numeric(!is.element(game$PitchCall, c("BallCalled", "BallIntentional", "HitByPitch"))),
                Count = paste(game$Balls, game$Strikes, sep = "-"), TimeElapsed = c(NA, as.numeric(difftime(game[c(2:nrow(game)), "Time"], game[c(1:(nrow(game) - 1)), "Time"]))))
  game[which(!is.element(game$PitchCall, c("BallCalled", "StrikeCalled", "StrikeSwinging")) | c(game$Batter[-c(1)] != game$Batter[-c(nrow(game))], F)) + 1, "TimeElapsed"] <- NA
  # if the last pitch was hit or the last pitch was a different batter
  game[which(game$Strikes == 2 & game$PitchCall == "FoulBall"), "Strike"] <- NA
  
  thedate <- gsub(pattern = "/", replacement = "-", x = mode(game$Date))
  main.dir <- "/users/micahdaley-harris/desktop/tar/micahdh.github.io"
  if(!dir.exists(paste0(main.dir, "/Post_Game_Reports/Pitcher/", thedate))) dir.create(paste0(main.dir, "/Post_Game_Reports/Pitcher/", thedate))
  output.dir <- paste0(main.dir, "/Post_Game_Reports/Pitcher/", thedate)
  
  
  savegame <- game
  for(p in na.omit(pitchers)){
    rmarkdown::render(input = "/users/micahdaley-harris/desktop/tar/chillmedia/Pitcher_Report_2019.Rmd", params = list(game = savegame[which(savegame$Pitcher == p),], allplayers = sort(unique(savegame$Pitcher))), output_file = paste0(output.dir, "/", p, " Pitcher Report ", gsub(mode(game$Date), pattern = "/", replacement = "-"), ".html"))
  }
  
  
}


render_pitch_specs_page <- function(pitchers){
  
  # alldata <- data %>%
  #   filter(PitchCall != "Undefined" & !is.na(HorzBreak)) %>%
  #   mutate(mvmt.x = (HorzBreak / 12), mvmt.z = (2.525 + InducedVertBreak / 12),
  #          PitchResult = ifelse(PitchCall != "InPlay", PitchCall, ifelse(PlayResult != "Out", PlayResult, paste(HitType, PlayResult))))
  # 
  # for(p in unique(data$Pitcher)){
  #   f <- paste0("/users/micahdaley-harris/desktop/tar/micahdh.github.io/Pitch_Specs/", gsub(p, pattern = " ", replacement = "_"), "_Pitch_Specs.html")
  #   if(!file.exists(f)) write(x = c(), file = f)
  # }
  
  #uncall <- read.csv(file = "data/uncall.csv", stringsAsFactors = F)
  uncall <- read.csv(file = "UNC P Data Tagged.csv", stringsAsFactors = F)
  
  for(p in unique(pitchers)[1]){
    rmarkdown::render(input = "/users/micahdaley-harris/desktop/tar/chillmedia/Pitch_Specs_Page.Rmd", params = list(data = uncall, pitcher = p),
                      output_file = paste0(gsub(p, pattern = " ", replacement = "_"), "_Pitch_Specs.html"), output_dir = "/users/micahdaley-harris/desktop/tar/micahdh.github.io/Pitch_Specs")
  }
  
  
}


render_pitcher_app <- function(pitchers, coach = T, videodrops = T){
  if(coach) rmarkdown::render("Pitching_Coach.Rmd", output_file = "Pitching_Coach.html", output_dir = paste0(main.dir, "/Pitchers"))
  if(missing(pitchers)) pitchers <- ps$Name[-c(1, 18)]
  alldata <- data
  
  if(videodrops){
    pitcherx <- data9 %>%
      filter(yyyymmdd > "2019-02-17" & PitchCall != "Undefined" & Scrimmage == "Season" & endsWith(Date, "9") & startsWith(PitcherTeam, "NOR_TAR")) %>%
      mutate(ab_string = paste(paste("Inning", Inning, Batter), AB_Result, sep = " - "), pitch_string = paste(paste("Pitch", PitchofPA), Pitch_Result, sep = " - "),
             TV_video_link = paste0("https://s3.amazonaws.com/publicuncsynergyvideo/", GameID, "/", key, ".mp4"),
             Side_video_link = paste0("https://s3.amazonaws.com/unchitterhighspeedvideo/", GameID, "_", if_else(PitcherThrows == "Left", "RF/", "LF/"), key, ".mp4"),
             Front_video_link = paste0("https://s3.amazonaws.com/unchitterhighspeedvideo/", GameID, "_", if_else(PitcherThrows == "Right", "RF/", "LF/"), key, ".mp4")
      ) %>%
      arrange(desc(yyyymmdd), Inning, PitchofPA) %>%
      select(Player = Pitcher, Game, ab_string, pitch_string, TV_video_link, Side_video_link, Front_video_link)
  }
  
  for(p in pitchers){
    print(p)
    pdata <- alldata %>%
      filter(Pitcher == p)
    print(nrow(pdata))
    rmarkdown::render("Pitcher App.Rmd", output_file = paste0(p, ".html"),
                      output_dir = paste0(main.dir, "/Pitchers"), params = list(pitcher = p, data = pdata))
    if(videodrops) videodropdown(pitcherx %>% filter(Player == p), pitcher = T)
  }
  
}



render_pitcher_app_current <- function(pitchers, coach = F){
  if(coach) rmarkdown::render("Pitching_Coach.Rmd", output_file = "Pitching_Coach.html", output_dir = paste0(main.dir, "/Pitchers"))
  if(missing(pitchers)) pitchers <- ps$Name[-c(1, 9, 13, 17, 18)]
  alldata <- data9
  
  for(p in pitchers){
    print(p)
    pdata <- alldata %>%
      filter(Pitcher == p)
    print(nrow(pdata))
    rmarkdown::render("Pitcher App.Rmd", output_file = paste0(p, ".html"),
                      output_dir = paste0(main.dir, "/Pitchers/2019"), params = list(pitcher = p, data = pdata))
  }
  
}


# fs <- dir(paste0(main.dir, "/Pitchers"))
# setwd(paste0(main.dir, "/Pitchers"))
# pyos <- import("os")
# for(f in fs[which(!endsWith(fs, "html"))][-1]) pyos$rename(paste0(f, "/", f, "_Pitcher_App.html"), paste0(gsub(f, pattern = "_", replacement = " "), ".html")) 


#####################################		Release Point Graphs		#####################################


#	find rms distance from each point to all others
##	select point with min distance
##	get points within 2 sd of that point
##	use ellipsoidhull to get ellipsoid

commasplit <- function(string){
  split <- strsplit(string, ", ", fixed = TRUE)
  
  return(paste(split[[1]][2], split[[1]][1]))
}

releasepointgraph <- function(data1, data2, pitcher = mode(data1$Pitcher), image, ball.strike = F, axistitles = T, ybot = 0, save = F, pitcherview = F){
  
  
  pitchtypes <- c("FB", "FC", "FS", "CH", "CU", "SL", "SI")
  #data1 <- data1[which(data1$Pitcher == pitcher & !is.element(data1$TaggedPitchType, c("Undefined", "IntentionalBall"))),]
  if(!pitcherview){
    data1$RelSide <- data1$RelSide * -1
    if(!missing(data2)){
      data2$RelSide <- data2$RelSide * -1
    }
  }
  
  if(ball.strike){
    data1$PitchCol <- ifelse(data1$PitchCall == "BallCalled", "green", ifelse(data1$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"), "red", NA))
    data1 <- data1[which(!is.na(data1$PitchCol)),]
  }
  
  data1 <- data1[which(data1$Pitcher == pitcher & is.element(data1$TaggedPitchType, c(pitchtypes))),]
  
  pside <- mode(data1$PitcherThrows)
  
  meanx <- mean(data1$RelSide, na.rm = TRUE) # use aggregate for ball.strike
  meanz <- mean(data1$RelHeight, na.rm = TRUE)
  
  xmax <- ifelse(meanx > 0, meanx + 5, meanx - 5) 
  xmin <- ifelse(meanx > 0, meanx - 3, meanx + 3)
  
  plot(0, 0, type = "n", xlim = c(ifelse(xmin < xmax, xmin, xmax), ifelse(xmax > xmin, xmax, xmin)), ylim = c(ybot, 7), xlab = ifelse(axistitles, "Horizontal Release Point (ft.)", ""), ylab = ifelse(axistitles, "Vertical Release Point (ft.)", ""), main = paste(pitcher, "Release Points"))
  
  if(missing(image)){
    imagedir <- paste("data/Release Point Pics/")
    if(paste0(commasplit(pitcher), ".png") %in% dir(imagedir)){
      image <- paste0(imagedir, commasplit(pitcher), ".png")
    } else{
      image <- NULL
    }
    
  }
  
  require(png)
  if(!is.null(image)){
    img <- readPNG(image)
    
    if(pside == "Right"){
      rasterImage(img, meanx, 0, c(meanx + 3.5), meanz)
    } else if(pside == "Left"){
      rasterImage(img, c(meanx - 3.5), 0,  meanx, meanz)
    }
  }
  
  cols <- c("green", "red", "blue", "orange", "brown", "pink", "purple")
  
  colnum <- 1
  alph <- 0.8
  
  if(ball.strike){
    points(data1$RelSide, data1$RelHeight, col = adjustcolor(data1$PitchCol, alpha = 0.5), pch = 16)
    legend("topright", legend = c("Ball", "Strike"), col = c("green", "red"), pch = 16, cex = 0.4, pt.cex = 1)
    # star avg ball, star avg strike
  } else{
    for(pitch in c(unique(data1[which(data1$TaggedPitchType != "Undefined"),"TaggedPitchType"]))){
      #print(c(unique(data1[which(data1$TaggedPitchType != "Undefined"),"TaggedPitchType"])))
      relps <- data1[which(data1$TaggedPitchType == pitch), c("RelSide", "RelHeight")]
      alph <- alph - 0.2
      points(relps[,1], relps[,2], pch = 16, cex = 0.75, col = adjustcolor(cols[colnum], alpha = ifelse(missing(data2), alph, 0.2)))
      colnum <- colnum + 1
    }
    legend("topright", legend = unique(data1$TaggedPitchType), pch = 16, cex = 0.4, pt.cex = 1, col = cols[c(1:colnum)])
  }
  
  if(!missing(data2)){
    colnum <- 1
    data2 <- data2[which(data2$Pitcher == pitcher & !is.element(data2$TaggedPitchType, c("Undefined", "IntentionalBall"))),]
    
    for(pitch in c(unique(data1$TaggedPitchType))){
      relps <- data2[which(data2$TaggedPitchType == pitch), c("RelSide", "RelHeight")]
      points(relps[,1], relps[,2], pch = 16, col = adjustcolor(cols[colnum], alpha = 0.9))
      #points(relps[,1], relps[,2]) was making it dark w/ small screen
      colnum <- colnum + 1
    }
  }
  
  
  
  if(missing(data2)){
    avgdist <- round(c(12 * mean(sqrt((data1$RelSide - meanx)^2 + (data1$RelHeight - meanz)^2))), digits = 1)
  } else{
    avgdist <- round(c(12 * mean(sqrt((data2$RelSide - mean(data2$RelSide, na.rm = TRUE))^2 + (data2$RelHeight - mean(data2$RelHeight, na.rm = TRUE))^2))), digits = 1)
  }
  
  if(!ball.strike){
    text(meanx, 7.5, paste("Avg. Distance from Center:", avgdist, "in."), cex = 0.8)
  }
  
  
  if(save){
    if(missing(data2)){
      quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point.png", sep = ""))
    } else{
      quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point ", gsub("/", ".", unique(data2$Date)), ".png", sep = ""))
    }
  }
  
  
}

releasepointgraph.ly <- function(data1, data2, pitcher = mode(data1$Pitcher), plot.title = paste(pitcher, "Release Points"), image, xvar = "RelSide", yvar = "RelHeight", ball.strike = F, axistitles = T, ybot = 0, save = F, pitcherview = F){
  require(png)
  
  pitchtypes <- c("FB", "FC", "FS", "CH", "CU", "SL", "SI")
  cols <- c("green", "red", "blue", "orange", "brown", "pink", "purple")
  
  data1$BallStrike <- ifelse(data1$PitchCall %in% c("BallCalled", "HitByPitch"), "Ball", ifelse(data1$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"), "Strike", ""))
  
  #### Filter Data  ####
  data <- cbind(data1, Ind = c(1))
  if(!missing(data2)) rbind(data, cbind(data2, Ind = c(2)))
  
  
  if(xvar == "RelSide" & !pitcherview) data[, "RelSide"] <- data[, "RelSide"] * -1
  
  data <- data[which(!is.element(data$PitchCall, c("Undefined", "IntentionalBall")) & data$Pitcher == pitcher & is.element(data$TaggedPitchType, c(pitchtypes))),]
  
  
  ####  Assign Colors ####
  if(ball.strike){
    data$PitchCol <- ifelse(data$PitchCall == "BallCalled", "green", 
                            ifelse(data$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"), "red", NA))
    data$Type <- ifelse(data$PitchCol == "green", "Ball", "Strike")
    data$PitchCol <- adjustcolor(col = data$PitchCol, alpha.f = 0.5 + 0.2 / data$Ind)
  } else{
    data$Type <- data$TaggedPitchType
    data$Both <- paste(data$Type, data$BallStrike, sep = "-")
    pitchcols <- ifelse(data$TaggedPitchType == "FB", "blue", 
                        ifelse(data$TaggedPitchType == "CH", "green", 
                               ifelse(data$TaggedPitchType == "CU", "red", 
                                      ifelse(data$TaggedPitchType == "SL", "orange", "purple"))))
    
    data$PitchCol <- adjustcolor(col = pitchcols, 
                                 alpha.f = 0.7)#0.5 + 0.2 / data$Ind)
  }
  
  data <- data[which(!is.na(data$PitchCol)),]
  
  #### Data Summary ####
  pside <- mode(data[which(data$Ind == 1), "PitcherThrows"])
  
  meanx <- mean(data[which(data$Ind %in% c(1, ifelse(ball.strike, 0, 2))), xvar], na.rm = T) 
  meanz <- mean(data[which(data$Ind %in% c(1, ifelse(ball.strike, 0, 2))), yvar], na.rm = T)
  
  
  
  
  ##### Image ####
  if(missing(image)){
    imagedir <- paste("data/Release Point Pics/")
    if(xvar == "Extension"){
      image <- paste0(imagedir, "/baum extension.png")
    } else if(paste0(commasplit(pitcher), ".png") %in% dir(imagedir)){
      image <- paste0(imagedir, commasplit(pitcher), ".png")
    } else{
      image <- paste0(imagedir, ifelse(pside == "Left", "/Caden O'Brien.png", "/Tyler Baum.png"))
      
    }
    
  }
  
  
  if(!is.null(image)){
    img <- readPNG(image)
    
    if(xvar == "Extension"){
      img.list <- list(source = raster2uri(img), xref = "x", yref = "y", x = meanx - 2, y = 0, sizex = 3.5, sizey = meanz, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below")
    } else if(pside == "Right"){
      img.list <- list(source = raster2uri(img), xref = "x", yref = "y", x = meanx, y = 0, sizex = 3.5, sizey = meanz, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below")
    } else if(pside == "Left"){
      img.list <- list(source = raster2uri(img), xref = "x", yref = "y", x = meanx - 3.5, y = 0, sizex = 3.5, sizey = meanz, sizing = "stretch", xanchor = "left", yanchor = "bottom", layer = "below")
    }
  } else{
    img.list <- list()
  }
  
  
  ####  Plot  ####
  
  p <- plot_ly(data = data, x = ~get(xvar), y = ~get(yvar)) %>%
    add_markers(color = ~BallStrike, opacity = .7, colors = c("Ball" = "green", "Strike" = "red"), symbol = ~Type, name = ~Both, hoverinfo = "text", 
                text = paste("Pitch:", ~TaggedPitchType, "<br>Result:", ~PitchResult, paste0("<br>", ifelse(xvar == "RelSide", "Release Side", xvar), ":"), round(data[,xvar], digits = 2), "feet",
                             "<br>Release Height:", round(data$RelHeight, digits = 2), "feet", "<br>Inning:", data$Inning, "<br>Count:", paste(data$Balls, data$Strikes, sep = "-"))) %>%
    layout(title = plot.title, xaxis = list(title = ifelse(axistitles, "Horizontal Release Point (ft.)", ""), range = range(c(meanx + 5, meanx - 3)), visible = F), 
           yaxis = list(title = ifelse(axistitles, "Vertical Release Point (ft.)", ""), range = c(ybot, 7), visible = F), 
           images = img.list)
  
  
  return(p)
  #### Show Averages  ####
  # if(missing(data2)){
  #   avgdist <- round(c(12 * mean(sqrt((data1$RelSide - meanx)^2 + (data1$RelHeight - meanz)^2))), digits = 1)
  # } else{
  #   avgdist <- round(c(12 * mean(sqrt((data2$RelSide - mean(data2$RelSide, na.rm = TRUE))^2 + (data2$RelHeight - mean(data2$RelHeight, na.rm = TRUE))^2))), digits = 1)
  # }
  # 
  # if(!ball.strike){
  #   text(meanx, 7.5, paste("Avg. Distance from Center:", avgdist, "in."), cex = 0.8)
  # }
  # 
  # 
  # if(save){
  #   if(missing(data2)){
  #     quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point.png", sep = ""))
  #   } else{
  #     quartz.save(file = paste(pitcher, ifelse(ball.strike, "Ball Strike", ""), " Release Point ", gsub("/", ".", unique(data2$Date)), ".png", sep = ""))
  #   }
  # }
  
  
}









makeellipse <- function(data, pitchername){
  data <- data[which(data$Pitcher == pitchername & !is.na(data$RelSide)),]
  
  relpoints <- data[,c("RelSide", "RelHeight")]
  
  alldists <- c()
  for(row in c(1:nrow(relpoints))){
    
    dist <- sum(sqrt(c(relpoints[row, "RelSide"] - relpoints[-row, "RelSide"])^2 + c(relpoints[row, "RelHeight"] - relpoints[-row, "RelHeight"])^2)) / (nrow(relpoints) - 1)
    
    alldists <- append(alldists, dist)
    
  }
  
  
  center <- relpoints[which.min(alldists),]
  
  row <- which.min(alldists)
  
  mindists <- sqrt(c(relpoints[row, "RelSide"] - relpoints[-row, "RelSide"])^2 + c(relpoints[row, "RelHeight"] - relpoints[-row, "RelHeight"])^2)
  
  sd2 <- sd(mindists, na.rm = TRUE) * 0.5
  
  range <- relpoints[which(abs(mean(mindists) - mindists) < sd2),]
  range <- relpoints[order(mindists),]
  
  range <- range[c(1:(0.67 * nrow(range))),]	
  
  #require(ellipse)
  #return(ellipse(range))
  
  require(cluster)
  #elip <- ellipsoidhull(as.matrix(range),)
  
  require(mixtools)
  ellipse(mu = elip[[1]], sigma = elip[[2]])	
}





CRA <- function(data){
  
  dat <- data %>%
    head(., 100) %>%
    applybbexpoutcomes() %>%
    apply.exp.called.strike() %>%
    mutate(LostStrikes = replace(pmin(pmax(0, Exp.Called.Strike), 100) - as.numeric(PitchCall == "StrikeCalled"), !is.element(PitchCall, c("BallCalled", "StrikeCalled")), NA))
  ## balls in play
    # positioning dependent, positioning independent
  
  ## balls/strikes
    # change in count
  
  ## order of hits (w/in inning and not)
  
  
  
  
}

















#### Improvements ####
# show avgs and ranges (star avg ball, star avg strik)
# add mound, plate
# add 3d
# specific pitch result & dist from avg in tooltip

 






# set.seed(12345)
# Date <- seq(as.Date("2010/1/1"), as.Date("2014/1/1"), "week")
# Y <- rnorm(n=length(Date), mean=100, sd=1)
# df <- data.frame(Date, Y)
# 
# df$Year <- format(df$Date, "%Y")
# df$Month <- format(df$Date, "%b")
# df$Day <- format(df$Date, "%d")
# 
# df$MonthDay <- format(df$Date, "%d-%b")
# 
# df$CommonDate <- as.Date(paste0("2000-",format(df$Date, "%j")), "%Y-%j")
# q <- ggplot(data = df,
#        mapping = aes(x = CommonDate, y = Y, shape = Year, colour = Year)) +
#   geom_point() +
#   geom_line() +
#   facet_grid(facets = Year ~ .) +
#   scale_x_date(labels = function(x) format(x, "%d-%b"))
# 
# ggplotly()





### 1. pitch type strike % hist
### 2. count strike % hist
### 3. relese point graph
### 4. velo by inning
### 5. hot/cold strike zone?
### 6. pace/rhythm




# OVERALL STRIKE %
# 
# PITCH TYPE STRIKE %
# 
# K % BY COUNT
# 
# RELEASE POINT DATA
# 
# VELO AVE BY INN
# 
# HOT/COLD STRIKE ZONE
# 
# PACE/RHYTHM