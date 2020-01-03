
shinyServer(function(input, output, session) {

  observe({
    query <- parseQueryString(session$clientData$url_search)
    # url_protocol, 
    # url_hostname, url_port, url_pathname, url_search, url_hash_initial and url_hash 
    print("there are dudes")
    
    print(session$clientData$url_search)
    print(parseQueryString(session$clientData$url_search))
  
    if (!is.null(query$player)) updateSelectInput(session, "player", choices = query$player)
    if(!is.null(query$tabs)) updateTabsetPanel(session, "tabs", selected = query$tabs)
    shinyjs::reset("smoothlocs")
  })
  
  
  progress <- Progress$new()
  progress$set(message = "Loading...")
  updateProgress <- function(detail) progress$inc(amount = 1 / ifelse(input$tabs == "Pitcher", 8, 5), detail = detail)
  
  
  subset.data <- reactive({
    session$clientData$url_search
    #if(input$setting == "Game"){
      data[which(data$BatterSide %in% c(ifelse(input$bside %in% c("All", "LHB"), "Left", "Right"), ifelse(input$bside == "All", "Right", "BLANK")) & data$PitcherThrows %in% c(ifelse(input$pside %in% c("All", "LHP"), "Left", "Right"), ifelse(input$pside == "All", "Right", "BLANK")) & 
                                        data$yyyymmdd > input$daterange[1] & data$yyyymmdd < input$daterange[2] & (data$TaggedPitchType %in% input$pitches | !is.element(data$PitcherTeam, c("NOR_TAR", "NOR_TAR2")) & data$AutoPitchType %in% input$pitches) &
                                        (is.na(data$ContactType) | data$ContactType %in% input$contactquality) & data$PitchCall %in% input$pitchresult & data$PlayResult %in% append(input$playresult, "Undefined")),]
    #} else{
    #  bp[which(bp$BatterSide %in% c(ifelse(input$bside %in% c("All", "LHB"), "Left", "Right"), ifelse(input$bside == "All", "Right", "BLANK")) & bp$PitcherThrows %in% c(ifelse(input$pside %in% c("All", "LHP"), "Left", "Right"), ifelse(input$pside == "All", "Right", "BLANK")) & 
    #               bp$yyyymmdd > input$daterange[1] & bp$yyyymmdd < input$daterange[2] & (bp$TaggedPitchType %in% input$pitches | !is.element(bp$PitcherTeam, c("NOR_TAR", "NOR_TAR2")) & bp$AutoPitchType %in% input$pitches) &
    #               (is.na(bp$ContactType) | bp$ContactType %in% input$contactquality) & bp$PitchCall %in% remove.spaces(gsub(x = input$pitchresult, pattern = "Ball", replacement = "BallCalled")) & bp$PlayResult %in% append(input$playresult, "Undefined")),]
    #}
      })   
  # subset.data2 <- reactive({dat <- subset.data()
  # if(!is.null(event_data("plotly_selected", source = 'spraychart'))) if(length(event_data("plotly_selected", source = 'spraychart')$key) > 0) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'spraychart')$key),]
  # if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
  # if(!is.null(event_data("plotly_selected", source = 'mvmnt'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'mvmnt')$key),]
  
  #dat
  #})
  
  

output$locPlot <- renderPlotly({
  updateProgress("Pitch Locations")
  # if(input$smoothlocs){
  #   data <- subset.data()
  #   data <- cbind(data, Bases = ifelse(data$PlayResult == "HomeRun", 4, ifelse(data$PlayResult == "Triple", 3, ifelse(data$PlayResult == "Double", 2, ifelse(data$PlayResult == "Single", 1, 0)))),
  #                 Strike = as.numeric(data$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "InPlay") | (data$PitchCall == "FoulBall" & data$Strikes < 2))) # need to null 2k foul balls
  # 
  #   wat_to_do_if_2ops <- 0
  #   data <- cbind(data, value = ifelse(input$smoothlocvar == "# of Pitches", 1, ifelse(input$smoothlocvar == "Strike %", data$Strike, ifelse(input$smoothlocvar == "2OPS", (wat_to_do_if_2ops), ifelse(input$smoothlocvar == "Slugging %", data$Bases, ifelse(input$smoothlocvar == "Batting Avg", as.numeric(data$Bases > 0), -1))))))
  #   if(input$smoothlocvar %in% c("2OPS", "Slugging %", "Batting Avg")){
  #     data <- data[which(data$PitchCall == "InPlay"),]
  #   }
  #   heatmap.pitch.freq(data)
  # } else{
  #   dat <- subset.data()
    dat <- subset.data()
    if(sum(dat$Pitcher == input$player) > 0) plot.pitches.gg(data = dat[which(dat$Pitcher == input$player),]) # date
    else plot_ly() %>% layout(title = "No Data Found")  # }

})


output$locPlotB <- renderPlotly({
  updateProgress("Pitch Locations")
  dat <- subset.data()
  if(sum(dat$Batter == input$player) > 0){
    dat$TaggedPitchType <- dat$AutoPitchType
    plot.pitches.gg(dat[which(dat$Batter == input$player),], p.view = F)
  } else{
    plot_ly() %>% layout(title = "No Data Found")
  }
  })

  output$veloPlot <- renderPlotly({updateProgress("Velo"); dat <- subset.data(); if(sum(dat$Pitcher == input$player) > 0) game.by.game.velo(data = dat[which(dat$Pitcher == input$player),], p = input$player)}) ## explain why 1 SD

  output$mvmntPlot <- renderPlotly({
    updateProgress("Movement")
  #   dat <- subset.data()
  #   #if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
    dat <- subset.data()
    if(sum(dat$Pitcher == input$player) > 0) movement.plot(dat[which(dat$Pitcher == input$player),])
    }) #give comps, show armside/glove side mvmnt, ride/sink
  
  
  
  output$relPlot1 <- renderPlotly({updateProgress("Release Point"); dat <- subset.data(); if(sum(dat$Pitcher == input$player) > 0) releasepointgraph.ly(data1 = dat[which(dat$Pitcher == input$player),])})
  output$relPlot2 <- renderPlotly({dat <- subset.data(); if(sum(dat$Pitcher == input$player) > 0) releasepointgraph.ly(data1 = dat[which(dat$Pitcher == input$player),], xvar = "Extension", plot.title = "")})
  
  # # output$relPlot <- renderPlotly({
  # #   data <- subset.data()
  # #   sph <- mutate(as.data.frame(matrix(nrow = 1e6)), theta = 2 * pi * runif(1e6), phi = acos(1 - 2 * runif(1e6)), x = sin(phi) * cos(theta), y = sin(phi) * sin(theta), z = cos(phi))[,-1]
  # #   plot_ly(data, x = ~RelSide, y = ~Extension, z = ~RelHeight, color = ~TaggedPitchType, source = 'rel')%>%#x = ~sph$x * 9, y = ~sph$y * 9, z = ~sph$z * 5, type = 'mesh3d', color = I('brown') %>%
  # #     #add_surface(z = ~matrix(data = rnorm(n = 1e4, mean = 0, sd = 1), ncol = 3)) %>%
  # #     layout(scene = list(xaxis = list(range = c(-5, 5)),
  # #            yaxis = list(range = c(0, 10)),
  # #            zaxis = list(range = c(0, 10))))
  # #   })
  # 
  
  
  output$strikes <- renderPlotly({updateProgress("Strikes"); dat <- subset.data(); if(sum(dat$Pitcher == input$player) > 0) control.plot(dat[which(dat$Pitcher == input$player),])})

  
  
  output$bbtypes <- renderPlotly({
    updateProgress("Contact Types")
    dat <- subset.data()
    if(sum(dat$Pitcher == input$player) > 0){
      dat <- applybbtype(dat[which(dat$PitchCall == "InPlay" & !is.na(dat$Bearing) & dat$Pitcher == input$player),])
      ag <- aggregate(dat[which(dat$PitchCall == "InPlay"),"BBtype"], list(`Batted Ball Type` = dat[which(dat$PitchCall == "InPlay"),"BBtype"]), length)
      ag$x <- round(100 * ag$x / sum(ag$x))
      plot_ly(x = ~ag[,"Batted Ball Type"], y = ~ag$x, type = "bar", name = input$player, source = 'bbtypes') %>% add_markers(x = c("FB", "GB", "LD", "PU"), y = c(9, 62, 15, 15), name = "elite", marker = list(symbol = "star", color = "red", size = 20), showlegend = T) %>% layout(title = "Hit Types", xaxis = list(title = "Hit Type"), yaxis = list(title = "% of Hits Given Up"))
      #ag <- table(atm[which(!is.na(atm$BBtype)),c("Pitcher", "BBtype")]); ag <- ag[which(rowSums(ag) > 9),]; ag <- ag / rowSums(ag); apply(ag, 2, function(x)quantile(x, probs = c(.1, .25, .5, .75, .9)))
    }
  })
  
  output$bbtypesB <- renderPlotly({
    updateProgress("Contact Types")
    dat <- subset.data()
    if(sum(dat$Batter == input$player) > 0){
      dat <- applybbtype(dat[which(dat$PitchCall == "InPlay" & !is.na(dat$Bearing) & dat$Batter == input$player),])
      ag <- aggregate(dat[which(dat$PitchCall == "InPlay"),"BBtype"], list(`Batted Ball Type` = dat[which(dat$PitchCall == "InPlay"),"BBtype"]), length)
      ag$x <- round(100 * ag$x / sum(ag$x))
      plot_ly(x = ~ag[,"Batted Ball Type"], y = ~ag$x, type = "bar", name = input$player, source = 'bbtypes') %>% add_markers(x = c("FB", "GB", "LD", "PU"), y = c(39, 20, 48, 0), name = "elite", marker = list(symbol = "star", color = "red", size = 20), showlegend = T) %>% layout(title = "Hit Types", xaxis = list(title = "Hit Type"), yaxis = list(title = "% of Hits Given Up"))
    }
  })
  
  
  
  output$spraychart <- renderPlotly({
    updateProgress("Spray Charts")
    dat <- subset.data()
   # if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
    if(sum(dat$Pitcher == input$player) > 0) spraychart(data = dat[which(dat$Pitcher == input$player),], plot.title = "Spraychart")})
  
  output$spraychartB <- renderPlotly({
    updateProgress("Spray Charts")
    dat <- subset.data()
    # if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
    if(sum(dat$Batter == input$player) > 0) spraychart(data = dat[which(dat$Batter == input$player),], plot.title = "Spraychart")})
  
  output$LAEVB <- renderPlotly({updateProgress("Launch Angle/Exit Velo"); dat <- subset.data(); if(sum(dat$Batter == input$player) > 0)laev.visual.ly(dat[which(dat$Batter == input$player),])})
  
  ## takes 3 seconds, use images
  output$heatmap <- renderPlot({updateProgress("Hot Cold Zones"); dat <- subset.data();if(sum(dat$Pitcher == input$player) > 0) plot.pitcher.hot.cold(pitcher.hot.cold(dat[which(dat$Pitcher == input$player),])); progress$close()})
  output$heatmapB <- renderPlot({updateProgress("Hot Cold Zones"); dat <- subset.data(); if(sum(dat$Batter == input$player) > 0)plot.batter.hot.cold(batter.hot.cold(dat[which(dat$Batter == input$player),])); progress$close()})
  
      
  }
  )
