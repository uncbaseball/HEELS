
##############################################
##                                          ##
##    dat$selection forces recalculation    ##
##                                          ##
##############################################

## only needs to be called after points are selected

### step one is making plots use only SharedData object
  ## both (in order of ease of conversion):
  # battedballs, stats, heatmap -- can be static img at least temporarily
  ## PO:
  # velo, release points, strike

  ## once they're all plotly, maybe put all in one ui output element

### step two is moving everything to js
  ## then move sidebar filters to plotly (top graph?) js? crosslink-plotly
  
  ## then move to an html file that loads in a json/csv

### step three is adding a button to show video for a selected point(s)

shinyServer(function(input, output, session) {
  
  
  ## can bring back the action button if we make this an observeEvent
  ## where all of the inputs are the defaults
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  # 
  #   if (!is.null(query$player)) updateSelectInput(session, "player", choices = query$player)
  #   if(!is.null(query$tabs)) updateTabsetPanel(session, "tabs", selected = query$tabs)
  #   shinyjs::reset("smoothlocs")
  # })
  
  # observeEvent(input$showSidebar, {
  #   shinyjs::show(id = "filters")
  # })
  # observeEvent(input$hideSidebar, {
  #   shinyjs::hide(id = "filters")
  # })
  # 
  progress <- Progress$new()
  progress$set(message = "Loading...")
  updateProgress <- function(detail) progress$inc(amount = 1 / ifelse(input$tabs == "Pitcher", 8, 5), detail = detail)


  ## save starting inputs to a list
  # observe({
  #   saveRDS(reactiveValuesToList(input), file = "initial chillmedia inputs.rds")
  #   stopApp()
  #   })
  
  
  
  subset.data <- reactive({
    #session$clientData$url_search
    #if(input$setting == "Game"){
    print(input$tabs)
    SharedData$new(
      data %>% filter_(paste0(input$tabs, " == '", input$player, "'")) %>%
        filter(BatterSide %in% c(ifelse(input$bside %in% c("All", "LHB"), "Left", "Right"), ifelse(input$bside == "All", "Right", "BLANK")) & #if_else(tab == "Pitcher" &
                         #PitcherThrows %in% c(ifelse(input$pside %in% c("All", "LHP"), "Left", "Right"), ifelse(input$pside == "All", "Right", "BLANK")) & assume you got the right pitcher hand if you're looking @ one dude
                         yyyymmdd > input$daterange[1] & yyyymmdd < input$daterange[2] &
                         (TaggedPitchType %in% input$pitches | !is.element(PitcherTeam, c("NOR_TAR", "NOR_TAR2")) & AutoPitchType %in% input$pitches) &
                         (is.na(ContactType) | ContactType %in% input$contactquality) & PitchCall %in% input$pitchresult & PlayResult %in% append(input$playresult, "Undefined")) %>%
        as.data.frame()
    )
  })
    
  #observe({print(reactiveValuesToList(input))})



  ####  STATS   ####
  output$statsP <- renderPlotly({
    if(input$tabs == "Pitcher"){
    }
  })

  output$statsB <- renderPlot({
    if(input$tabs == "Batter"){
      # "BA", "OBP", "SLG", "2OPS", "BBper", "Kper", "Good_Contact%" -- per 10
  
      # "Avg_Exit_Velo", "Max_Exit_Velo"
  
      dat <- subset.data()
      if(!is.null(dat$selection())){
        dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      } else{
        dat <- dat$origData()
      }
  
      ## somehow the stats should be x and the values should be y
        # difficulty is the overlap
      
      ## add a column that's include in stats (to filter)
      
      # dat %<>% mutate(Result = if_else(PlayResult != "Undefined", PlayResult,
      #                                  if_else(KorBB != "Undefined", KorBB, PitchCall)))
      # dat$one <- 1
      # Result <- dat$Result
      # 
      # ## maybe a button for if you only want in play
      # plotly::subplot(
      # plot_ly(data = dat, x = ~Result, y = ~one, type = "bar", name = "results",
      #         transforms = list(
      #           #list(type = "filter", target = dat$one, operation = "=", value = 1),
      #           list(type = "aggregate", groups = ~Result,
      #                                aggregations = list(list(target = "y", func = "count", enabled = T)))
      #           )),
      
      plotly::subplot(plot_ly(data = dat, x = "Batting Avg", y = ~Hit, type = "bar", name = "AVG",
              transforms = list(
                list(type = "aggregate", group = ~Hit, 
                     aggregations = list(list(target = "y", func = "avg", enabled = T)))
              )),
      plot_ly(data = dat, x = "On Base %", y = ~Reach, type = "bar", name = "OBP",
              transforms = list(
                list(type = "aggregate", group = ~Reach, 
                     aggregations = list(list(target = "y", func = "avg", enabled = T)))
              )),
      plot_ly(data = dat, x = "Slugging %", y = ~Bases, type = "bar", name = "SLG",
        transforms = list(
          list(type = "aggregate", group = ~Bases, 
               aggregations = list(list(target = "y", func = "avg", enabled = T)))
        )),
      plot_ly(data = dat, x = "Strikeout %", y = ~K, type = "bar", name = "K%",
              transforms = list(
                list(type = "aggregate", group = ~K, 
                     aggregations = list(list(target = "y", func = "avg", enabled = T)))
              )),
      plot_ly(data = dat, x = "Walk %", y = ~BB, type = "bar", name = "BB%",
              transforms = list(
                list(type = "aggregate", group = ~BB, 
                     aggregations = list(list(target = "y", func = "avg", enabled = T)))
              )),
      plot_ly(data = dat, x = "Well Hit %", y = ~Good_Contact, type = "bar", name = "Well Hit%",
              transforms = list(
                list(type = "aggregate", group = ~Bases, 
                     aggregations = list(list(target = "y", func = "avg", enabled = T)))
              )), nrows = 1, shareY = T)
      
      
      #,
                                #list(type = "aggregate", )))
      
  
  #   htmlwidgets::onRender("transforms: [{
  #                         type: 'aggregate',
  #                         groups: 'x',
  #                         aggregations: [
  #                         {target: 'y', func = 'count'}
  #                         ]
  #                         }
  #                         ]")
  # , 
  # 
  # {
  #   type: 'filter',
  #   target: 'y',
  #   operation: '=',
  #   value: 1
  # },
  
      # if(nrow(dat) > 0){
      #   hitter.stats.from.tm(dat)
      # }
    }
  })


  ####  LOCATIONS   #####

  output$locPlot <- renderPlotly({
    if(input$tabs == "Pitcher"){
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
        if(nrow(dat$origData()) > 0) plot.pitches.gg(data = dat) %>% highlight("plotly_selected")# date
        else plot_ly() %>% layout(title = "No Data Found")  # }
    }
  })


  output$locPlotB <- renderPlotly({
    if(input$tabs == "Batter"){
      updateProgress("Pitch Locations")
  
      dat <- subset.data()
  
      if(nrow(dat$origData()) > 0 & input$tabs == "Batter"){
        #dat$TaggedPitchType <- dat$AutoPitchType
        plot.pitches.gg(dat, p.view = F, dragmode = "lasso") %>% highlight("plotly_selected")
      } else{
        plot_ly() %>% layout(title = "No Data Found")
      }
    }
  })


  ####  VELO  ####

  output$veloPlot <- renderPlotly({
    if(input$tabs == "Pitcher"){
    updateProgress("Velo")
      dat <- subset.data()
      if(!is.null(dat$selection())){
        dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      } else{
        dat <- dat$origData()
      }
      if(nrow(dat) > 0) game.by.game.velo(data = dat, p = input$player)
    }
  }) ## explain why 1 SD

  #### MOVEMENT ####
  output$mvmntPlot <- renderPlotly({
    if(input$tabs == "Pitcher"){
      updateProgress("Movement")
    #   dat <- subset.data()
    #   #if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
      dat <- subset.data()
      if(nrow(dat$origData()) > 0) movement.plot.plate(dat, lhp = as.logical(input$player == "O'Brien, Caden")) %>% highlight("plotly_selected")
    }
  }) #give comps, show armside/glove side mvmnt, ride/sink

  ####  RELEASE POINT ####

  output$relPlot1 <- renderPlotly({
    if(input$tabs == "Pitcher"){
      updateProgress("Release Point")
      dat <- subset.data()
      if(!is.null(dat$selection())){
        dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      } else{
        dat <- dat$origData()
      }
      if(nrow(dat) > 0) releasepointgraph.ly(data1 = dat)
    }
  })

  output$relPlot2 <- renderPlotly({
    if(input$tabs == "Pitcher"){
      dat <- subset.data()
      if(!is.null(dat$selection())){
        dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      } else{
        dat <- dat$origData()
      }
      if(nrow(dat) > 0) releasepointgraph.ly(data1 = dat, xvar = "Extension", plot.title = "")
    }
  })

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

  ####  STRIKES ####
  output$strikes <- renderPlotly({
    if(input$tabs == "Pitcher"){
      updateProgress("Strikes")
      dat <- subset.data()
      if(!is.null(dat$selection())){
        dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      } else{
        dat <- dat$origData()
      }
      if(nrow(dat) > 0) control.plot(dat)
    }
  })


  ####  BATTED-BALL TYPES ####
  output$bbtypes <- renderPlotly({
    if(input$tabs == "Pitcher"){
      updateProgress("Contact Types")
      print('making batted balls')
      dat <- subset.data()
      #if(!is.null(dat$selection())){
        #dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      #} else{
        dat <- dat$origData()
      #}
      if(nrow(dat) > 0){
        dat <- applybbtype(dat[which(dat$PitchCall == "InPlay" & !is.na(dat$Bearing)),])
        ag <- aggregate(dat[which(dat$PitchCall == "InPlay"),"BBtype"], list(`Batted Ball Type` = dat[which(dat$PitchCall == "InPlay"),"BBtype"]), length)
        ag$x <- round(100 * ag$x / sum(ag$x))
        plot_ly(x = ~ag[,"Batted Ball Type"], y = ~ag$x, type = "bar", name = input$player, source = 'bbtypes') %>% add_markers(x = c("FB", "GB", "LD", "PU"), y = c(9, 62, 15, 15), name = "elite", marker = list(symbol = "star", color = "red", size = 20), showlegend = T) %>% layout(title = "Hit Types", xaxis = list(title = "Hit Type"), yaxis = list(title = "% of Hits Given Up"))
        #ag <- table(atm[which(!is.na(atm$BBtype)),c("Pitcher", "BBtype")]); ag <- ag[which(rowSums(ag) > 9),]; ag <- ag / rowSums(ag); apply(ag, 2, function(x)quantile(x, probs = c(.1, .25, .5, .75, .9)))
      }
    }
  })

  output$bbtypesB <- renderPlotly({
    if(input$tabs == "Batter"){
      updateProgress("Contact Types")
      dat <- subset.data()
      # if(!is.null(dat$selection())){
      #   dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      # } else{
      #   dat <- dat$origData()
      # }

      #if(nrow(dat) > 0){
        bbtypes <- as.data.frame(nnet::class.ind(dat$BBtype))
        bbtypes[which(dat$PitchCall != "InPlay"),] <- NA
        dat <- cbind(dat, bbtypes)
        
        plotly::subplot(plot_ly(data = dat, x = "Groundball %", y = ~GB, name = "GB%", type = "bar", transforms = list(list(type = "aggregate", groups = ~GB, aggregations = list(list(target = "y", func = "avg", enabled = T))))),
        plot_ly(data = dat, x = "Line Drive %", y = ~LD, name = "LD%", type = "bar", transforms = list(list(type = "aggregate", groups = ~LD, aggregations = list(list(target = "y", func = "avg", enabled = T))))),
        plot_ly(data = dat, x = "Fly Ball %", y = ~FB, name = "FB%", type = "bar", transforms = list(list(type = "aggregate", groups = ~FB, aggregations = list(list(target = "y", func = "avg", enabled = T))))),
        plot_ly(data = dat, x = "Pop Up %", y = ~PU, name = "PU%", type = "bar", transforms = list(list(type = "aggregate", groups = ~PU, aggregations = list(list(target = "y", func = "avg", enabled = T))))), shareY = T)
        
        
        
        # plot_ly(data = dat, x = ~BBtype, y = ~BBtype, type = "bar", transforms = list(list(type = "aggregate", groups = ~BBtype, aggregations = list(list(target = "y", func = "count", enabled = T)))))
      #   dat <- dat[which(dat$PitchCall == "InPlay" & !is.na(dat$Bearing)),] ## do this filter during applybbtype fxn
      #   ag <- aggregate(dat[which(dat$PitchCall == "InPlay"),"BBtype"], list(`Batted Ball Type` = dat[which(dat$PitchCall == "InPlay"),"BBtype"]), length)
      #   ag$x <- round(100 * ag$x / sum(ag$x))
      #   plot_ly(x = ~ag[,"Batted Ball Type"], y = ~ag$x, type = "bar", name = input$player, source = 'bbtypes') %>% add_markers(x = c("FB", "GB", "LD", "PU"), y = c(39, 20, 48, 0), name = "elite", marker = list(symbol = "star", color = "red", size = 20), showlegend = T) %>% layout(title = "Hit Types", xaxis = list(title = "Hit Type"), yaxis = list(title = "% of Hits Given Up"))
      # }
    }
  })


  ####  SPRAYCHARTS  ####

  ## we can probably use the same one for B and P
  output$spraychart <- renderPlotly({
    if(input$tabs == "Pitcher"){
      #updateProgress("Spray Charts")
      dat <- subset.data()
     # if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
  
      print('making spraycharts`')
      if(nrow(dat$origData()) > 0) spraychart(data = dat, plot.title = "Spraychart")
    }
  })

  output$spraychartB <- renderPlotly({
    if(input$tabs == "Batter"){
      updateProgress("Spray Charts")
      dat <- subset.data()
      # if(!is.null(event_data("plotly_selected", source = 'loc'))) dat <- dat[which(dat$key %in% event_data("plotly_selected", source = 'loc')$key),]
      if(nrow(dat$origData()) > 0) spraychart(data = dat, plot.title = "Spraychart")
    }
  })



  ####  LA/EV #####
  output$LAEVB <- renderPlotly({
    if(input$tabs == "Batter"){
      updateProgress("Launch Angle/Exit Velo")
      dat <- subset.data()
      if(nrow(dat$origData()) > 0) laev.visual.ly(dat, batter = input$player, lefty = as.logical(mode(dat$origData()$BatterSide) == "Left"), abnum = F)
    }
  })

   
  ####  HEATMAPS  ####
  ## takes 3 seconds, use images
  output$heatmap <- renderPlot({
    if(input$tabs == "Pitcher"){
      #updateProgress("Hot Cold Zones")
      dat <- subset.data()
      # if(!is.null(dat$selection())){
      #   dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      # } else{
      dat <- dat$origData()
      # }
      if(nrow(dat) > 0) print('making pitcher hc'); plot.pitcher.hot.cold(pitcher.hot.cold(dat))
      progress$close()
    }
  })

  output$heatmapB <- renderPlot({
    if(input$tabs == "Batter"){
      updateProgress("Hot Cold Zones")
      dat <- subset.data()
      # if(!is.null(dat$selection())){
      #   dat <- as.data.frame(subset.data()$origData())[which(subset.data()$selection()),]
      # } else{
        dat <- dat$origData()
      # }

      if(nrow(dat) > 0){
        print('making batter hc')
        plot.batter.hot.cold(batter.hot.cold(dat))
      }
      progress$close()
    }
  })
  
  

  
  
  

  #hide(id = "loading-content", anim = TRUE, animType = "fade")    
  #
  
  #eventReactive({not default inputs}) show("app-content")
  #show("app-content")
      
})
