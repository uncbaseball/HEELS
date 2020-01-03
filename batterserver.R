server <- function(input, output) {
  
  subset.data <- reactive({data[which(data$Batter == input$batter & data$yyyymmdd > input$daterange[1] & data$yyyymmdd < input$daterange[2] & data$AutoPitchType %in% convert.pitch.names(input$pitches) & data$PitchCall %in% remove.spaces(gsub(x = input$pitchresult, pattern = "Ball", replacement = "BallCalled")) & data$PlayResult %in% append(input$playresult, "Undefined")),]}) 
  output$locPlot <- renderPlot({
    data <- subset.data()
    par(mfrow = c(2,1), mar = c(0, 0, 2, 0))
    plot.pitches(data = data[which(data$PitcherThrows == "Left"),], plot.title = "vs. LHP", legend.cex = .7)
    plot.pitches(data = data[which(data$PitcherThrows == "Right"),], plot.title = "vs. RHP", legend.cex = .7)
    
  })
  output$laev <- renderPlot({laev.visual(unique(subset.data()))})
  output$bbtypes <- renderPlotly({
    data <- subset.data()
    data <- applybbtype(data[which(data$PitchCall == "InPlay" & !is.na(data$Bearing)),])
    ag <- aggregate(data[which(data$PitchCall == "InPlay"),"BBtype"], list(`Batted Ball Type` = data[which(data$PitchCall == "InPlay"),"BBtype"]), length)
    ag$x <- round(100 * ag$x / sum(ag$x))
    plot_ly(x = ~ag[,"Batted Ball Type"], y = ~ag$x, type = "bar") %>% layout(title = "Hit Types", xaxis = list(title = "Hit Type"), yaxis = list(title = "% of Hits"))
    
  })
  output$spraychart <- renderPlotly({spraychart(data = subset.data(), plot.title = "Spraychart")})
  output$stats <- renderPlotly({
    data <- subset.data()
    ag <- data %>%
      group_by(GameID, Inning, PAofInning) %>%
      mutate(NoPA = !any(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Walk", "Strikeout")), NonHitReach = as.numeric(any(PitchCall == "HitByPitch" | KorBB == "Walk")), Hit = as.numeric(any(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))), Bases = ifelse(any(PlayResult == "Single"), 1, ifelse(any(PlayResult == "Double"), 2, ifelse(any(PlayResult == "Triple"), 3, ifelse(any(PlayResult == "HomeRun"), 4, 0))))) %>%
      filter(!NoPA) %>%
      group_by() %>%
      summarise(OBP = mean(NonHitReach | Hit), AVG = mean(Hit), SLG = mean(Bases)) %>%
      mutate(`2OPS` = (1.7 * OBP + SLG) / 3) %>%
      as.data.frame()
    
    plot_ly(x = ~colnames(ag), y = ~unlist(ag[1,]), type = "bar") %>% layout(title = "Stats", xaxis = list(title = "Stat Type"), yaxis = list(title = ""))
    
  })
  output$hotcold <- renderPlot({plot.batter.hot.cold(batter.hot.cold(subset.data()))})
}
