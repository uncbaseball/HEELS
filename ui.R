
################################
##                            ##
##      FUTURE VERSIONS       ##
##                            ##
################################

### V3

  ## Stats -- pitcher, controllables, percentiles
    # hit it in a bad spot, they made a great play or had you shifted

#         --

### V4
  ## login, loading page # https://www.youtube.com/watch?v=PEzQYET_0jg

### Beyond

  ## Video

  ## Additional Visuals
    # pitcher spin visual
    # batter contact location
    # pace

  ## Ease of Use
    # explanations
    # link to postgame reports
    # faster load
    # tick for each velo date
  
  ## Additional Data
    # bp/bullpens
    # catcher
      ## catcher won't be that hard -- just add playid to tm, catcher to pbp and merge. add to dataset (should add yyyymmdd too)
      ## 2 visuals - pitch loc w/ strike prob in tooltip, strike zone (split R/L) and game by game strikes above avg

  ## Additional Stats
    # pitch values (hot cold behind movement?)


############################
##                        ##
##      LOADING TIME      ##
##                        ##
############################

## Background load takes 22 seconds, no data graph after additional 3 seconds
## Pitcher load takes 12 seconds
## Batter load takes 8 seconds

## 23 seconds to get to batter w/ all columns, same removing some (reading uncall.csv)
## down to about 20 seconds when you read the abridged file


library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(ggplot2)


pitch.names <- c("Fastball" = "FB", "Changeup" = "CH", "Curveball" = "CU", "Slider" = "SL", "Cutter" = "FC", "Breaking Ball" = "BB")
pitch.results <- c("Strike Called" = "StrikeCalled", "Strike Swinging" = "StrikeSwinging", "Foul" = "FoulBall", "Ball" = "BallCalled", "In Play" = "InPlay")
bb.results <- c("Out", "Single", "Double", "Triple", "HomeRun")
con.qual <- c("Barrel", "Solid Contact" = "Solid", "Flares & Burners" = "F&B", "Topped", "Under", "Weak")
smooth.loc.stats <- c("# of Pitches", "Strike %", "2OPS", "Slugging %", "Batting Avg")
counts <- c(paste(c(0:3), 0, sep = "-"), paste(c(0:3), 1, sep = "-"), paste(c(0:3), 2, sep = "-")) # paste(c(0:3), "balls"), paste(0:2, "strikes")



shinyUI(dashboardPage(
  
  # Application title
  title = "UNC Data",
  #shinyWidgets::setBackgroundImage(src = 'https://goheels.com/images/2012/12/11/ZVFMCVYVNBWCQGR.20121211232849.jpg'),
  
#   tags$style(type = "text/css", "
# .shiny-output-error { visibility: hidden; }
# .shiny-output-error:before {
#   visibility: visible;
#   content: 'There was an error loading the page. Can you tell Micah? He's gonna be pissed but he needs to know.'; }
# }
# "),
  header = dashboardHeader(title = tags$a(href = "http://goheels.com/index.aspx?path=baseball", tags$img(src = "UNC Logo.png", width = "50em", height = "50em"))),
  
  sidebar = dashboardSidebar(sidebarUserPanel("Player", image = "headshots/Bergner,Austin.jpg"),
                             
                            
                   selectInput(inputId = "player", label = "Player", choices = sort(unique(c(data[which(data$PitcherTeam %in% c("NOR_TAR", "NOR_TAR2")),"Pitcher"], 
                                                                                             data[which(data$BatterTeam %in% c("NOR_TAR", "NOR_TAR2")),"Batter"]))), selected = "Busch, Michael"),
                   dateRangeInput(inputId = "daterange", label = "Date Range", start = "2018-02-16", end = Sys.Date(), min = "2016-09-01", max = Sys.Date()),
                   # radioButtons(inputId = "setting", label = "Game/Practice", choices = c("Game", "Practice"), selected = "Game"), # later BP/pen vs live bp vs scrimmages
                   #conditionalPanel("input.setting == 'Game'", checkboxGroupInput(InputID = "gametype", label = "Regular Season")),
                   selectInput("pside", "Pitcher L/R", choices = c("All", "LHP", "RHP")),
                   selectInput("bside", "Batter L/R", choices = c("All", "LHB", "RHB")),
                   checkboxGroupInput(inputId = "pitches", label = "Pitch Types", choices = pitch.names, selected = pitch.names),
                   # counts could be a pickerinput from shinywidgets
                   checkboxGroupInput(inputId = "count", label = "Count", choices = counts, selected = counts, inline = T),
                   checkboxGroupInput(inputId = "pitchresult", label = "Pitch Result", choices = pitch.results, selected = pitch.results),
                   checkboxGroupInput(inputId = "playresult", label = "Hit Result", choices = bb.results, selected = bb.results),
                   checkboxGroupInput(inputId = "contactquality", label = "Contact Quality", choices = con.qual, selected = con.qual),
                   checkboxInput(inputId = "smoothlocs", label = "Smooth Pitch Locations", value = F),
                   conditionalPanel("input.smoothlocs == true", selectizeInput(inputId = "smoothlocvar", label = "Pitch Location Stat", choices = smooth.loc.stats, selected = "# of Pitches")),
                   conditionalPanel("input.smoothlocs == true", sliderInput(inputId = "smoothness", label = "How Smooth", min = 0, max = 1, value = .05, step = .01)),
                   id = "filters",
                   # selectInput(inputID = "pace", label = "Pace Before/After", choices = c("Before", "After"), selected = "After"),
                   actionButton("submit", "Apply Changes")
  ),
  
  body = dashboardBody(
#     shinyjs::useShinyjs(),
#     shinyjs::inlineCSS("
# #loading-content {
#               position: absolute;
#               background: #000000;
#               opacity: 0.9;
#               z-index: 100;
#               left: 0;
#               right: 0;
#               height: 100%;
#               text-align: center;
#               color: #FFFFFF;
#               }
#               "),

    # div(
    #   id = "loading-content",
    #   h2("Loading...")
    # ),
    
    #hidden(
      div(
        id = "app-content",
        
    tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("Pitcher", value = "Pitcher", fluidRow(
                  plotOutput("statsP"),
                  plotlyOutput("locPlot"),
                  plotlyOutput("veloPlot"),
                  plotlyOutput("mvmntPlot"),
                  fluidRow(
                    box(plotlyOutput("relPlot1")),
                    box(plotlyOutput("relPlot2"))),
                  fluidRow(
                    box(plotlyOutput("bbtypes")),
                    box(plotlyOutput("spraychart"))),
                  plotlyOutput("strikes", height = "700px"),
                  plotOutput("heatmap")#,
                  # plotlyOutput("pacePlot")# ,
                  # verbatimTextOutput("brush"),
                  # verbatimTextOutput("zoom")
                )),
                #tabPanel("Batter", value = "Batter", fluidRow(
                tabPanel("Batter", value = "Batter", fluidRow(
                  plotOutput("statsB"),
                  plotlyOutput("locPlotB"),
                  plotOutput("heatmapB"),
                  fluidRow(
                  box(plotlyOutput("spraychartB")),
                  box(plotlyOutput("LAEVB"))),#, height = "600px", width = "575px"
                  plotlyOutput("bbtypesB")
                  # plotlyOutput("batter", height = "1200px")
                )))
      )
    #)
    
  )
  
  
))


#### Improvements:  ####
## Loading message while plots render https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running
