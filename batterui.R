pitch.names <- c("Fastball", "Changeup", "Breaking Ball")
convert.pitch.names <- function(x)ifelse(x == "Fastball", "FB", ifelse(x == "Changeup", "CH", ifelse(x == "Breaking Ball", "BB", x)))
pitch.results <- c("Strike Called", "Strike Swinging", "Foul", "Ball", "In Play")
bb.results <- c("Out", "Single", "Double", "Triple", "HomeRun")
counts <- c(paste(c(0:3), 0, sep = "-"), paste(c(0:3), 1, sep = "-"), paste(c(0:3), 2, sep = "-")) # paste(c(0:3), "balls"), paste(0:2, "strikes")
load("data/UNCall.RData")
data$yyyymmdd <- paste0("20", str_sub(as.Date(data$Date, format = "%m/%d/%Y"), 3, -1))
data <- generic.pitch.tag(data)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("UNC Batter Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "batter", label = "Batter", choices = sort(unique(data$Batter)), selected = "Busch, Michael"),
      dateRangeInput(inputId = "daterange", label = "Date Range", start = "2018-02-16", end = "2018-06-20", min = "2016-09-01", max = Sys.Date()),
      checkboxGroupInput(inputId = "pitches", label = "Pitch Types", choices = pitch.names, selected = pitch.names),
      # counts could be a pickerinput from shinywidgets
      checkboxGroupInput(inputId = "count", label = "Count", choices = counts, selected = counts, inline = T),
      checkboxGroupInput(inputId = "pitchresult", label = "Pitch Result", choices = pitch.results, selected = pitch.results),
      checkboxGroupInput(inputId = "playresult", label = "Hit Result", choices = bb.results, selected = bb.results),
      submitButton()
    ),
    
    
    mainPanel(
      plotOutput("locPlot", height = "800px"), # heat maps - option to smooth, then how much
      plotOutput("laev", height = "800px"),
      plotlyOutput("bbtypes"),
      plotlyOutput("spraychart"),
      plotlyOutput("stats"), # add expected
      plotOutput("hotcold", height = "700px", width = "700px")
      
      ## with BP/full TM data
      # contact location
      # trackman data by date
    )
  )
)
)
