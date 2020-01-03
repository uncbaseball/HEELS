
## Need to make sure there are no Undefined BatterSides

##  Currently Missing:
  # High Speed From Garnder Webb
  # High Speed From Miami
  # Synergy From Liberty
  # Synergy From VT Game 1 Pitcher
  # VT Synergy

batterx <- data9 %>%
    filter(yyyymmdd > "2019-02-17" & PitchCall != "Undefined" & Scrimmage == "Season" & endsWith(Date, "9") & startsWith(BatterTeam, "NOR_TAR")) %>% #yyyymmdd > "2019-02-17" # GameID == "UMLUNC030219"
    mutate(ab_string = paste(paste("AB", Times_In_The_Box), AB_Result, sep = " - "), pitch_string = paste(paste("Pitch", PitchofPA), Pitch_Result, sep = " - "),
           HS_video_link = paste0("https://s3.amazonaws.com/unchitterhighspeedvideo/", GameID, paste0("_", substr(BatterSide, 1, 1), "F/") , key, ".mp4"),
           TV_video_link = paste0("https://s3.amazonaws.com/publicuncsynergyvideo/", GameID, "/", key, ".mp4")
           ) %>%
  arrange(desc(yyyymmdd), Inning, PitchofPA) %>%
  select(Player = Batter, Game, ab_string, pitch_string, HS_video_link, TV_video_link)
# 
pitcherx <- data9 %>%
  filter(yyyymmdd > "2019-02-17" & PitchCall != "Undefined" & Scrimmage == "Season" & endsWith(Date, "9") & startsWith(PitcherTeam, "NOR_TAR")) %>%
  mutate(ab_string = paste(paste("Inning", Inning, Batter), AB_Result, sep = " - "), pitch_string = paste(paste("Pitch", PitchofPA), Pitch_Result, sep = " - "),
         TV_video_link = paste0("https://s3.amazonaws.com/publicuncsynergyvideo/", GameID, "/", key, ".mp4"),
         Side_video_link = paste0("https://s3.amazonaws.com/unchitterhighspeedvideo/", GameID, "_", if_else(PitcherThrows == "Left", "RF/", "LF/"), key, ".mp4"),
         Front_video_link = paste0("https://s3.amazonaws.com/unchitterhighspeedvideo/", GameID, "_", if_else(PitcherThrows == "Right", "RF/", "LF/"), key, ".mp4")
         ) %>%
  arrange(desc(yyyymmdd), Inning, PitchofPA) %>%
  select(Player = Pitcher, Game, ab_string, pitch_string, TV_video_link, Side_video_link, Front_video_link)

# setwd(paste0(main.dir, "/Pitchers"))
# for(p in unique(pitcherx$Player)) videodropdown(pitcherx %>% filter(Player == p), pitcher = T)
# for(p in unique(batterx$Player)) videodropdown(batterx %>% filter(Player == p))#, "Slo-Mo")


videodropdown <- function(x, pitcher = F, in.app = F){
  usa <- function(x) unlist(strsplit(as.character(x), split = "\n"))
  linklegal <- function(x) gsub(x, pattern = "/| | - |\\(|\\)|&|,", replacement = "_")
  tags <- htmltools::tags
  
  player <- unique(x$Player)
  assert_that(length(player) == 1)
  
  
  
  doc <- 
    tags$body(
      tags$div(class="container",
                 
               tags$h2(paste(player, "Video")),
               tags$button(class = "btn-warning", tags$a("Back to App", href = paste0(player, ".html"))),
               tags$p("Click on the Game / At-Bat / Pitch you want to watch"),
               tags$br(),
               #tags$p("Missing Slo-Mo Video From: Campbell, Miami, Gardner-Webb and Before UMass-Lowell"),
               tags$div(class="panel-group",
                        tags$div(class="panel panel-default")
               )
      )
    )
  
  thehtml <-
    c("<html>",
    "<head>",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/css/bootstrap.min.css\">",
    "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js\"></script>",
    "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/js/bootstrap.min.js\"></script>",
    "</head>",
    usa(doc))
  
  tailhtml <- c(tail(thehtml, 3), "</html>")
  thehtml <- head(thehtml, -3)
  thehtml[length(thehtml)] <- str_sub(thehtml[length(thehtml)], 1, -7)
  
  if(in.app){
    everything <-       usa(tags$div(class="container",

                              tags$h2(paste(player, "Video")),
                              tags$button(class = "btn-warning", tags$a("Back to App", href = paste0(player, ".html"))),
                              tags$p("Click on the Game / At-Bat / Pitch you want to watch"),
                              tags$br(),
                              tags$p("MISSING: Miami and Gardner-Webb High Speed, and High Speed Before UMass-Lowell"),
                              tags$div(class="panel-group",
                                       tags$div(class="panel panel-default")
                              )
    ))
    thehtml <- head(everything, -3)
    tailhtml <- tail(everything, 3)
    
  }

  for(game in unique(x$Game)){
    thehtml <- c(thehtml, 
                 usa(tags$div(class="panel-heading",
                            tags$h4(class="panel-title",
                                    tags$a("data-toggle"="collapse", href=linklegal(paste0("#", game)), game)
                            )
                    )),
                 paste0("<div id=\"", linklegal(game), "\" class=\"panel-collapse collapse\">")
                 #tags$div(id=paste(game, sep = "_"), class="panel-collapse collapse",
                 )
    for(ab_string in unique(x %>% filter(Game == game) %>% select(ab_string))[,1]){   
      thehtml <- c(thehtml,
                          usa(
                            tags$div(class="panel-heading",
                                     tags$h4(class="panel-title",
                                             tags$a("data-toggle"="collapse", href=linklegal(paste0("#", game, "_", ab_string)), ab_string)
                                     )
                            )
                          ),
                    paste0("<div id=\"", linklegal(paste0(game, "_", ab_string)), "\" class=\"panel-collapse collapse\">")
                  )
                            #tags$div(id=paste(game, ab_string, sep = "_"), class="panel-collapse collapse",
                                     for(row in which(x$Game == game & x$ab_string == ab_string)){
                                       video_link <- x[row, "video_link"]
                                       pitch_string <- x[row, "pitch_string"]
                                       if(pitcher){
                                         thehtml <- c(thehtml,      
                                                      usa(tags$button(class="btn btn-info", style="display:block;", tags$a(href=x[row, "TV_video_link"], paste("TV:", pitch_string)))),
                                                      usa(tags$button(class="btn btn-warning", style="display:block;", tags$a(href=x[row, "Side_video_link"], paste("Side Slo-Mo:", pitch_string)))),
                                                      usa(tags$button(class="btn btn-warning", style="display:block;", tags$a(href=x[row, "Front_video_link"], paste("Front Slo-Mo:", pitch_string))))
                                         )
                                       } else{
                                         thehtml <- c(thehtml,      
                                                      usa(tags$button(class="btn btn-info", style="display:block;", tags$a(href=x[row, "TV_video_link"], paste("TV:", pitch_string)))),
                                                      usa(tags$button(class="btn btn-warning", style="display:block;", tags$a(href=x[row, "HS_video_link"], paste("Slo-Mo:", pitch_string))))
                                         )
                                       }

                                      }
      thehtml <- c(thehtml, "</div>")
    }
    thehtml <- c(thehtml, "</div>")
  }
  
  if(in.app){
    return(c(thehtml, tailhtml))
  } else{
    write(c(thehtml, tailhtml), file = paste0(main.dir, "/", ifelse(pitcher, "Pitchers/", "Hitters/"), gsub(paste(player, "Video Dropdown.html"), pattern = "  ", replacement = " ")))
  }
  

}




