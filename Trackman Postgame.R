#################   Trackman Postgame Processing    #################

filename <- "UNC vs. Auburn 6.10.19"
opp <- "Auburn"
camera.folder.prefix <- "Auburn"
camera.folder.path <- "/Users/micahdaley-harris/Desktop/"
# 1. Fix Warmup Pitches
game <- fix.warmup.pitchofpa(slide.trackman.columns(read.csv(file = paste0("../TAR Database/", filename, ".csv"), stringsAsFactors = F)))

# game$Time <- ifelse(substr(game$Time, 2, 2) == ":",
#                     paste0(as.numeric(substr(game$Time, 1, 1)) - 4 + ifelse(startsWith(game$Time, '1'), 12, 0), str_sub(game$Time, 2, -3), "PM"),
#                     paste0(as.numeric(substr(game$Time, 1, 2)) - 4, str_sub(game$Time, 3, -3), "PM"))
# game$Time <- gsub(game$Time, pattern = "^-2", replacement = "10")
# game$Date <- "6/1/2019"

# 2. Pitch Tagging
gametag <- tag.pitches(game, remove.warmups = T)
# 3. Add predicted Columns
game.pred <- add.predicted.columns(game, opponent = opp, export = T)
# 4. Export Synergy csv
## Validate exported file
## Upload https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html
gameid <- unique(game.pred$GameID)
assert_that(length(gameid) == 1)

# Add Data to DB
data9 <- rbind(data9, game.pred)
nrow(data9)
# write.csv(data9, file = "data/uncall.csv", row.names = F)
# data <- rbind(pre2019 %>% filter((PitcherTeam %in% c("NOR_TAR", "NOR_TAR2") & Pitcher %in% ps$Name) | (BatterTeam %in% c("NOR_TAR", "NOR_TAR2") & Batter %in% hs$Name)),
#               data9)

unc.vs.opp.ev()

# 5. Update App

## Framing
framing.breakdowns(game.pred, opponent = opp)

hitters <- unique(game.pred[which(game.pred$BatterTeam == "NOR_TAR" & game.pred$PitchCall != "Undefined"),"Batter"])
pitchers <- unique(game.pred[which(game.pred$PitcherTeam == "NOR_TAR" & game.pred$PitchCall != "Undefined"),"Pitcher"])

message("These are the hitters:")
print(hitters)

message("These are the pitchers:")
print(pitchers)

# render.dashboard(hitters)
# render.dashboard.current(hitters, coach = F)
# 
# render_pitcher_app(pitchers)
# render_pitcher_app_current(pitchers)

# 6. Tag High Speed Video
compare.video.counts(cameraside = "Left", gameid = gameid, folder.path = paste0(camera.folder.path, camera.folder.prefix, " LF Cam"), byinning = F)

# 2. If not: TIME
# - tag start of inning thumbnails
# - compare.video.counts
# - remove extra videos/add dummy

rename.videos(folder.path = paste0(camera.folder.path, camera.folder.prefix, " LF Cam"), ids = dir(paste0(camera.folder.path, camera.folder.prefix, " LF Cam")), 
              new.ids = get.video.keys(gameid = gameid, cameraside = "Left"))

compare.video.counts(cameraside = "Right", gameid = gameid, folder.path = paste0(camera.folder.path, camera.folder.prefix, " RF Cam"), byinning = F)
# 2. If not: TIME
# - tag start of inning thumbnails
# - compare.video.counts
# - remove extra videos/add dummy
rename.videos(folder.path = paste0(camera.folder.path, camera.folder.prefix, " RF Cam"), ids = dir(paste0(camera.folder.path, camera.folder.prefix, " RF Cam")), 
              new.ids = get.video.keys(gameid = gameid, cameraside = "Right"))

















## Cleaning and Add to Database
process.tmgame <- function(filename, add.to.db, add.to.chillmedia){
  
  game <- slide.trackman.columns(read.csv(file = filename, stringsAsFactors = F))
  
                                  ####  Cleaning  ####
  
  game$warmup <- as.numeric(game$PitchCall == "Undefined")
  
  ## balls/strikes increment correctly
  
  ## KorBB is Strikeout/Walk
  
  ## OutsOnPlay includes all of them (strikeouts)
  
  ## compare w playbyplay
  
  ## check that handedness are consistent in game, w/ roster (careful w/ switch hitters)
  global.r
                                ####  Pitch Tagging   ####
  ##  Column For Is Tagged
  game$AutoPitchTagged <- 1

  ## can consider a something for opponents who have auto pitch tagging as their tagging
  is.tagged <- !any(nchar(game$TaggedPitchType) != 2 & game$PitcherTeam %in% c("NOR_TAR", "NOR_TAR2") & !is.na(game$RelSpeed) & game$PitchCall != "Undefined")
  
  while(any(nchar(game$TaggedPitchType) != 2 & game$PitcherTeam %in% c("NOR_TAR", "NOR_TAR2") & !is.na(game$RelSpeed) & game$PitchCall != "Undefined")){
    retag <- yn("There are some untagged UNC pitches, would you like to tag them?")
    if(retag){
      game <- tag.pitches(game)[[1]] # filter this to just pitchers w/ untagged pitches
      is.tagged <- !any(nchar(game$TaggedPitchType) != 2 & game$PitcherTeam %in% c("NOR_TAR", "NOR_TAR2") & !is.na(game$RelSpeed) & game$PitchCall != "Undefined")
    } else{
      is.tagged <- yn("Do you want to label this data as tagged?")
      break
    }
    
  }
  
  
  game$PitchTagged <- as.numeric(is.tagged)
  
  therows <- which(game$AutoPitchType %in% c("FB", "CH", "BB") & nchar(game$TaggedPitchType) != 2 & !is.element(game$PitcherTeam, c("NOR_TAR", "NOR_TAR2")))
  if(length(therows) > 0) game[therows,"TaggedPitchType"] <- game[therows, "AutoPitchType"]
  

                                            ####  Save Data   ####
  if(missing(add.to.db)) add.to.db <- yn("Add data to database")
  
  if(add.to.db){
    dbWriteTable(tarconn, "dataimport", value = game, row.names = F, append = T)
    dbSendQuery(tarconn, "INSERT INTO `tm_staging` SELECT * FROM `dataimport`")
    dbSendQuery(tarconn, "DROP TABLE `dataimport`")
  }
  
  if(missing(add.to.chillmedia)) add.to.chillmedia <- yn("Add data to chillmedia?")
  
  if(add.to.chillmedia) data <- saveRDS(rbind(readRDS("data/uncall.RDS"), game), file = "data/uncall.RDS")
  
  ## make postgame reports
  
}

## cleaning -- balls and strikes increment correctly, everything matches box score, handednesses/teams

## TTO (p vs. b), AB_Num

## link w/ video