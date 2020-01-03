scrape.acc.stats <- function(hitter = T, pitcher = F, fielder = F, season = 2018, conference = T){
  x <- htmltab::htmltab(paste0("https://theacc.com/stats.aspx?path=baseball&year=", season, "&conf=", tolower(conference)), which = 4 + pitcher + fielder)
  x <- cbind(Player = unlist(lapply(strsplit(x$Player, " \\("), `[`, 1)), Team = str_sub(lapply(strsplit(x$Player, "\\("), `[`, 2), 1, -2), Season = season, 
             GP = as.numeric(unlist(lapply(strsplit(x[,3], "-"), `[`, 1))), GS = as.numeric(unlist(lapply(strsplit(x[,3], "-"), `[`, 2))), 
             x[,-c(1, 3)] %>% mutate_if(is.character, function(x)as.numeric(as.character(x))))
  x
}
hitter <- scrape.acc.stats()
pitcher8 <- scrape.acc.stats(pitcher = T)

htmltab::htmltab("https://theacc.com/stats.aspx?path=baseball&year=2018&conf=false", which = 4)

# all.acc <- read.csv(file = "../All ACC.csv", stringsAsFactors = F)
# all.acc <- rbind(all.acc$First.Team %>% strsplit(., split = "[-,]") %>% unlist() %>% trimws %>%
#   matrix(., ncol = 3, byrow = T) %>% as.data.frame() %>% cbind(., all.acc$Season, Team = 1),
#   all.acc$Second.Team %>% strsplit(., split = "[-,]") %>% unlist() %>% trimws %>%
#     matrix(., ncol = 3, byrow = T) %>% as.data.frame() %>% cbind(., all.acc$Season, Team = 2),
#   all.acc$Third.Team %>% strsplit(., split = "[-,]") %>% unlist() %>% trimws %>%
#     matrix(., ncol = 3, byrow = T) %>% as.data.frame() %>% cbind(., all.acc$Season, Team = 3))
# 
# colnames(all.acc) <- c("Pos", "Player", "School", "Season", "Team")
# write.csv(all.acc, file = "../All ACC 2016-18.csv", row.names = F)

all.acc <- read.csv(file = "../All ACC 2016-18.csv", stringsAsFactors = F)


both <- merge(hitter %>% dplyr::rename(School = "Team"), all.acc[which(all.acc$Season == 2018 & !endsWith(as.character(all.acc$Pos), "P")),], by = c("Player", "Season"), all.x = T, all.y = F)

pct.starters <- 1 - mean((both$GS * 2) >= both$GP)
pct.all.acc <- 1 - mean(!is.na(both$Team))
pct.1st.team <- 1 - mean(!is.na(both$Team) & both$Team == 1)

targets <- both %>%
  filter(!is.na(AVG)) %>%
  mutate(Hit = AVG, 'Reached Base' = `OB%`, Bases = `SLG%`, PA = AB + BB + HBP + SF + SH, 
         Walk = BB / PA, Strikeout = SO / PA, "Home Run" = HR) %>%
  select(Hit, "Reached Base", Bases, Walk, Strikeout, "Home Run") %>%
  summarise_all(function(x)paste(quantile(x, probs = c(pct.starters, pct.all.acc, pct.1st.team, 1, 1 - c(pct.starters, pct.all.acc, pct.1st.team, 1))), collapse = " "))

goals <- strsplit(as.character(targets[1,]), split = " ") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(., nrow = 6, byrow = T, dimnames = list(names(targets), c("start", "all.acc", "first.team", "poy", "V5", "V6", "V7", "V8")))

goals["Strikeout",c(1:4)] <- goals["Strikeout",c(5:8)]

goals <- goals[,c(1:4)]


atm <- dbGetQuery(tarconn, "SELECT * FROM `alltm`")
atm %<>% mutate(XCoord = c(cos(Angle * pi / 180) * ExitSpeed), 
                YCoord = c(sin(Angle * pi / 180) * ExitSpeed))
for(contact.type in c("barrel", "solid", "F&B", "topped", "under")){
  atm[which(!is.na(atm$XCoord)), contact.type] <- as.numeric(in.out(as.matrix(coords[which(coords$Bin == contact.type),c("X", "Y")]), as.matrix(atm[which(!is.na(atm$XCoord)),c("XCoord", "YCoord")])))
}
atm[which(!is.na(atm$XCoord)),"weak"] <- as.numeric(atm[which(!is.na(atm$XCoord)),"ExitSpeed"] < 60)

ag <- atm %>% filter(League == "ACC" & PitchCall == "InPlay") %>% 
  mutate(HardHit = barrel + solid + `F&B`, NotHardHit = topped + under + weak) %>%
  group_by(BatterTeam, Batter) %>% 
  summarise(IP = n(), "Hard Hit" = mean(HardHit / (HardHit + NotHardHit), na.rm = T)) %>%
  filter(IP >= 20)

goals <- rbind(goals, "Hard Hit" = quantile(ag$`Hard Hit`, probs = c(pct.starters, pct.all.acc, pct.1st.team, 1)))
goals <- cbind.data.frame(stat = rownames(goals), goals)

# write.csv(goals, file = "data/Batter Targets.csv", row.names = T)






######    PITCHER GOALS
pitcher <- read.csv(file = "../ACC Pitcher Stats.csv", stringsAsFactors = F)
pitcher %<>%
  mutate(School = gsub(str_sub(Players, -4, -2), pattern = "\\(", replacement = ""),
         GP = unlist(lapply(strsplit(GP.GS, split = "-"), `[`, 1)), GS = unlist(lapply(strsplit(GP.GS, split = "-"), `[`, 1)),
         GP = if_else(nchar(GP) == 3, match(GP, month.abb), as.integer(GP)), GS = if_else(nchar(GS) == 3, match(GS, month.abb), as.integer(GS)))


school.conv <- data.frame(FullSchool = sort(as.character(unique(all.acc$School))), School = as.character(c("BC", "CU", "DU", "FS", "GAT", "LOU", "UM", "ST", "NC", "ND", "UP", "VA", "VT", "WF")))

both <- pitcher %>%
  left_join(school.conv, by = "School") %>%
  mutate(Player = str_sub(Players, 1, ifelse(nchar(School) == 2, -6, -7))) %>%
  #dplyr::rename(School = "Team") %>%
  left_join(all.acc[which(all.acc$Season == 2018 & endsWith(as.character(all.acc$Pos), "P")),], by = c("Player"), all.x = T, all.y = F)


pct.starters <- mean(both$IP >= 15)
pct.all.acc <- mean(!is.na(both$Team))
pct.1st.team <- mean(!is.na(both$Team) & both$Team == 1)

pitcher.targets <- both %>%
  filter(IP >= 5) %>%
  mutate(BF = AB + BB + HBP,
         Strikeout = SO / BF,
         Out = 1 - ((H + BB) / BF)
         ) %>%
  select(Strikeout, Out) %>%
  summarise_all(function(x)paste(quantile(x, probs = 1 - c(pct.starters, pct.all.acc, pct.1st.team, 0)), collapse = " ")) %>%
  slice(1) %>%
  as.character() %>%
  strsplit(split = " ") %>%
  unlist() %>%
  matrix(ncol = 4, byrow = T, dimnames = list(NULL, c("start", "all.acc", "first.team", "poy"))) %>%
  rbind.data.frame(


both %>%
  filter(IP >= 5) %>%
  mutate(BF = AB + BB + HBP,
         "Home Run" = HR / BF,
         Walk = BB / BF,
         Hit = B.AVG
  ) %>%
  select(Walk, Hit) %>%
  summarise_all(function(x)paste(quantile(x, probs = c(pct.starters, pct.all.acc, pct.1st.team, 0.001)), collapse = " ")) %>%
  slice(1) %>%
  as.character() %>%
  strsplit(split = " ") %>%
  unlist() %>%
  matrix(ncol = 4, byrow = T, dimnames = list(NULL, c("start", "all.acc", "first.team", "poy")))
) %>%
  mutate(stat = c("Strikeout", "Out", "Walk", "Hit"))



# write.csv(pitcher.targets, file = "data/Pitcher Targets.csv", row.names = F)
