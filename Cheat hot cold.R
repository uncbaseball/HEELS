
Freeman <- data[which(data$Batter == "Tessar, Dallas"),]
y <- seq(0.5, 5, length.out=50)
x <- seq(-1.5, 1.5, length.out=50)






#fit <- gam(Hit ~ s(PlateLocSide_Catcher, PlateLocHeight), data=Freeman)

Freeman <- Freeman[which(Freeman$PitchCall == "InPlay" & !is.na(Freeman$PlateLocHeight) & (endsWith(Freeman$GameID, "8") | endsWith(Freeman$GameID, "9")) & Freeman$Scrimmage != "Live BP"),]#!is.na(Freeman$Bases) & 

fit <- loess(Bases ~ PlateLocSide_Catcher, PlateLocHeight, data=Freeman, span = .4)#, degree = 2, nk = 40)#, pmethod = "none")
  #PlateLocHeight PlateLocSide_Catcher Bases
  
data.predict <- data.frame(PlateLocSide_Catcher = c(outer(x, y * 0 + 1)), PlateLocHeight = c(outer(x * 0 + 1, y)))

lp <- predict(fit, data.predict)
data.predict <- cbind(data.predict, unname(lp))


data %>% filter(Batter %in% hs$Name & PitchCall == "InPlay") %>% 
  group_by(Batter) %>% summarise(sum(PlayResult %in% c("HomeRun", "Double"), na.rm = T))
hist(Freeman$ExpSLG)

hitterhc <- data.frame()

for(b in hs$Name[-c(1, 6, 14, 15, 18, 19)]){
  redo <- T
  while(redo){
    plot.pitches(data[which(data$Batter == b & data$PlayResult %in% c("Double", "Triple", "HomeRun") & (endsWith(data$GameID, "8") | endsWith(data$GameID, "9"))),],
                 pitch.numbers = F, plot.title = paste(b, "Hot Zone"))
    hc <- locator(n = 2, type = "p")
    redo <- yn("Would you like to redo it?")
  }
  hitterhc <- rbind(hitterhc, data.frame(Batter = b, PlateLocSide_Catcher = hc$x[1], PlateLocHeight = hc$y[2],
                                         radius = sqrt(diff(hc$x)^2 + diff(hc$y)^2)))
}


pdf("Hitter Hot Zones February 8th.pdf", onefile = T)
par(mfrow = c(2, 2), mar = c(.5, .5, .5, .5))
for(b in hs$Name[-c(1, 6, 14, 15, 18, 19)]){
  plot.pitches(data[which(data$Batter == b & data$PlayResult %in% c("Double", "Triple", "HomeRun") & (endsWith(data$GameID, "8") | endsWith(data$GameID, "9"))),],
               pitch.numbers = F, plot.title = paste(b, "Hot Zone"), legend.type = "none")
  symbols(hitterhc[which(hitterhc$Batter == b),c(2, 3)], circles = hitterhc[which(hitterhc$Batter == b),"radius"], bg = adjustcolor("red", 0.5), add = T, inches = F)
}
dev.off()

