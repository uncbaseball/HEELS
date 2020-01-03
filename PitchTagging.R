pitchtagging <- function(fulldata){
	
	uncps <- c("Aker", "Attianese", "Baum", "Bergner", "Butler", "Dalatri", "Gay", "Hiatt", "Hutchinson", "Odum", "Peto", "Rogo", "Sabato", "Welch")
	

	pitchtypes <- c("FB", "FB", "FC", "FS", "CH", "CU", "SL", "SI")
  fulldata$TaggedPitchType <- as.character(fulldata$TaggedPitchType)
	fulldata[,"TaggedPitchType"] <- c("Undefined")
	#levels(fulldata$TaggedPitchType) <- append("Undefined", pitchtypes)

	fulldata[which(fulldata$PitcherTeam != "NOR_TAR" & fulldata$PitcherTeam != "NOR_TAR2"), "TaggedPitchType"] <- c(fulldata[which(fulldata$PitcherTeam != "NOR_TAR" & fulldata$PitcherTeam != "NOR_TAR2"), "AutoPitchType"])
	
	if(is.element("Hutchison, Rodney", fulldata$Pitcher)){
	  fulldata[which(fulldata$Pitcher == "Hutchison, Rodney"), "Pitcher"] <- c("Hutchison Jr., Rodney")
	  warning("'Hutchison Jr., Rodney' was changed to 'Hutchison, Rodney'")
	}

	if(is.element(c("Aker, Cole"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Aker, Cole" & fulldata$HorzBreak < -5), "TaggedPitchType"] <- c("CU")	
		fulldata[which(fulldata$Pitcher == "Aker, Cole" & fulldata$HorzBreak > -5  & fulldata$VertBreak > -30), "TaggedPitchType"] <- c("FB")
		fulldata[which(fulldata$Pitcher == "Aker, Cole" & fulldata$HorzBreak > -5 & fulldata$VertBreak < -30), "TaggedPitchType"] <- c("CH")		
		fulldata[which(fulldata$Pitcher == "Aker, Cole" & fulldata$HorzBreak > 5 & fulldata$VertBreak < -35), "TaggedPitchType"] <- c("SL")
		#### SL tagging will pick up lots of CH's		
		
	}
	
	
	if(is.element(c("Attianese, Zach"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Attianese, Zach" & fulldata$RelSpeed > 84 & fulldata$SpinAxis > 115 & fulldata$SpinAxis < 180 & fulldata$HorzBreak < 1 & fulldata$HorzBreak > -20 & fulldata$VertBreak < 0 & fulldata$VertBreak > -35), "TaggedPitchType"] <- c("FB")
	
		fulldata[which(fulldata$Pitcher == "Attianese, Zach" & fulldata$RelSpeed < 76 & fulldata$SpinAxis > 270 & fulldata$HorzBreak > 2 & fulldata$VertBreak < -50), "TaggedPitchType"] <- c("CU")

		fulldata[which(fulldata$Pitcher == "Attianese, Zach" & fulldata$RelSpeed > 85 & fulldata$SpinAxis > 100 & fulldata$SpinAxis < 155 & fulldata$HorzBreak < 0 & fulldata$HorzBreak > -20 & fulldata$VertBreak > -30 & fulldata$VertBreak < -10), "TaggedPitchType"] <- c("FB")		
		
		fulldata[which(fulldata$Pitcher == "Attianese, Zach" & fulldata$RelSpeed > 75.5 & fulldata$RelSpeed < 83 & fulldata$SpinAxis < 200 & fulldata$HorzBreak > -55 & fulldata$HorzBreak < 0 & fulldata$VertBreak < 0 & fulldata$VertBreak > -25), "TaggedPitchType"] <- c("CH")		
		
	}
	
	
	
	if(is.element(c("Baum, Tyler"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Baum, Tyler" & fulldata$RelSpeed > 87), "TaggedPitchType"] <- c("FB")
		fulldata[which(fulldata$Pitcher == "Baum, Tyler" & fulldata$RelSpeed < 86 & fulldata$HorzBreak > 7), "TaggedPitchType"] <- c("CH")		
		fulldata[which(fulldata$Pitcher == "Baum, Tyler" & fulldata$RelSpeed < 85 & fulldata$HorzBreak < 6), "TaggedPitchType"] <- c("CU")		
		
	}	
	
	
	if(is.element(c("Bergner, Austin"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Bergner, Austin" & fulldata$HorzBreak < 2), "TaggedPitchType"] <- c("CU")	
		fulldata[which(fulldata$Pitcher == "Bergner, Austin" & fulldata$HorzBreak > 2 & fulldata$VertBreak < -23), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Bergner, Austin" & fulldata$HorzBreak > 2 & fulldata$VertBreak > -30 & fulldata$VertBreak < -25 & fulldata$RelSpeed > 88), "TaggedPitchType"] <- c("FB")				
		fulldata[which(fulldata$Pitcher == "Bergner, Austin" & fulldata$HorzBreak > 2 & fulldata$VertBreak > -25 & fulldata$RelSpeed > 88), "TaggedPitchType"] <- c("FB")		
		
	}	
	
	if(is.element("Blendinger, Kyle", fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Blendinger, Kyle" & fulldata$HorzBreak < 3 & fulldata$RelSpeed < 82), "TaggedPitchType"] <- c("SL")
		fulldata[which(fulldata$Pitcher == "Blendinger, Kyle" & fulldata$HorzBreak > 3 & fulldata$RelSpeed < 82), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Blendinger, Kyle" & fulldata$RelSpeed > 82), "TaggedPitchType"] <- c("FB")
		
	}
	
	if(is.element("Bukauskas, J.B.", fulldata$Pitcher)){
		fulldata[which(fulldata$RelSpeed > 89 & fulldata$Pitcher == "Bukauskas, J.B."), "TaggedPitchType"] <- c("FB")
		fulldata[which(fulldata$RelSpeed < 89 & fulldata$HorzBreak > 7 & fulldata$Pitcher == "Bukauskas, J.B."), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$RelSpeed < 89 & fulldata$HorzBreak < 5 & fulldata$Pitcher == "Bukauskas, J.B."), "TaggedPitchType"] <- c("SL")
		
	}
		
	if(is.element(c("Butler, Hansen"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Butler, Hansen" & fulldata$RelSpeed > 87), "TaggedPitchType"] <- c("FB")
		fulldata[which(fulldata$Pitcher == "Butler, Hansen" & fulldata$RelSpeed < 85 & fulldata$HorzBreak > 4), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Butler, Hansen" & fulldata$RelSpeed < 80 & fulldata$HorzBreak < -3), "TaggedPitchType"] <- c("CU")

	}
	
	if(is.element("Casparius, Ben", fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Casparius, Ben" & fulldata$HorzBreak < 0 & fulldata$RelSpeed < 85), "TaggedPitchType"] <- c("CU")
		fulldata[which(fulldata$Pitcher == "Casparius, Ben" & fulldata$HorzBreak > 0 & fulldata$RelSpeed < 85), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Casparius, Ben" & fulldata$RelSpeed > 85), "TaggedPitchType"] <- c("FB")
			
	}
	
	if(is.element("Criswell, Cooper", fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Criswell, Cooper" & fulldata$HorzBreak < -5), "TaggedPitchType"] <- c("CU")
		fulldata[which(fulldata$Pitcher == "Criswell, Cooper" & fulldata$HorzBreak > 5 & fulldata$RelSpeed < 82), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Criswell, Cooper" & fulldata$RelSpeed >= 82), "TaggedPitchType"] <- c("FB")
	}
	
	if(is.element(c("Dalatri, Luca"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Dalatri, Luca" & fulldata$RelSpeed > 85.5 & fulldata$HorzBreak > 0), "TaggedPitchType"] <- c("FB")
		fulldata[which(fulldata$Pitcher == "Dalatri, Luca" & fulldata$RelSpeed < 85.5 & fulldata$HorzBreak > 3), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Dalatri, Luca" & fulldata$RelSpeed < 79 & fulldata$HorzBreak < 0), "TaggedPitchType"] <- c("CU")
		
	}
	
	## Dancy
	fulldata[which(fulldata$Pitcher == "Dancy, John" & fulldata$RelSpeed > 84), "TaggedPitchType"] <- c("FB")
	fulldata[which(fulldata$Pitcher == "Dancy, John" & fulldata$RelSpeed < 84 & fulldata$RelSpeed > 74), "TaggedPitchType"] <- c("SL")
	fulldata[which(fulldata$Pitcher == "Dancy, John" & fulldata$RelSpeed < 74), "TaggedPitchType"] <- c("CU")
	
	
	## Daniels
	fulldata[which(fulldata$HorzBreak < 0 & fulldata$Pitcher == "Daniels, Brett"), "TaggedPitchType"] <- c("SL")
	fulldata[which(fulldata$HorzBreak > 0 & fulldata$RelSpeed < 82 & fulldata$Pitcher == "Daniels, Brett"), "TaggedPitchType"] <- c("CH")
	fulldata[which(fulldata$RelSpeed > 84 & fulldata$Pitcher == "Daniels, Brett"), "TaggedPitchType"] <- c("FB")
	
	

	if(is.element(c("Gay, Trevor"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Gay, Trevor" & fulldata$HorzBreak < 5), "TaggedPitchType"] <- c("SL")		
		fulldata[which(fulldata$Pitcher == "Gay, Trevor" & fulldata$HorzBreak > 5 & fulldata$VertBreak < -42), "TaggedPitchType"] <- c("CH")		
		fulldata[which(fulldata$Pitcher == "Gay, Trevor" & fulldata$HorzBreak > 5 & fulldata$VertBreak < -31 & fulldata$VertBreak > -42), "TaggedPitchType"] <- c("FB")				
		fulldata[which(fulldata$Pitcher == "Gay, Trevor" & fulldata$HorzBreak > 5 & fulldata$VertBreak > -31), "TaggedPitchType"] <- c("FB")		
		
	}
	
	
	if(is.element(c("Hiatt, Josh"), fulldata$Pitcher)){

		fulldata[which(fulldata$Pitcher == "Hiatt, Josh" & fulldata$RelSpeed > 86), "TaggedPitchType"] <- c("FB")
		fulldata[which(fulldata$Pitcher == "Hiatt, Josh" & fulldata$RelSpeed < 86 & fulldata$RelSpeed > 82.5), "TaggedPitchType"] <- c("CH")		
		fulldata[which(fulldata$Pitcher == "Hiatt, Josh" & fulldata$RelSpeed < 78), "TaggedPitchType"] <- c("SL")	
		
	}
	
		
	if(is.element(c("Hutchison Jr., Rodney"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Hutchison Jr., Rodney" & fulldata$RelSpeed > 88 & fulldata$HorzBreak > 8.5), "TaggedPitchType"] <- c("FB")		
		fulldata[which(fulldata$Pitcher == "Hutchison Jr., Rodney" & fulldata$RelSpeed < 88 & fulldata$HorzBreak > 11), "TaggedPitchType"] <- c("CH")		
		fulldata[which(fulldata$Pitcher == "Hutchison Jr., Rodney" & fulldata$RelSpeed > 84 & fulldata$RelSpeed < 91 & fulldata$HorzBreak < 10 & fulldata$HorzBreak > -5), "TaggedPitchType"] <- c("SL")		
		fulldata[which(fulldata$Pitcher == "Hutchison Jr., Rodney" & fulldata$RelSpeed > 81 & fulldata$RelSpeed < 82.5 & fulldata$HorzBreak < -7), "TaggedPitchType"] <- c("CU")
		
	}	
	
	if(is.element("Lancellotti, Joey", fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Lancellotti, Joey" & fulldata$RelSpeed < 85 & fulldata$HorzBreak < 2.5), "TaggedPitchType"] <- c("SL")
		fulldata[which(fulldata$Pitcher == "Lancellotti, Joey" & fulldata$RelSpeed < 85 & fulldata$HorzBreak > 2.5), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Lancellotti, Joey" & fulldata$RelSpeed > 85), "TaggedPitchType"] <- c("FB")
		
	}
	
	## Love
	fulldata[which(fulldata$Pitcher == "Love, Austin" & fulldata$RelSpeed > 83), "TaggedPitchType"] <- c("FB")
	fulldata[which(fulldata$Pitcher == "Love, Austin" & fulldata$RelSpeed < 83 & fulldata$InducedVertBreak > 6), "TaggedPitchType"] <- c("CH")
	fulldata[which(fulldata$Pitcher == "Love, Austin" & fulldata$RelSpeed < 83 & fulldata$InducedVertBreak < 6 & fulldata$InducedVertBreak > 2), "TaggedPitchType"] <- c("SL")
	fulldata[which(fulldata$Pitcher == "Love, Austin" & fulldata$RelSpeed < 83 & fulldata$InducedVertBreak < 2), "TaggedPitchType"] <- c("CU")
	
	
	## Morgan
	fulldata[which(fulldata$Pitcher == "Morgan, Jason" & fulldata$RelSpeed > 87), "TaggedPitchType"] <- c("FB")
	
	fulldata[which(fulldata$Pitcher == "Morgan, Jason" & fulldata$RelSpeed > 78 & fulldata$RelSpeed < 83 & fulldata$HorzBreak > 10), "TaggedPitchType"] <- c("CH")
	
	fulldata[which(fulldata$Pitcher == "Morgan, Jason" & fulldata$RelSpeed < 86 & fulldata$HorzBreak < 10), "TaggedPitchType"] <- c("SL")
	
	if(is.element("O'Brien, Caden", fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "O'Brien, Caden" & fulldata$HorzBreak < -1 & fulldata$RelSpeed < 79), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "O'Brien, Caden" & fulldata$HorzBreak > -1 & fulldata$RelSpeed < 75), "TaggedPitchType"] <- c("SL")
		fulldata[which(fulldata$Pitcher == "O'Brien, Caden" & fulldata$RelSpeed > 79), "TaggedPitchType"] <- c("FB")
		 
	}
		
	
	
	if(is.element(c("Odum, Evan"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Odum, Evan" & fulldata$HorzBreak < 0 & fulldata$VertBreak < -38), "TaggedPitchType"] <- c("CU")

		fulldata[which(fulldata$Pitcher == "Odum, Evan" & fulldata$VertBreak > -35), "TaggedPitchType"] <- c("FB")		
		
		fulldata[which(fulldata$Pitcher == "Odum, Evan" & fulldata$VertBreak < -35 & fulldata$HorzBreak > 0), "TaggedPitchType"] <- c("CH")		
		
	}
	
	
	if(is.element(c("Peto, Robbie"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Peto, Robbie" & fulldata$RelSpeed > 85.5 & fulldata$SpinAxis > 185 & fulldata$SpinAxis < 238 & fulldata$HorzBreak > 0 & fulldata$VertBreak > -35), "TaggedPitchType"] <- c("FB")
		
		fulldata[which(fulldata$Pitcher == "Peto, Robbie" & fulldata$RelSpeed > 85.5 & fulldata$SpinAxis > 215 & fulldata$SpinAxis < 250 & fulldata$HorzBreak > 5 & fulldata$VertBreak > -35), "TaggedPitchType"] <- c("FB")		

		fulldata[which(fulldata$Pitcher == "Peto, Robbie" & fulldata$RelSpeed > 77.5 & fulldata$RelSpeed < 82.5 & fulldata$SpinAxis < 303 & fulldata$HorzBreak < 15 & fulldata$VertBreak < -25), "TaggedPitchType"] <- c("SL")		
		
		fulldata[which(fulldata$Pitcher == "Peto, Robbie" & fulldata$RelSpeed > 72.5 & fulldata$RelSpeed < 82.5 & fulldata$HorzBreak < 5 & fulldata$VertBreak < -45), "TaggedPitchType"] <- c("CU")	
		
		fulldata[which(fulldata$Pitcher == "Peto, Robbie" & fulldata$RelSpeed > 76 & fulldata$RelSpeed < 84.5 & fulldata$SpinAxis > 206 & fulldata$SpinAxis < 275 & fulldata$HorzBreak > 7 & fulldata$VertBreak > -50 & fulldata$VertBreak < -20 ), "TaggedPitchType"] <- c("CH")				
		
	}
	

	if(is.element("Randolph, Jake", fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Randolph, Jake" & fulldata$HorzBreak > 2 & fulldata$RelSpeed < 77), "TaggedPitchType"] <- c("CU")
		fulldata[which(fulldata$Pitcher == "Randolph, Jake" & fulldata$HorzBreak < -5 & fulldata$RelSpeed < 80), "TaggedPitchType"] <- c("CH")
		fulldata[which(fulldata$Pitcher == "Randolph, Jake" & fulldata$RelSpeed > 80), "TaggedPitchType"] <- c("FB")
		
	}
	
	if(is.element(c("Sabato, Teddy"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Sabato, Teddy" & fulldata$RelSpeed > 80 & fulldata$SpinAxis > 170 & fulldata$SpinAxis < 240 & fulldata$HorzBreak > -2 & fulldata$HorzBreak < 20 & fulldata$VertBreak > -35), "TaggedPitchType"] <- c("FB")	
		fulldata[which(fulldata$Pitcher == "Sabato, Teddy" & fulldata$RelSpeed < 79 & fulldata$HorzBreak > 0), "TaggedPitchType"] <- c("SL")
		fulldata[which(fulldata$Pitcher == "Sabato, Teddy" & fulldata$RelSpeed < 80 & fulldata$SpinAxis < 183 & fulldata$HorzBreak < 0 & fulldata$VertBreak < -40), "TaggedPitchType"] <- c("CU")		
		fulldata[which(fulldata$Pitcher == "Sabato, Teddy" & fulldata$RelSpeed < 85 & fulldata$HorzBreak > 0), "TaggedPitchType"] <- c("CH")	
		fulldata[which(fulldata$Pitcher == "Sabato, Teddy" & fulldata$RelSpeed > 82 & fulldata$SpinAxis > 75 & fulldata$HorzBreak < 20 & fulldata$HorzBreak > 0 & fulldata$VertBreak > -35), "TaggedPitchType"] <- c("FB")		

	}	


	if(is.element(c("Sugg, Taylor"), fulldata$Pitcher)){
		fulldata[which(fulldata$Pitcher == "Sugg, Taylor" & fulldata$RelSpeed > 86.5 & fulldata$HorzBreak > 5 & fulldata$VertBreak > -30), "TaggedPitchType"] <- c("FB")	
		fulldata[which(fulldata$Pitcher == "Sugg, Taylor" & fulldata$RelSpeed > 86.5 & fulldata$SpinAxis >= 240 & fulldata$VertBreak > -40 & fulldata$HorzBreak > 5), "TaggedPitchType"] <- c("FB")	
		fulldata[which(fulldata$Pitcher == "Sugg, Taylor" & fulldata$RelSpeed > 78 & fulldata$RelSpeed < 86 & fulldata$HorzBreak > 5), "TaggedPitchType"] <- c("CH")	
		fulldata[which(fulldata$Pitcher == "Sugg, Taylor" & fulldata$RelSpeed < 81 & fulldata$SpinAxis < 140 & fulldata$HorzBreak < 5 & fulldata$VertBreak < -40), "TaggedPitchType"] <- c("SL")
		}
	

	fulldata[which(fulldata$Pitcher == "Weiss, Bo" & fulldata$RelSpeed > 87 & fulldata$HorzBreak > -1), "TaggedPitchType"] <- c("FB")
	fulldata[which(fulldata$Pitcher == "Weiss, Bo" & fulldata$RelSpeed < 84 & fulldata$RelSpeed > 80 & fulldata$HorzBreak > 4), "TaggedPitchType"] <- c("CH")
	fulldata[which(fulldata$Pitcher == "Weiss, Bo" & fulldata$RelSpeed > 83 & fulldata$RelSpeed < 87.5 & fulldata$HorzBreak < -1), "TaggedPitchType"] <- c("SL")
	fulldata[which(fulldata$Pitcher == "Weiss, Bo" & fulldata$RelSpeed < 78 & fulldata$HorzBreak < -4), "TaggedPitchType"] <- c("CU")
	
	## Zarate
	fulldata[which(fulldata$Pitcher == "Zarate, Angel" & fulldata$HorzBreak > -5 & fulldata$RelSpeed < 75), "TaggedPitchType"] <- c("CU")
	fulldata[which(fulldata$Pitcher == "Zarate, Angel" & fulldata$HorzBreak < 0 & fulldata$RelSpeed > 75 & fulldata$RelSpeed < 80), "TaggedPitchType"] <- c("CH")
	fulldata[which(fulldata$Pitcher == "Zarate, Angel" & fulldata$RelSpeed > 80), "TaggedPitchType"] <- c("FB")
		
	
		
	fulldata[which(fulldata$PitchCall == "BallIntentional"), "TaggedPitchType"] <- c("IntentionalBall")
	
	
	return(fulldata)	
	
}
