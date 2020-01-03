acc_team_p <- htmltab::htmltab("http://theacc.com/stats.aspx?path=baseball&year=2019&conf=true", which = 2)
acc_team_p$K <- as.numeric(substr(acc_team_p$SO, 1, 3))
acc_team_p$KS <- as.numeric(str_sub(acc_team_p$SO, -2, -1))

acc_team_p$IP <- as.numeric(gsub(gsub(acc_team_p$IP, pattern = "2$", replacement = "6"), pattern = "1$", replacement = "3"))

for(i in c("K", "BB", "HR")) acc_team_p[,i] <- as.numeric(acc_team_p[,i]) / acc_team_p$IP


mod <- lm(ERA ~ K + BB + HR, data = acc_team_p)
cbind(acc_team_p$Team, predict(mod, acc_team_p))
