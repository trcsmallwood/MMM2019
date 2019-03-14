##MMM_Scoring
##Calculate scores and summarise by round
##NOTE: Incomplete matches should be labelled "<<INSERT>>" in master brakcet

#Read in dataframe of predictions and results
MMM_df <- read.csv("../Submissions/MMM2019_PredictionsSummary.csv", stringsAsFactors = F)

#Remove incomplete matchs. NOTE: Incomplete matches should be labelled "<<INSERT>>" in master bracket
MMM_df <- MMM_df[which(MMM_df$Outcome != "<<INSERT>>"),]

#Force factors in Round to be ordered correctly 
MMM_df$Round <- ordered(MMM_df$Round, levels = unique(MMM_df$Round))

#Extract participants names
participants <- names(MMM_df)[-c(1:4)]

#Create output dataframe for whether each person's predictions are correct
correct_df <- data.frame("Match" = MMM_df$Match, "Round" = MMM_df$Round)

#Create output dataframe for the number of points each person recieves for each match
points_df <- data.frame("Match" = MMM_df$Match, "Round" = MMM_df$Round)

#Create output dataframe for the proportion of each person's predictions which are correct by round
round_correct_df <- data.frame("Round" = unique(MMM_df$Round))

#Create output dataframe for number of points each person scored per round
round_points_df <- data.frame("Round" = unique(MMM_df$Round))

#Determine which matches were correct and how many points each person scored by match and by round ofr each person in turn
for(p in 1:length(participants)){
  
  #Determine which matchs were correct
  correct_df[,2+p] <- MMM_df$Outcome == MMM_df[,4+p]
  
  #Calculate number of points scored per match
  points_df[,2+p] <- correct_df[,2+p] * MMM_df$Points
  
  #Calculate proportion of predictions that were correct per round
  round_correct_df[,1+p] <- tapply(correct_df[,2+p], correct_df$Round, mean)*100
  
  #Calcualte number of points per round
  round_points_df[,1+p] <- tapply(points_df[,2+p], points_df$Round, sum)
  
}

#Name columns as participants
names(correct_df)[-c(1:2)] <- participants
names(points_df)[-c(1:2)] <- participants
names(round_correct_df)[-c(1)] <- participants
names(round_points_df)[-c(1)] <- participants

#If multiple matchs have been completed, calculate cumulative points by match and by round
if(nrow(points_df) > 1){
  cum_match_points <- data.frame("MatchNo" = seq(1, length(MMM_df$Match)), apply(points_df[,-c(1,2)],2,cumsum))
  cum_round_points <- data.frame("Round" = round_points_df$Round, apply(round_points_df[,-1],2,cumsum))
}else{ #Otherwise adapt point and round dataframes
  cum_match_points <- data.frame("MatchNo" = 1, points_df[,-c(1,2)])
  cum_round_points <- round_points_df
}

#Sort participants by number of total number of points in the last row of cumulative points dataframe
standings <- sort(cum_match_points[nrow(cum_match_points),-1], decreasing = T)

#Put standings into dataframe along with ranking with the highest points ranked 1 and ties recieving the same ranking
standings_df <- data.frame("Participant" = names(standings), "Points" = as.numeric(standings), "Standings" = rank(-standings, ties.method = "min"))

#Output dataframes into relative dir
write.csv(correct_df, "../Results/MMM2019_MatchCorrectSummary.csv", row.names = F)
write.csv(points_df, "../Results/MMM2019_MatchPointsSummary.csv", row.names = F)
write.csv(round_correct_df, "../Results/MMM2019_RoundCorrectSummary.csv", row.names = F)
write.csv(round_points_df, "../Results/MMM2019_RoundPointsSummary.csv", row.names = F)
write.csv(cum_match_points, "../Results/MMM2019_CumulativeMatchPoints.csv", row.names = F)
write.csv(cum_round_points, "../Results/MMM2019_CumulativeRoundPoints.csv", row.names = F)
write.csv(standings_df, "../Results/MMM2019_Standings.csv", row.names = F)
