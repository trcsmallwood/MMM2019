##MMM_Scoring
##Calculate scores and summarise by round
##NOTE: Incomplete matches should be labelled "<<INSERT>>" in master brakcet

#Read in dataframe of predictions and results
MMM_df <- read.csv("../Submissions/MMM2019_PredictionsSummary.csv", stringsAsFactors = F)

MMM_df$Round <- ordered(MMM_df$Round, levels = unique(MMM_df$Round))

#Remove incomplete contests NOTE: Incomplete matches should be labelled "<<INSERT>>" in master bracket
MMM_completed_df <- MMM_df[which(MMM_df$Outcome != "<<INSERT>>"),]

#Force factors in Round to be ordered correctly 
MMM_completed_df$Round <- ordered(MMM_completed_df$Round, levels = unique(MMM_df$Round))

#Extract participants names
participants <- names(MMM_completed_df)[-c(1:4)]

#Create output dataframe for whether each person's predictions are correct
correct_df <- data.frame("Match" = MMM_completed_df$Match, "Round" = MMM_completed_df$Round)

#Create output dataframe for the number of points each person recieves for each match
points_df <- data.frame("Match" = MMM_completed_df$Match, "Round" = MMM_completed_df$Round)

#Create output dataframe for the proportion of each person's predictions which are correct by round
round_correct_df <- data.frame("Round" = unique(MMM_completed_df$Round))

#Create output dataframe for number of points each person scored per round
round_points_df <- data.frame("Round" = unique(MMM_completed_df$Round))

#Determine which matches were correct and how many points each person scored by match and by round of each person in turn
for(p in 1:length(participants)){
  
  #Determine which matchs were correct
  correct_df[,2+p] <- MMM_completed_df$Outcome == MMM_completed_df[,4+p]
  
  #Calculate number of points scored per match
  points_df[,2+p] <- correct_df[,2+p] * MMM_completed_df$Points
  
  #Calculate proportion of predictions that were correct per round
  round_correct_df[,1+p] <- tapply(correct_df[,2+p], correct_df$Round, mean)[which(is.na(tapply(correct_df[,2+p], correct_df$Round, mean)*100) == F)]*100
  
  #Calcualte number of points per round
  round_points_df[,1+p] <- tapply(points_df[,2+p], MMM_completed_df$Round, sum)[which(is.na(tapply(points_df[,2+p], MMM_completed_df$Round, sum)) == F)]
  
}

#Name columns as participants
names(correct_df)[-c(1:2)] <- participants
names(points_df)[-c(1:2)] <- participants
names(round_correct_df)[-c(1)] <- participants
names(round_points_df)[-c(1)] <- participants

#If multiple matchs have been completed, calculate cumulative points by match and by round
if(nrow(points_df) > 1){
  cum_match_points <- data.frame("MatchNo" = seq(1, length(MMM_completed_df$Match)), apply(points_df[,-c(1,2)],2,cumsum))
  cum_round_points <- data.frame("Round" = round_points_df$Round, apply(round_points_df[,-1],2,cumsum))
}else{ #Otherwise adapt point and round dataframes
  cum_match_points <- data.frame("MatchNo" = 1, points_df[,-c(1,2)])
  cum_round_points <- round_points_df
}

#Sort participants by number of total number of points in the last row of cumulative points dataframe
total_cum_points <- cum_match_points[nrow(cum_match_points),-1]

#Put standings into dataframe along with ranking with the highest points ranked 1 and ties recieving the same ranking
standings_df <- data.frame("Participant" = names(total_cum_points),
                           "Points" = as.numeric(total_cum_points),
                           "Standings" = rank(-total_cum_points, ties.method = "min"),
                           row.names = NULL)

if(all(MMM_df$Round != "Final")){
  
  #Select contests which haven't been completed
  TBC_df <- MMM_df[which(MMM_df$Round > max(MMM_completed_df$Round)),] 
  
  #Create output dataframe for whether each person's predictions can still be correct
  potential_correct_df <- data.frame("Match" = TBC_df$Match, "Round" = TBC_df$Round)
  
  #Create output dataframe for how many points each participant could still get for each contest 
  potential_points_df <- data.frame("Match" = TBC_df$Match, "Round" = TBC_df$Round)
  
  for(p in 1:length(participants)){
    
    #Determine which remaining matches have predicted winners which are still in the contest
    potential_correct_df[,2+p] <- apply(
      sapply(TBC_df[,4+p], grepl,
             MMM_completed_df$Outcome[which(MMM_completed_df$Round == max(MMM_completed_df$Round))]),
      2, any)
    
    #Calculate number of points that can be scored in each match
    potential_points_df[,2+p] <- potential_correct_df[,2+p] * TBC_df$Points
    
  }
  
  #Name columns as participants
  names(potential_correct_df)[-c(1:2)] <- participants
  names(potential_points_df)[-c(1:2)] <- participants
  
  #Calculate the total number of potential points for each participant based on winners to date
  total_potential_points <- apply(potential_points_df[,-c(1,2)],2,sum)
  
  #Calculate total possible points over the rest of the competition
  standings_df$PotentialTotal <- standings_df$Points + total_potential_points

}

#Order dataframe by standings
standings_df <- standings_df[order(standings_df$Standings),]

#Output dataframes into relative dir
write.csv(correct_df, "../Results/MMM2019_MatchCorrectSummary.csv", row.names = F)
write.csv(points_df, "../Results/MMM2019_MatchPointsSummary.csv", row.names = F)
write.csv(round_correct_df, "../Results/MMM2019_RoundCorrectSummary.csv", row.names = F)
write.csv(round_points_df, "../Results/MMM2019_RoundPointsSummary.csv", row.names = F)
write.csv(cum_match_points, "../Results/MMM2019_CumulativeMatchPoints.csv", row.names = F)
write.csv(cum_round_points, "../Results/MMM2019_CumulativeRoundPoints.csv", row.names = F)
write.csv(standings_df, "../Results/MMM2019_Standings.csv", row.names = F)
