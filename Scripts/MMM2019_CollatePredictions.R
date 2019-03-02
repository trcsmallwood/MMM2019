##MMM2019_CollatePredictions
##Collate all submitted predictions into a single dataframe
##NOTE: Submitted and Paid in participants dataframe should be marked Y or N

#Read in dataframe of participants from relative pathway .csv
participants_df <- read.csv("../Submissions/Participants.csv", stringsAsFactors = F)

#Select names of participants who have submitted brackets and paid
participants <- participants_df$Participants[which(participants_df$Submitted == "Y" & participants_df$Paid == "Y")]

#Load in master bracket of results
MMM_df <- read.csv("../Submissions/MMM2019_MASTER.csv", stringsAsFactors = F)

#Read in each participants bracket submissions from their name
for(p in 1:length(participants)){
  
  #Read .csv
  tmp_predictions <- read.csv(paste("../Submissions/MMM2019_RESULTS_", participants[p], ".csv", sep = ""), stringsAsFactors = F)
  
  #Add predictions to dataframe
  MMM_df[,4+p] <- tmp_predictions$Prediction
  
  #Name column with participants name
  names(MMM_df)[4+p] <- participants[p]
  
}

#Output dataframe to relative pathway dir
write.csv(MMM_df, "../Submissions/MMM2019_PredictionsSummary.csv", row.names = F)
