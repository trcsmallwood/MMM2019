##MMM2019_PairwiseSimilarity
##Quantify and plot the relative similarity of different participants predictions

library(reshape2)
library(ggplot2)

#Read in dataframe of predictions
MMM_df <- read.csv("../Submissions/MMM2019_PredictionsSummary.csv", stringsAsFactors = F)

#Load in dataframe of participants
participants_df <- read.csv("../Submissions/Participants.csv", stringsAsFactors = F)

#Remove those who haven't submitted (or paid)
participants_df <- participants_df[which(participants_df$Submitted == "Y"),]# & participants_df$Paid == "Y")]

#Extract names of participants
participants <- participants_df$Participants

#Extract plot colours for participants
participant_cols <- participants_df$PreferredColour

#Create a matrix for unweighted pairwise comparisons of predictions with named columns and rows 
sim_mat <- matrix(NA, nrow = length(participants), ncol = length(participants))
row.names(sim_mat) <- participants
colnames(sim_mat) <- participants

#Create a matrix for point-weighted pairwise comparisons of predictions with named columns and rows 
sim_points_mat <- matrix(NA, nrow = length(participants), ncol = length(participants))
row.names(sim_points_mat) <- participants
colnames(sim_points_mat) <- participants

#Loop through participants
for(p in 1:length(participants)){
  
  #Extract participants' predictions
  tmp_preds <- MMM_df[,p+4]
  
  #Loop through other participants
  for(a in (1:length(participants))[-p]){
    
    #Calculate proportion of predictions which are the same and assign to matrix
    sim_mat[p,a] <- sum(tmp_preds == MMM_df[,a+4])/length(tmp_preds)
    
    #Calculate the proportion of predictions which are the same weighted by point value and assign to matrix
    sim_points_mat[p,a] <- sum((tmp_preds == MMM_df[,a+4])*MMM_df$Points)/sum(MMM_df$Points)
    
  }
}

#Create vectors for mean similarities (except Random)
mean_sim <- rep(NA, length(participants[-1]))
mean_sim_points <- rep(NA, length(participants[-1]))

#Calculate mean similarity (unweighted and weighted) for each participant
for(p in 1:length(participants[-1])){
  mean_sim[p] <- mean(sim_mat[p+1,], na.rm = T)
  mean_sim_points[p] <- mean(sim_points_mat[p+1,], na.rm = T)
}

#Combine into data frame
sim_df <- data.frame("Participants" = participants[-1],
                     "Similarity" = mean_sim,
                     "Round-Weighted Similarity" = mean_sim_points)

#Order by weighted similarity
sim_df <- sim_df[order(sim_df$Round.Weighted.Similarity, decreasing = T),]

#Output as a .csv file
write.csv(sim_df, "../Results/MMM2019_MeanSimilarity.csv", row.names = F)

#Reshape into dataframe for ploting and name columns
sim_mat_plot <- melt(sim_mat)
names(sim_mat_plot) <- c("Participant1", "Participant2", "Similarity")

#Reshape into dataframe for ploting and name columns
sim_points_mat_plot <- melt(sim_points_mat)
names(sim_points_mat_plot) <- c("Participant1", "Participant2", "Similarity")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_Similarity.pdf", width=11.69, height=8.27)

#Plot unweighted similarity of predictions in grid
ggplot(sim_mat_plot, aes(x = Participant1, y = ordered(sim_mat_plot$Participant2, levels = rev(participants)), fill = Similarity)) +
  geom_tile() + #Grid layout
  geom_text(aes(Participant1, ordered(sim_mat_plot$Participant2, levels = rev(participants)),
                label = ifelse(is.na(Similarity), "", formatC(Similarity, digits = 2, format = "f"))),
            color = "white", size = 4) + #Similarity score to 2 sig fig, exclude NAs
  theme_bw() + #Simple balck and white base theme
  theme(axis.ticks.length = unit(0, "cm"), #Ticks marks inside
        axis.text.x = element_text(size = 12, margin=margin(10,10,10,10,"pt"), colour = participant_cols, angle = 45, hjust = 0), #x axis text size and spacing, rotate 45 degrees
        axis.text.y = element_text(size = 12, margin=margin(10,10,10,10,"pt"), colour = rev(participant_cols)), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_blank(), axis.line.y = element_blank(), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "right", #Legend postion
        plot.margin = unit(c(0.5,0.2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + #Legend title and text size
  scale_fill_gradientn( colours = c("gold", "orange", "orangered", "red", "darkred"), limits = c(0,0.9), breaks = seq(0,1,0.2)) + #Scale from blue to red and from 0.1 to 0.9
  scale_x_discrete(position = "top", name = " ") + #x axis at top and without name
  scale_y_discrete(name = " ") + #y axis no name
  NULL

#Close .pdf
dev.off()

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_SimilarityRoundWeighted.pdf", width=11.69, height=8.27)

#Plot round-weighted similarity of predictions in grid
ggplot(sim_points_mat_plot, aes(x = Participant1, y = ordered(sim_mat_plot$Participant2, levels = rev(participants)), fill = Similarity)) +
  geom_tile() + #Grid layout
  geom_text(aes(Participant1, ordered(sim_mat_plot$Participant2, levels = rev(participants)),
                label = ifelse(is.na(Similarity), "", formatC(Similarity, digits = 2, format = "f"))),
            color = "white", size = 4) + #Similarity score to 2 sig fig, exclude NAs
  theme_bw() + #Simple balck and white base theme
  theme(axis.ticks.length = unit(0, "cm"), #Ticks marks inside
        axis.text.x = element_text(size = 12, margin=margin(10,10,10,10,"pt"), colour = participant_cols, angle = 45, hjust = 0), #x axis text size and spacing, rotate 45 degrees
        axis.text.y = element_text(size = 12, margin=margin(10,10,10,10,"pt"), colour = rev(participant_cols)), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_blank(), axis.line.y = element_blank(), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "right", #Legend postion
        plot.margin = unit(c(0.5,0.2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + #Legend title and text size
  scale_fill_gradientn(expr("Round-\nWeighted\nSimilarity"), colours = c("gold", "orange", "orangered", "red", "darkred"), limits = c(0,0.9), breaks = seq(0,1,0.2)) + #Scale from blue to red and from 0.1 to 0.9
  scale_x_discrete(position = "top", name = "") + #x axis at top and without name
  scale_y_discrete(name = "") + #y axis no name
  NULL

#Close .pdf
dev.off()
