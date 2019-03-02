##MMM2019_Plotting
##Plot participants results

#Load in packages
library(reshape2)
library(ggplot2)

#Load in preferred colours of participants
participant_cols <- read.csv("../Submissions/Participants.csv", stringsAsFactors = F)$PreferredColour

#Load in cumulative points by match
cum_match_points_df <- read.csv("../Results/MMM2019_CumulativeMatchPoints.csv")

#Reshape dataframe for plotting
cum_match_points_plot_df <- melt(cum_match_points_df, id.vars = "MatchNo", variable.name = "Participant", value.name = "Points")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_CumulativeMatchPoints.pdf", width=11.69, height=8.27)

#Plot cumulative points of each participant
ggplot(cum_match_points_plot_df, aes(x = MatchNo, y = Points, col = Participant)) +
  geom_line() + #Plot as lines
  theme_bw() + #Simple balck and white base theme
  #guides(col = guide_legend(nrow = 2, byrow = T)) + #Control number of rows in lengend
  theme(axis.ticks.length = unit(-0.2, "cm"), #Ticks marks inside
        axis.text.x = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #x axis text size and spacing
        axis.text.y = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_line(size = 0.5, color = "black"), axis.line.y = element_line(size = 0.5, color = "black"), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "bottom", #Legend postion
        plot.margin = unit(c(0.5,0.2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + #Legend title and text size
  scale_x_continuous("Match Number", limits = c(0,64), expand = c(0,0)) + #x axis title and limits. Set for a max of 64 matchs
  scale_y_continuous("Cumulative Points", limits = c(0, max(cum_match_points_plot_df$Points)*1.1), expand = c(0,0)) + #y axis title and limits. Max set to 10% more than the highest score. Max points available is 138
  scale_color_manual("Participant", values = participant_cols) + #Legend title and colour by preferred colours
  NULL

#Close .pdf
dev.off()

#Read in cumulative point per round
cum_round_points_df <- read.csv("../Results/MMM2019_CumulativeRoundPoints.csv")

#Enforce ordering of rounds
cum_round_points_df$Round <- ordered(cum_round_points_df$Round, levels = unique(cum_round_points_df$Round))

#Reshape dataframe for plotting
cum_round_points_plot_df <- melt(cum_round_points_df, id.vars = "Round", variable.name = "Participant", value.name = "Points")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_CumulativeRoundPoints.pdf", width=11.69, height=8.27)

#Plot cumulative points per round
ggplot(cum_round_points_plot_df, aes(x = Round, y = Points, col = Participant, group = Participant)) +
  geom_point() + #Plot as points
  geom_line() + #Plot as lines
  theme_bw() + #Simple balck and white base theme
  #guides(col = guide_legend(nrow = 2, byrow = T)) + #Control number of rows in lengend
  theme(axis.ticks.length = unit(-0.2, "cm"), #Ticks marks inside
        axis.text.x = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #x axis text size and spacing
        axis.text.y = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_line(size = 0.5, color = "black"), axis.line.y = element_line(size = 0.5, color = "black"), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "bottom", #Legend postion
        plot.margin = unit(c(0.5,0.2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + #Legend title and text size
  scale_y_continuous("Cumulative Points", limits = c(0, max(cum_match_points_plot_df$Points)*1.1), expand = c(0,0)) + #y axis title and limits. Max set to 10% more than the highest score. Max points available is 138
  scale_x_discrete("Round", drop = F) + #x axis title and include all rounds, not just those with scores
  scale_color_manual("Participant", values = participant_cols) + #Legend title and colour by preferred colours
  NULL

#Close .pdf
dev.off()

#Read in proportion correct per round
round_correct_df <- read.csv("../Results/MMM2019_RoundCorrectSummary.csv")

#Enforce ordering of rounds
round_correct_df$Round <- ordered(round_correct_df$Round, levels = unique(round_correct_df$Round))

#Reshape dataframe for plottin
round_correct_plot_df <- melt(round_correct_df, id.vars = "Round", variable.name = "Participant", value.name = "PropCorrect")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_RoundPropCorrect.pdf", width=11.69, height=8.27)

#Plot proportion correct per round
ggplot(round_correct_plot_df, aes(x = Round, y = PropCorrect, fill = Participant)) +
  geom_col(position = "dodge")+
  theme_bw() + #Simple balck and white base theme
  #guides(col = guide_legend(nrow = 2, byrow = T)) + #Control number of rows in lengend
  theme(axis.ticks.length = unit(-0.2, "cm"), #Ticks marks inside
        axis.ticks.x = element_blank(), #No ticks on x axis
        axis.text.x = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #x axis text size and spacing
        axis.text.y = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_line(size = 0.5, color = "black"), axis.line.y = element_line(size = 0.5, color = "black"), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "bottom", #Legend postion
        plot.margin = unit(c(0.5,0.2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + #Legend title and text size
  scale_y_continuous(name = "Percentage Correct", limits = c(0, 100), expand = c(0,0)) + #y axis title and limits
  scale_x_discrete("Round", drop = F) + #x axis title and include all rounds, not just those with scores
  scale_fill_manual("Participant", values = participant_cols) + #Legend title and colour by preferred colours
  NULL

#Close .pdf
dev.off()
