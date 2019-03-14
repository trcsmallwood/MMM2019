##MMM2019_Plotting
##Plot participants' results

#Load in packages
library(reshape2)
library(ggplot2)

rounds <- c("Wild Card", "Round 1", "Round 2", "Round 3", "Quarter Final", "Semi Final", "Final")

#Load in preferred colours of participants
participants_df <- read.csv("../Submissions/Participants.csv", stringsAsFactors = F)

participants_df <- participants_df[which(participants_df$Submitted == "Y"),]# & participants_df$Paid == "Y")]

participant_cols <- participants_df$PreferredColour
line_types <- c("dotted", rep("solid", nrow(participants_df)-1))
shape_types <- c(1, rep(19, nrow(participants_df)-1))

#Load in cumulative points by match
cum_match_points_df <- read.csv("../Results/MMM2019_CumulativeMatchPoints.csv")

#Add a zero origin for all participants
cum_match_points_df <- rbind(cum_match_points_df, rep(0,ncol(cum_match_points_df)))

#Reshape dataframe for plotting
cum_match_points_plot_df <- melt(cum_match_points_df, id.vars = "MatchNo", variable.name = "Participant", value.name = "Points")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_CumulativeMatchPoints.pdf", width=11.69, height=8.27)

#Plot cumulative points of each participant
ggplot(cum_match_points_plot_df, aes(x = MatchNo, y = Points, col = Participant, linetype = Participant)) +
  geom_vline(xintercept = 64, linetype = "dashed", col = "grey50") +
  geom_line(size=0.75) + #Plot as lines
  geom_text(data = subset(cum_match_points_plot_df, MatchNo == max(cum_match_points_plot_df$MatchNo)),
            aes(x = MatchNo, y = Points, label = Participant),
            size = 5, hjust = -0.2, show.legend = F) +
  geom_point(data = subset(cum_match_points_plot_df, MatchNo == max(cum_match_points_plot_df$MatchNo)),
            aes(x = MatchNo, y = Points, shape = Participant)) +
  coord_cartesian(clip = "off") +
  theme_bw() + #Simple balck and white base theme
  guides(col = guide_legend(nrow = 3, byrow = T)) + #Control number of rows in lengend
  theme(axis.ticks.length = unit(-0.2, "cm"), #Ticks marks inside
        axis.text.x = element_text(size = 11, margin=margin(10,10,10,10,"pt")), #x axis text size and spacing
        axis.text.y = element_text(size = 11, margin=margin(10,10,10,10,"pt")), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_line(size = 0.5, color = "black"), axis.line.y = element_line(size = 0.5, color = "black"), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "bottom", #Legend postion
        plot.margin = unit(c(0.5,2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14, angle = 90), legend.text = element_text(size = 11)) + #Legend title and text size
  scale_x_continuous("Match Number", limits = c(0,65), expand = c(0,0)) + #x axis title and limits. Set for a max of 64 matchs
  scale_y_continuous("Cumulative Points", limits = c(0, ceiling(max(cum_match_points_plot_df$Points)*1.1)), expand = c(0,0)) + #y axis title and limits. Max set to 10% more than the highest score. Max points available is 138
  scale_color_manual(values = participant_cols) + #Legend title and colour by preferred colours
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shape_types) +
  NULL

#Close .pdf
dev.off()

#Read in cumulative point per round
cum_round_points_df <- read.csv("../Results/MMM2019_CumulativeRoundPoints.csv")

#Enforce ordering of rounds
cum_round_points_df$Round <- ordered(cum_round_points_df$Round, levels = rounds)

#Reshape dataframe for plotting
cum_round_points_plot_df <- melt(cum_round_points_df, id.vars = "Round", variable.name = "Participant", value.name = "Points")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_CumulativeRoundPoints.pdf", width=11.69, height=8.27)

#Plot cumulative points per round
ggplot(cum_round_points_plot_df, aes(x = Round, y = Points, col = Participant, group = Participant, linetype = Participant, shape = Participant)) +
  geom_point() + #Plot as points
  geom_line() + #Plot as lines
  geom_text(data = subset(cum_round_points_plot_df, Round == unique(cum_round_points_plot_df$Round)[length(unique(cum_round_points_plot_df$Round))]),
            aes(x = Round, y = Points, label = Participant),
            hjust = -0.2, show.legend = F) +
  coord_cartesian(clip = "off") +
  theme_bw() + #Simple balck and white base theme
  guides(col = guide_legend(nrow = 3, byrow = T)) + #Control number of rows in lengend
  theme(axis.ticks.length = unit(-0.2, "cm"), #Ticks marks inside
        axis.text.x = element_text(size = 11, margin=margin(10,10,10,10,"pt")), #x axis text size and spacing
        axis.text.y = element_text(size = 11, margin=margin(10,10,10,10,"pt")), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_line(size = 0.5, color = "black"), axis.line.y = element_line(size = 0.5, color = "black"), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(5,5,5,5,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "bottom", #Legend postion
        plot.margin = unit(c(0.5,2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14, angle = 90), legend.text = element_text(size = 11)) + #Legend title and text size
  scale_y_continuous("Cumulative Points", limits = c(0, ceiling(max(cum_round_points_plot_df$Points)*1.1)), expand = c(0,0)) + #y axis title and limits. Max set to 10% more than the highest score. Max points available is 138
  scale_x_discrete("Round", drop = F) + #x axis title and include all rounds, not just those with scores
  scale_color_manual("Participant", values = participant_cols) + #Legend title and colour by preferred colours
  scale_linetype_manual(values = line_types) +
  scale_shape_manual(values = shape_types) +
  NULL

#Close .pdf
dev.off()

#Read in proportion correct per round
round_correct_df <- read.csv("../Results/MMM2019_RoundCorrectSummary.csv")

#Enforce ordering of rounds
round_correct_df$Round <- ordered(round_correct_df$Round, levels = rounds)

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
