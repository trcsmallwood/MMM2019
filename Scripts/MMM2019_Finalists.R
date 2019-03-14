##MMM2019_Finalists
##Calculate the proportion of brackets that pick each species as finalists and champions

library(stringr)
library(ggplot2)

MMM_df <- read.csv("../Submissions/MMM2019_PredictionsSummary.csv", stringsAsFactors = F)

MMM_finalists_df <- MMM_df[which(MMM_df$Round == "Semi Final" | MMM_df$Round == "Final"),-c(1,3:5)]



finalists_df <- data.frame(table(unlist(MMM_finalists_df[which(MMM_finalists_df$Round == "Semi Final"),-1])))
champions_df <- data.frame(table(unlist(MMM_finalists_df[which(MMM_finalists_df$Round == "Final"),-1])))

names(finalists_df) <- c("Species", "Finalist")
names(champions_df) <- c("Species", "Champion")  

combined_df <- merge(finalists_df, champions_df, by = "Species")

combined_prop_df <- data.frame("Species" = combined_df[,1], combined_df[,c(2:3)]/ apply(combined_df[,c(2:3)],2, sum))

combined_prop_plot_df <- melt(combined_prop_df, variable.name = "Position", value.name = "Proportion")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_Finalists.pdf", width=11.69, height=8.27)

#Plot proportion correct per round
ggplot(combined_prop_plot_df, aes(x = Species, y = Proportion, fill = Position)) +
  geom_col(position = "dodge") +
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
  scale_y_continuous(name = "Proportion of Brackets", limits = c(0, 0.5), expand = c(0,0)) + #y axis title and limits
  scale_x_discrete("", labels = str_wrap(combined_prop_plot_df$Species, width = 10)) + #x axis title and include all rounds, not just those with scores
  scale_fill_manual("", values = c("grey70", "gold")) + #Legend title and colour by preferred colours
  NULL

#Close .pdf
dev.off()
