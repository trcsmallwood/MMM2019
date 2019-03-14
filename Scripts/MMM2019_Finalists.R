##MMM2019_Finalists
##Calculate the proportion of brackets that pick each species as finalists and champions

#Load in packages
library(stringr)
library(ggplot2)

#Read in dataframe of predictions and results
MMM_df <- read.csv("../Submissions/MMM2019_PredictionsSummary.csv", stringsAsFactors = F)

#Extract finalists
finalists_df <- data.frame(table(unlist(MMM_df[which(MMM_df$Round == "Semi Final"),-c(1:5)])))
names(finalists_df) <- c("Species", "Finalist")

#Extract champions
champions_df <- data.frame(table(unlist(MMM_df[which(MMM_df$Round == "Final"),-c(1:5)])))
names(champions_df) <- c("Species", "Champion")  

#Merge dataframes including all species
combined_df <- merge(finalists_df, champions_df, by = "Species", all = T)

#Assign zeros for finalists who don't become champions
combined_df[which(is.na(combined_df$Champion) == T),3] <- 0

#Calculate Runners Up
combined_df$RunnerUp <- combined_df$Finalist - combined_df$Champion

#Calculate proportion of brackets which predicted each finalist, champion and runner up
combined_prop_df <- data.frame("Species" = combined_df[,1], combined_df[,c(2:4)]/ apply(combined_df[,c(2:4)],2, sum))

#Reshape for plotting, including only champion and runner up columns
combined_plot_df <- melt(combined_df[,-2],
                              id.vars = "Species", variable.name = "Position", value.name = "Count")

#Create output .pdf in relative dir
pdf(file = "../Plots/MMM2019_Finalists.pdf", width=11.69, height=8.27)

#Plot proportion of participants who predicted each species to be a champion or runner up
ggplot(combined_plot_df, aes(x = Species, y = Count, fill = Position)) +
  geom_col(position = position_stack(reverse = T)) +
  theme_bw() + #Simple balck and white base theme
  #coord_flip() +
  theme(axis.ticks.length = unit(-0.2, "cm"), #Ticks marks inside
        axis.ticks.x = element_blank(), #No ticks on x axis
        axis.text.x = element_text(size = 8, margin=margin(10,10,0,10,"pt"), angle = 45, hjust =1), #x axis text size and spacing
        axis.text.y = element_text(size = 12, margin=margin(10,10,10,10,"pt")), #y axis text size and spacing
        panel.border = element_blank(), #No border
        axis.line.x = element_line(size = 0.5, color = "black"), axis.line.y = element_line(size = 0.5, color = "black"), #Axes colours and thickness
        axis.title.x = element_text(size = 14, margin=margin(0,0,0,0,"pt")), axis.title.y = element_text(size = 14, margin=margin(5,5,5,5,"pt")), #Axis titles size and space=ing
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), #No grid lines
        legend.position = "bottom", #Legend postion
        plot.margin = unit(c(0.5,0.2,0.1,0.1), "cm"), #Space around the outside, including space for the ends of the axes
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + #Legend title and text size
  scale_y_continuous(name = "Number of Brackets", expand = c(0,0), breaks = c(0,5,10)) + #y axis title and limits
  scale_x_discrete("", labels = str_wrap(combined_prop_plot_df$Species, width = 20)) + #x axis title and include all rounds, not just those with scores
  scale_fill_manual("", values = c("gold", "grey70"), labels= c("Champion", "Runner Up")) + #Legend title and colour by preferred colours
  NULL

#Close .pdf
dev.off()
