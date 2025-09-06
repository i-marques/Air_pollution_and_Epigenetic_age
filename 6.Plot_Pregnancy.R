#### Air pollution and Epigenetic age
#### Create Plots for Results section ####
#### Pregnancy

setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Plots")
#load data MA results
Pregnancy_FE_MAresults <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/Results Oct 2024 - Updated/Pregnancy_All/Pregnancy_FE_MAresults.csv")
Pregnancy_FEMALES_MAresults <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/Results Oct 2024 - Updated/Preganancy_FEMALES/Pregnancy_FE_FEMALE_MAresults.csv")
Pregnancy_MALE_MAresults <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/Results Oct 2024 - Updated/Pregnancy_MALES/Pregnancy_FE_MALE_MAresults.csv")
Pregnancy_nonsmk_MAresults <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/Results Oct 2024 - Updated/Pregnancy_nonsmk/Pregnancy_nonsmk_FE_MAresults.csv")

library(ggplot2)
########################################################### Pregnancy ALL ----

# Pregnancy_FE_MAresults$category is now = "Model 1_no2"
# I want to have one column for model and another for exposure
#this splits values at the _
split_values <- strsplit(as.character(Pregnancy_FE_MAresults$category), "_")

# this creates the new columns
Pregnancy_FE_MAresults$Model <- sapply(split_values, "[[", 1)
Pregnancy_FE_MAresults$Exposure <- sapply(split_values, "[[", 2)

# create "Label" column for ggplot, this will show in the plot label
Pregnancy_FE_MAresults$Label <- ifelse(Pregnancy_FE_MAresults$Model == "Model 2", "All",
                                       ifelse(Pregnancy_FE_MAresults$Model == "Model 3", "All smkAdj", "NA"))

## Keep only model 2 for plot and the necesarry columns
names(Pregnancy_FE_MAresults)
Pregnancy_FE_MAresults <- Pregnancy_FE_MAresults[Pregnancy_FE_MAresults$Model == "Model 2" | Pregnancy_FE_MAresults$Model == "Model 3" ,c("Model", "Exposure", "Label", "beta", "ci.lb", "ci.ub", "se","pval","I2","Total_Cohorts") ]

########################################################### Pregnancy Girls ----

# Pregnancy_FE_MAresults$category is now = "Model 1_no2"
# I want to have one column for model and another for exposure
#this splits values at the _
split_values <- strsplit(as.character(Pregnancy_FEMALES_MAresults$category), "_")

# this creates the new columns
Pregnancy_FEMALES_MAresults$Model <- sapply(split_values, "[[", 2)
Pregnancy_FEMALES_MAresults$Exposure <- sapply(split_values, "[[", 3)

# create "Label" column for ggplot
Pregnancy_FEMALES_MAresults$Label <- "Girls"

## Keep only model 2 for plot and the necesarry columns
Pregnancy_FEMALES_MAresults <- Pregnancy_FEMALES_MAresults[Pregnancy_FEMALES_MAresults$Model == "Model 2" ,c("Model", "Exposure", "Label", "beta", "ci.lb", "ci.ub", "se","pval","I2","Total_Cohorts") ]

########################################################### Pregnancy Boys ----

# Pregnancy_FE_MAresults$category is now = "Model 1_no2"
# I want to have one column for model and another for exposure
#this splits values at the _
split_values <- strsplit(as.character(Pregnancy_MALE_MAresults$category), "_")

# this creates the new columns
Pregnancy_MALE_MAresults$Model <- sapply(split_values, "[[", 2)
Pregnancy_MALE_MAresults$Exposure <- sapply(split_values, "[[", 3)

# create "Label" column for ggplot
Pregnancy_MALE_MAresults$Label <- "Boys"

## Keep only model 2 for plot and the necesarry columns
Pregnancy_MALE_MAresults <- Pregnancy_MALE_MAresults[Pregnancy_MALE_MAresults$Model == "Model 2" ,c("Model", "Exposure", "Label", "beta", "ci.lb", "ci.ub", "se","pval","I2","Total_Cohorts") ]

########################################################### Pregnancy All non-smk ----

# Pregnancy_FE_MAresults$category is now = "Model 1_no2"
# I want to have one column for model and another for exposure
#this splits values at the _
split_values <- strsplit(as.character(Pregnancy_nonsmk_MAresults$category), "_")

# this creates the new columns
Pregnancy_nonsmk_MAresults$Model <- sapply(split_values, "[[", 1)
Pregnancy_nonsmk_MAresults$Exposure <- sapply(split_values, "[[", 2)

# create "Label" column for ggplot
Pregnancy_nonsmk_MAresults$Label <- "All non-smk"

## Keep only model 2 for plot and the necesarry columns
Pregnancy_nonsmk_MAresults <- Pregnancy_nonsmk_MAresults[Pregnancy_nonsmk_MAresults$Model == "Model 7" ,c("Model", "Exposure", "Label", "beta", "ci.lb", "ci.ub", "se","pval","I2","Total_Cohorts") ]

################### Merge Results for plotting

Pregnancy_plot <- rbind(Pregnancy_FE_MAresults, Pregnancy_nonsmk_MAresults, Pregnancy_FEMALES_MAresults, Pregnancy_MALE_MAresults)

write.csv(Pregnancy_plot, file = "Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/Results Oct 2024 - Updated/Pregnancy_new_plot.csv", row.names = F, quote = F)

Pregnancy_plot <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/Results Oct 2024 - Updated/Pregnancy_new_plot.csv")

str(Pregnancy_plot)
Pregnancy_plot$Exposure <- factor(Pregnancy_plot$Exposure, levels = c("no2","nox","pm10","pm25","bc"))
Pregnancy_plot$Label <- factor(Pregnancy_plot$Label, levels = c("All","All smkAdj","All non-smk","Girls","Boys"))

## Plot pregnancy

Pregnancy_ggplot <- ggplot(data = Pregnancy_plot, aes(x = Exposure, y = beta, ymin = ci.lb, ymax = ci.ub, color = Label, shape = Label)) +
  geom_pointrange(linewidth = 0.6, size = 0.6, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, lty=1, color = "black", linewidth = 0.5) +
  xlab("Air Pollutant") + ylab("Meta-analyses Effect estimates (95% CI)") + # Assign titles to x-axis and y-axis
  #ggtitle("A)") + # Add title
  scale_color_manual(values = c("All" = "#1F77B4","All smkAdj" = "#4A90D9", "All non-smk" = "#A6C9E2", "Girls" = "#8B0000","Boys" = "#006400")) + # Assign specific colors to groups
  scale_shape_manual(values = c("All" = 15, "All smkAdj" = 17, "All non-smk" = 0, "Girls" = 16, "Boys" = 18)) + # Choose dot shapes
  theme_bw(base_size = 14) + # Use black and white theme with larger base text size
  theme(axis.text.y = element_text(color = "black", size = 12),  # Make y-axis labels black and change text size
        axis.text.x = element_text(color = "black", size = 12),  # Make x-axis labels black and change text size
        axis.title.x = element_text(size = 14, color = "black"),  # Change size  and color of x-axis title
        axis.title.y = element_text(size = 14, color = "black"),  # Change size  and color of y-axis title
        plot.title = element_text(hjust = 0.5, size = 16, color = "black"),  # Center title, change color
        panel.grid.major = element_line(color = "gray90", linetype = "dashed"),  # Light raster lines
        panel.grid.minor = element_blank(),  # No minor raster lines
        #panel.border = element_rect(color = "black", linewidth = 1.2),  # Black line around plot
        panel.background = element_blank()  # Empty background
  )

getwd()
ggsave("Pregnancy_smkAdj_FE_Plot.png", plot = Pregnancy_ggplot, width = 10, height = 6, dpi = 300, units = "in")
