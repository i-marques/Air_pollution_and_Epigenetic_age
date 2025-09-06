library(metafor)
library(dplyr)
setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy")

MA_pregnancy <- read.csv("MA_pregnancy_08-Oct-2024.csv")
names(MA_pregnancy)
### multiply coefficient and SE by 10 to get effect  per 10ug/m3 coefficient in exposure, except for black carbon
MA_pregnancy <- MA_pregnancy %>%
  mutate(Coefficient10 = ifelse(exp != "bc", Coefficient * 10, Coefficient))

MA_pregnancy <- MA_pregnancy %>%
  mutate(SE10 = ifelse(exp != "bc", SE * 10, SE))

MA_pregnancy$exp <- ifelse(MA_pregnancy$exp == "bc", "BC",
                           ifelse(MA_pregnancy$exp == "no2", "NO2",
                                  ifelse(MA_pregnancy$exp == "nox", "NOx",
                                         ifelse(MA_pregnancy$exp == "pm10", "pm10",
                                                ifelse(MA_pregnancy$exp == "pm25", "pm25", MA_pregnancy$exp)))))

MA_pregnancy$N <- MA_pregnancy$N_Participants

###### ----------------------------------------------------------------------
###### Start Random-effects MA  ---------------------------------------------
###### ----------------------------------------------------------------------

unique_categories <- unique(MA_pregnancy$model_exp)

# Create an empty data frame to store results
results_df <- data.frame(category = character(),
                         beta = numeric(),
                         se = numeric(),
                         pval = numeric(),
                         ci.lb = numeric(),
                         ci.ub = numeric(),
                         I2 = numeric(),
                         Total_Cohorts = numeric(),
                         stringsAsFactors = FALSE)

# Open a PDF device to save all forest plots
pdf("Pregnancy_RE_forest_plots.pdf")

# Loop over each unique category
for (category in unique_categories) {

  cat("\nProcessing category:", category, "\n")
  
  # Subset the data for the current category
  subset_MA_pregnancy <- subset(MA_pregnancy, model_exp == category)
  
  # Define yi and sei based on the value of 'exp'
  exp_type <- subset_MA_pregnancy$exp[1]
  
  yi_col <- if (exp_type == "BC") "Coefficient" else "Coefficient10"
  sei_col <- if (exp_type == "BC") "SE" else "SE10"
  
  # Assign yi and sei to columns
  subset_MA_pregnancy$yi <- subset_MA_pregnancy[[yi_col]]
  subset_MA_pregnancy$sei <- subset_MA_pregnancy[[sei_col]]
  
  ### Random-effects MA
  res <- rma(yi = yi, sei = sei, data = subset_MA_pregnancy, control = list(stepadj = 0.5))
  
  # Save results to the results data frame
  results_df[nrow(results_df) + 1, ] <- c(category, res$beta, res$se, res$pval, res$ci.lb , res$ci.ub, res$I2, res$k)
  
  # Construct title for the forest plot
  title <- paste("Forest plot for", subset_MA_pregnancy[1, "exp"], subset_MA_pregnancy[1, "Model"], ", I-squared =", round(res$I2, 2))
  
  # Create forest plot
  cohort <- subset_MA_pregnancy$cohort
  N <- subset_MA_pregnancy$N
  
  forest(res, showweights = TRUE, header=c("Cohort","Estimate [95% CI]"), cex=0.75,
         slab = cohort, 
         mlab="RE meta-analysis",col="deepskyblue2",border="deepskyblue2",
         ilab = c(N), main = title)
}

# Close the PDF device
dev.off()

# Save results to a CSV file
write.csv(results_df, "Pregnancy_RE_MAresults.csv", quote = F ,row.names = FALSE)

###### ----------------------------------------------------------------------
###### Start Fixed-effects MA -----------------------------------------------
###### ----------------------------------------------------------------------
unique_categories <- unique(MA_pregnancy$model_exp)

# Create an empty data frame to store results
results_df <- data.frame(category = character(),
                         beta = numeric(),
                         se = numeric(),
                         pval = numeric(),
                         ci.lb = numeric(),
                         ci.ub = numeric(),
                         I2 = numeric(),
                         Total_Cohorts = numeric(),
                         stringsAsFactors = FALSE)

# Open a PDF device to save all forest plots
pdf("Pregnancy_FE_forest_plots.pdf")

# Loop over each unique category
for (category in unique_categories) {

  cat("\nProcessing category:", category, "\n")
  
  # Subset the data for the current category
  subset_MA_pregnancy <- subset(MA_pregnancy, model_exp == category)
  
  # Define yi and sei based on the value of 'exp'
  exp_type <- subset_MA_pregnancy$exp[1]
  
  yi_col <- if (exp_type == "BC") "Coefficient" else "Coefficient10"
  sei_col <- if (exp_type == "BC") "SE" else "SE10"
  
  # Assign yi and sei to columns
  subset_MA_pregnancy$yi <- subset_MA_pregnancy[[yi_col]]
  subset_MA_pregnancy$sei <- subset_MA_pregnancy[[sei_col]]
 
  ### Random-effects MA
  res <- rma(yi = yi, sei = sei, data = subset_MA_pregnancy, control = list(stepadj = 0.5), method = "FE")
  
  # Save results to the results data frame
  results_df[nrow(results_df) + 1, ] <- c(category, res$beta, res$se, res$pval, res$ci.lb , res$ci.ub, res$I2, res$k)
  
  # Construct title for the forest plot
  title <- paste("Forest plot for", subset_MA_pregnancy[1, "exp"], subset_MA_pregnancy[1, "Model"], ", I-squared =", round(res$I2, 2))
  
  # Create forest plot
  cohort <- subset_MA_pregnancy$cohort
  N <- subset_MA_pregnancy$N
  
  forest(res, showweights = TRUE, header=c("Cohort","Estimate [95% CI]"), cex=0.75,
         slab = cohort, 
         mlab="FE meta-analysis",col="deepskyblue2",border="deepskyblue2",
         ilab = c(N), main = title)
}

# Close the PDF device
dev.off()

# Save results to a CSV file
write.csv(results_df, "Pregnancy_FE_MAresults.csv", quote = F ,row.names = FALSE)

R.version.string #"R version 4.4.1 (2024-06-14 ucrt)"
