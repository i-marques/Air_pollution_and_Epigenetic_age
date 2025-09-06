###############################################################################################
#
# Adjustment for multiple testing -- accounting for correlated exposures at each timepoint
#
###############################################################################################

# Same approach as in HELIX paper https://www.ncbi.nlm.nih.gov/pubmed/34144479

############################################# Pregnancy, based on GenR --------------------------------------------------
setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Datasets/Birthdataset")

Pregnancy_birth_models <- read.csv("Pregnancy_birth_models.csv")
exposures <- Pregnancy_birth_models[ , c("preg_no2","preg_nox","preg_pm10","preg_pm25","preg_pm25abs")]


# 1. Compute the correlation matrix
cor_matrix <- cor(exposures, use = "pairwise.complete.obs", method = "spearman")

# 2. Perform eigen decomposition
eigen_values <- eigen(cor_matrix)$values

# 3. Calculate ENT using the adapted formula
ENT <- sum((eigen_values - 1) * (eigen_values > 1))

# 4. Adjust the Bonferroni threshold
alpha <- 0.05  # Nominal significance level
adjusted_threshold <- alpha / ENT


# Output results
cat("Effective Number of Tests (ENT):", round(ENT, 3), "\n") #2.723 
cat("Adjusted p-value threshold:", round(adjusted_threshold, 3), "\n") # 0.018

############################################# 4-7years, based on GenR --------------------------------------------------

Data_5yo <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Datasets/child_5/Exposure_outcome_covariates_5yo.csv")
names(Data_5yo)
exposures <- Data_5yo[ , c("child_5_acute_no2","child_5_acute_nox","child_5_acute_pm25","child_5_acute_pm25abs","child_5_acute_pm10",
                           "child_5_long_no2","child_5_long_nox","child_5_long_pm25","child_5_long_pm25abs","child_5_long_pm10")]


# 1. Compute the correlation matrix
cor_matrix <- cor(exposures, use = "pairwise.complete.obs", method = "spearman")

# 2. Perform eigen decomposition
eigen_values <- eigen(cor_matrix)$values

# 3. Calculate ENT using the adapted formula
ENT <- sum((eigen_values - 1) * (eigen_values > 1))

# 4. Adjust the Bonferroni threshold
alpha <- 0.05  # Nominal significance level
adjusted_threshold <- alpha / ENT


# Output results
cat("Effective Number of Tests (ENT):", round(ENT, 3), "\n") #6.774 
cat("Adjusted p-value threshold:", round(adjusted_threshold, 3), "\n") # 0.007

############################################# 8-10years, based on GenR --------------------------------------------------

Data_9yo <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Datasets/child_9/Exposure_outcome_covariates_9yo.csv")
names(Data_9yo)
exposures <- Data_9yo[ , c("child_9_acute_no2","child_9_acute_nox","child_9_acute_pm25","child_9_acute_pm25abs","child_9_acute_pm10",
                           "child_9_long_no2","child_9_long_nox","child_9_long_pm25","child_9_long_pm25abs","child_9_long_pm10")]


# 1. Compute the correlation matrix
cor_matrix <- cor(exposures, use = "pairwise.complete.obs", method = "spearman")

# 2. Perform eigen decomposition
eigen_values <- eigen(cor_matrix)$values

# 3. Calculate ENT using the adapted formula
ENT <- sum((eigen_values - 1) * (eigen_values > 1))

# 4. Adjust the Bonferroni threshold
alpha <- 0.05  # Nominal significance level
adjusted_threshold <- alpha / ENT


# Output results
cat("Effective Number of Tests (ENT):", round(ENT, 3), "\n") #6.778 
cat("Adjusted p-value threshold:", round(adjusted_threshold, 3), "\n") # 0.007

############################################# 16-18years, based on ALSPAC --------------------------------------------------

## values copied from the "ALSPAC_18yo_allvars_correlation_plot_2024-05-13.pdf"
cor_matrix <- matrix(c(
  1,    0.63, 0.89, 0.79, 0.46, 0.71,
  0.63, 1,    0.54, 0.39, 0.73, 0.35,
  0.89, 0.54, 1,    0.75, 0.44, 0.84,
  0.79, 0.39, 0.75, 1,    0.57, 0.90,
  0.46, 0.73, 0.44, 0.57, 1,    0.52,
  0.71, 0.35, 0.84, 0.90, 0.52, 1
), nrow = 6, byrow = TRUE)

# 2. Perform eigen decomposition
eigen_values <- eigen(cor_matrix)$values

# 3. Calculate ENT using the adapted formula
ENT <- sum((eigen_values - 1) * (eigen_values > 1))

# 4. Adjust the Bonferroni threshold
alpha <- 0.05  # Nominal significance level
adjusted_threshold <- alpha / ENT


# Output results
cat("Effective Number of Tests (ENT):", round(ENT, 3), "\n") #3.208 
cat("Adjusted p-value threshold:", round(adjusted_threshold, 3), "\n") # 0.016
