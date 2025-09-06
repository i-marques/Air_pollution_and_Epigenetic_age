###########################################################################################
# Air Pollution exposure during pregnancy & Bohlin DNAmGA
# Collect descriptive information 
# Irene Marques
# January 2025
###########################################################################################

###########################################
########################################### NOTE: you may look for the word CHANGE to find 
########################################### parts of the script that might need to be addapted
###########################################

library(tableone)

###########################################
### Set initial parameters
cohort <- "GenR" #CHANGE to your cohort name
period <- "pregnancy" 
Date<- Sys.Date()

### Directory to store result files
#CHANGE to your results directory for the pregnancy and DNAmGA analyses - New_descriptives
resultsdir <- "Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results/New_descriptives/Pregnancy"

###########################################
### CHANGE to directory with cleaned dataset for pregnancy exposure & DNAmGA
setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Datasets/Birthdataset")

data <- read.csv("Pregnancy_birth_models.csv") # CHANGE to your cleaned dataset file name
names(data)
#FOR THIS SCRIPT TO WORK you should have these variables in your dataset
#If your variables have different names, please adjust the script as needed

# [1] "IDC"            "sex"            "GA"             "Bohlin"         "ageAcc2.Bohlin" "birthweight"    "bcell"         
# [8] "cd4t"           "cd8t"           "nRBC"           "gran"           "mono"           "nk"             "LMP" # LMP not necessary           
#[15] "mat.age"        "mat.BMI"        "mat.edu"        "mat.smk"        "mat.parity"     "ses.average"    "Plate"         
#[22] "preg_no2"       "preg_nox"       "preg_pm10"      "preg_pm25"      "preg_pm25abs" 

dim(data)
#[1] 1359   26

################################################################################
# Saving Descriptive information
################################################################################

## Important step, be sure to categorize the variables into numeric or categorical here
## so that the correct test is used to compare the populations

#################### Specify numerical versus categorical variables
catvars <- c("IDC","sex","Plate",
             "mat.edu","mat.smk","mat.parity","ses.average") #CHANGE to your categorical variables if you used different names

data[catvars] <- lapply(data[catvars], as.factor)


numvars <- c("GA","Bohlin","ageAcc2.Bohlin",
             "bcell","cd4t","cd8t","nRBC","gran","mono","nk",
             "mat.age","mat.BMI",
             "preg_no2","preg_nox","preg_pm10","preg_pm25","preg_pm25abs") # CHANGE to your continuous variables if you used different names

data[numvars] <- lapply(data[numvars], as.numeric)

#################### Overall Table 1
table1 <- CreateTableOne(data = data[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_", Date, ".csv")))


#################### Table 1 for Model 2 (main model)

# subset to complete cases for model 2
data_model2 <- data[complete.cases(data[, c("Plate", "GA", "sex", "mat.edu", "mat.age", "ses.average", "mat.parity")]), ]

table1 <- CreateTableOne(data = data_model2[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_Model2_", Date, ".csv")))

#################### Table 1 for Model 2 (main model) - Males

#subset for males
data_model2_males <- data_model2[data_model2$sex == 1, ] # CHANGE if you use a different category for males

table1 <- CreateTableOne(data = data_model2_males[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_Model2_boys_", Date, ".csv")))

#################### Table 1 for Model 2 (main model) - Females

#subset for females
data_model2_females <- data_model2[data_model2$sex == 2, ]  # CHANGE if you use a different category for females

table1 <- CreateTableOne(data = data_model2_females[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_Model2_girls_", Date, ".csv")))

#################### Table 1 for Model 2 (main model) - Non-smokers

# subset for participants not exposed to smoking
data_model2_nosmk <- data_model2[data_model2$mat.smk == 0, ]  # CHANGE if you use a different category for non-smokers
data_model2_nosmk <- data_model2_nosmk[!is.na(data_model2_nosmk$mat.smk), ]

table1 <- CreateTableOne(data = data_model2_nosmk[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_Model2_nonsmk_", Date, ".csv")))

#################### Table 1 for Model 2 (main model) - Smokers

# subset for participants not exposed to smoking
data_model2_smk <- data_model2[data_model2$mat.smk == 1, ]    # CHANGE if you use a different category for smokers
data_model2_smk <- data_model2_smk[!is.na(data_model2_smk$mat.smk), ]

table1 <- CreateTableOne(data = data_model2_smk[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_Model2_smk_", Date, ".csv")))

################################################################################
# Comparison of descriptive stats between main model and sensitivity groups
################################################################################

# create data frame to store results
comparison_results <- data.frame(
  Variable = character(),
  Population = character(),
  Test = character(),
  Statistic = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

######## Main model 2 versus males

# Loop through all variables in the dataset (except ID variable that should be the 1st on the dataset)
for (var in colnames(data_model2)[-1]) {
  # Check if the variable is categorical or numerical
  if (var %in% catvars) {
    # Categorical variable: Run Chi-square test
    table_model2 <- table(data_model2[[var]], useNA = "ifany")
    table_model2_males <- table(data_model2_males[[var]], useNA = "ifany")
    chi_result <- chisq.test(cbind(table_model2, table_model2_males), simulate.p.value = TRUE)
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2males",
      Test = "Chi-square",
      Statistic = chi_result$statistic,
      P_value = chi_result$p.value,
      stringsAsFactors = FALSE
    ))
  } else if (var %in% numvars) {
    # Numerical variable: Run t-test
    t_result <- t.test(data_model2[[var]], data_model2_males[[var]])
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2males",
      Test = "t-test",
      Statistic = t_result$statistic,
      P_value = t_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

######## Main model 2 versus females

# Loop through all variables in the dataset (except ID variable that should be the 1st on the dataset)
for (var in colnames(data_model2)[-1]) {
  # Check if the variable is categorical or numerical
  if (var %in% catvars) {
    # Categorical variable: Run Chi-square test
    table_model2 <- table(data_model2[[var]], useNA = "ifany")
    table_model2_females <- table(data_model2_females[[var]], useNA = "ifany")
    chi_result <- chisq.test(cbind(table_model2, table_model2_females), simulate.p.value = TRUE)
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2females",
      Test = "Chi-square",
      Statistic = chi_result$statistic,
      P_value = chi_result$p.value,
      stringsAsFactors = FALSE
    ))
  } else if (var %in% numvars) {
    # Numerical variable: Run t-test
    t_result <- t.test(data_model2[[var]], data_model2_females[[var]])
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2females",
      Test = "t-test",
      Statistic = t_result$statistic,
      P_value = t_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
}


######## Main model 2 versus non-smokers

# Loop through all variables in the dataset (except ID variable that should be the 1st on the dataset)
for (var in colnames(data_model2)[-1]) {
  if (var == "mat.smk") {
    next  # Skip the variable "mat.smk"
  }
  
  if (var %in% catvars) {
    # Categorical variable: Run Chi-square test
    table_model2 <- table(data_model2[[var]], useNA = "ifany")
    table_model2_nosmk <- table(data_model2_nosmk[[var]], useNA = "ifany")
    chi_result <- chisq.test(cbind(table_model2, table_model2_nosmk), simulate.p.value = TRUE)
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2nosmk",
      Test = "Chi-square",
      Statistic = chi_result$statistic,
      P_value = chi_result$p.value,
      stringsAsFactors = FALSE
    ))
  } else if (var %in% numvars) {
    # Numerical variable: Run t-test
    t_result <- t.test(data_model2[[var]], data_model2_nosmk[[var]])
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2nosmk",
      Test = "t-test",
      Statistic = t_result$statistic,
      P_value = t_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

######## Main model 2 versus Smokers

# Loop through all variables in the dataset (except ID variable that should be the 1st on the dataset)
for (var in colnames(data_model2)[-1]) {
  if (var == "mat.smk") {
    next  # Skip the variable "mat.smk"
  }
  
  if (var %in% catvars) {
    # Categorical variable: Run Chi-square test
    table_model2 <- table(data_model2[[var]], useNA = "ifany")
    table_model2_smk <- table(data_model2_smk[[var]], useNA = "ifany")
    chi_result <- chisq.test(cbind(table_model2, table_model2_smk), simulate.p.value = TRUE)
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2smk",
      Test = "Chi-square",
      Statistic = chi_result$statistic,
      P_value = chi_result$p.value,
      stringsAsFactors = FALSE
    ))
  } else if (var %in% numvars) {
    # Numerical variable: Run t-test
    t_result <- t.test(data_model2[[var]], data_model2_smk[[var]])
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2_vs_M2smk",
      Test = "t-test",
      Statistic = t_result$statistic,
      P_value = t_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

######## Female versus male

# Loop through all variables in the dataset (except ID variable that should be the 1st on the dataset)
for (var in colnames(data_model2_females)[-1]) {
  if (var == "sex") {
    next  # Skip the variable "sex"
  }
  
  if (var %in% catvars) {
    # Categorical variable: Run Chi-square test
    table_model2_f <- table(data_model2_females[[var]], useNA = "ifany")
    table_model2_m <- table(data_model2_males[[var]], useNA = "ifany")
    chi_result <- chisq.test(cbind(table_model2_f, table_model2_m), simulate.p.value = TRUE)
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2female_vs_M2male",
      Test = "Chi-square",
      Statistic = chi_result$statistic,
      P_value = chi_result$p.value,
      stringsAsFactors = FALSE
    ))
  } else if (var %in% numvars) {
    # Numerical variable: Run t-test
    t_result <- t.test(data_model2_females[[var]], data_model2_males[[var]])
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2female_vs_M2male",
      Test = "t-test",
      Statistic = t_result$statistic,
      P_value = t_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

######## Smokers versus non-smokers

# Loop through all variables in the dataset (except ID variable that should be the 1st on the dataset)
for (var in colnames(data_model2_nosmk)[-1]) {
  if (var == "mat.smk") {
    next  # Skip the variable "mat.smk"
  }
  
  if (var %in% catvars) {
    # Categorical variable: Run Chi-square test
    table_model2_nosmk <- table(data_model2_nosmk[[var]], useNA = "ifany")
    table_model2_smk <- table(data_model2_smk[[var]], useNA = "ifany")
    chi_result <- chisq.test(cbind(table_model2_nosmk, table_model2_smk), simulate.p.value = TRUE)
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2nonsmk_vs_M2smk",
      Test = "Chi-square",
      Statistic = chi_result$statistic,
      P_value = chi_result$p.value,
      stringsAsFactors = FALSE
    ))
  } else if (var %in% numvars) {
    # Numerical variable: Run t-test
    t_result <- t.test(data_model2_nosmk[[var]], data_model2_smk[[var]])
    
    comparison_results <- rbind(comparison_results, data.frame(
      Variable = var,
      Population = "M2nonsmk_vs_M2smk",
      Test = "t-test",
      Statistic = t_result$statistic,
      P_value = t_result$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

######## Save results

row.names(comparison_results) <- NULL
write.csv(comparison_results, file.path(resultsdir, paste0(cohort,"_",period, "_comparison_results_", Date, ".csv")), row.names = FALSE)


###### END, thank you!
