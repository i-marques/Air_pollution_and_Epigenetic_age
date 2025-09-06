###########################################################################################
# Air Pollution exposure during pregnancy & Bohlin DNAmGA
# Irene Marques
# 16.11.2023
###########################################################################################

library(tableone)
library(car)
library(corrplot)

###########################################
### Set initial parameters
cohort <- "GenR" #CHANGE to your cohort name
period <- "pregnancy" 
Date<- Sys.Date()

### Directory to store result files
#CHANGE to your results directory for the pregnancy and DNAmGA analyses
resultsdir <- "Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results/Pregnancy_LifeCycle"

# List of exposure variables//EDIT ACCORDING TO EXPOSURES AVAILABLE IN YOUR COHORT
exposure_vars <- c("preg_no2", "preg_nox", "preg_pm10", "preg_pm25", "preg_pm25abs") # CHANGE exposures if needed

###########################################
### CHANGE to directory with cleaned dataset for pregnancy exposure & DNAmGA
setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Datasets/Birthdataset")

data <- read.csv("Pregnancy_birth_models.csv") # CHANGE to your cleaned dataset file name
names(data)
#FOR THIS SCRIPT TO WORK you should have these variables in your dataset
#If your variables have different names, please adjust as needed

#[1] "IDC"            "sex"            "GA"             "Bohlin"         "ageAcc2.Bohlin" "birthweight"    "bcell"         
#[8] "cd4t"           "cd8t"           "nRBC"           "gran"           "mono"           "nk"             "LMP"           
#[15] "mat.age"        "mat.BMI"        "mat.edu"        "mat.smk"        "mat.parity"     "ses.average"    "Plate"         
#[22] "preg_no2"       "preg_nox"       "preg_pm10"      "preg_pm25"      "preg_pm25abs" 

dim(data)
#[1] 1359   26

################################################################################
################################################################################
# Saving Descriptive information / generating plots
################################################################################
##### Significant correlations with color

M<-cor(data[ , -1], use = "pairwise.complete.obs", method = "spearman")

testRes = cor.mtest(M, conf.level = 0.95)

pdf(file.path(resultsdir, paste0(cohort,"_",period, "_correlation_plot_", Date, ".pdf")), width=10, height=10)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'upper', 
         tl.col = 'black', tl.srt = 45, insig='blank', number.cex = 0.7, tl.cex = 0.7, diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2), cex = 0.6)
dev.off()

####################
catvars <- c("IDC","sex","Plate",
             "mat.edu","mat.smk","mat.parity","ses.average", "LMP") #CHANGE to your categorical variables

data[catvars] <- lapply(data[catvars], as.factor)


numvars <- c("GA","Bohlin","ageAcc2.Bohlin",
             "bcell","cd4t","cd8t","nRBC","gran","mono","nk",
             "mat.age","mat.BMI",
             "preg_no2","preg_nox","preg_pm10","preg_pm25","preg_pm25abs") # CHANGE to your continuous variables

data[numvars] <- lapply(data[numvars], as.numeric)

####################
table1 <- CreateTableOne(data = data[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_", Date, ".csv")))

summary(table1) # PLEASE copy paste the output to a txt file, needed for information on NAs

####################

#Change exposures if needed

pdf(file.path(resultsdir, paste0(cohort,"_",period, "_Airp_hist_", Date, ".pdf")), width=13, height=10)

par(mfrow=c(2,3))
hist(data$preg_no2, breaks = 20, xlab=" ",main="preg_no2")
hist(data$preg_nox, breaks = 20, xlab=" ",main="preg_nox")
hist(data$preg_pm10, breaks = 20, xlab=" ",main="preg_pm10")
hist(data$preg_pm25, breaks = 20, xlab=" ",main="preg_pm25")
hist(data$preg_pm25abs, breaks = 20 ,xlab=" ",main="preg_pm25abs")

dev.off()

####################

pdf(file.path(resultsdir, paste0(cohort,"_",period, "_GA_DNAmGA_", Date, ".pdf")))

par(mfrow=c(2,2))
hist(data$GA, breaks = 50,xlab = "weeks", main="GA",col=rgb(0.2,0.4,0.8,0.5), xlim = c(28, 45))
hist(data$Bohlin, breaks = 50,xlab = "weeks", main="Bohlin",col=rgb(0.2,0.4,0.8,0.5), xlim = c(30, 45))
plot(data$GA, data$Bohlin, xlab = "GA", ylab = "Bohlin", main="GA vs Bohlin",
     pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5),frame = FALSE, xlim = c(28, 45))
plot(data$GA, data$ageAcc.Bohlin, xlab = "GA", ylab = "ageAcc.Bohlin", main="GA vs ageAcc.Bohlin",
     pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5),frame = FALSE, xlim = c(28, 45))

dev.off()

####################
#Change exposures if needed

pdf(file.path(resultsdir, paste0(cohort,"_",period, "_Bohlin_Airp_", Date, ".pdf")))

par(mfrow = c(2, 3))
plot(data$preg_no2, data$Bohlin, xlab = "NO2",
     ylab = "Bohlin", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_nox, data$Bohlin, xlab = "NOx",
     ylab = "Bohlin", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_pm10, data$Bohlin, xlab = "PM10",
     ylab = "Bohlin", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_pm25, data$Bohlin, xlab = "PM25",
     ylab = "Bohlin", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_pm25abs, data$Bohlin, xlab = "PM25abs",
     ylab = "Bohlin", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))

dev.off()

####################
#Change exposures if needed

pdf(file.path(resultsdir, paste0(cohort,"_",period, "_GA_Airp_", Date, ".pdf")))

par(mfrow = c(2, 3))
plot(data$preg_no2, data$GA, xlab = "NO2",
     ylab = "GA", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_nox, data$GA, xlab = "NOx",
     ylab = "GA", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_pm10, data$GA, xlab = "PM10",
     ylab = "GA", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_pm25, data$GA, xlab = "PM25",
     ylab = "GA", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
plot(data$preg_pm25abs, data$GA, xlab = "PM25abs",
     ylab = "GA", pch=20 , cex=1 , col=rgb(0.2,0.4,0.8,0.5))
dev.off()


################################################################################
#                      Linear Regressions
#          
#             all children, mat.smk as covariate
################################################################################
setwd(resultsdir)

# Create a function to extract coefficients, SE, p-values, and VIF for the exposure variable
extract_exposure_info <- function(model, exposure_var) {
  coef <- coef(model)[exposure_var]
  se <- summary(model)$coefficients[exposure_var, "Std. Error"]
  p_value <- summary(model)$coefficients[exposure_var, "Pr(>|t|)"]
  n_participants <- length(model$model$Bohlin)
  return(c(coef, se, p_value, n_participants))
}

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(15),
  Model = character(15),
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA  # Add a VIF column
)

# Create a data frame to store VIF values
vif_results <- data.frame()


# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_withsmk_GRAN_", exposure_var, ".pdf", sep = ""))
  
  
  for (j in 1:5) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + mat.smk + ses.average + mat.parity")
    } else if (j == 4) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + ses.average + mat.parity + gran + bcell + cd4t + cd8t + nRBC + mono + nk")
    } else if (j == 5) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + mat.smk + ses.average  + mat.parity + gran + bcell + cd4t + cd8t + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = data, na.action = na.omit)
    results[(i * 5 - (5 - j)), 1] <- exposure_var
    results[(i * 5 - (5 - j)), 2] <- paste("Model", j)
    results[(i * 5 - (5 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "_Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}
write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_lm_DNAmGA_withsmk_withGRAN_", Date, ".csv")), row.names = FALSE)

write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_VIF_lm_DNAmGA_withsmk_withGRAN_", Date, ".csv")), row.names = FALSE)

###################### REMOVE GRAN

# Create a function to extract coefficients, SE, p-values, and VIF for the exposure variable
extract_exposure_info <- function(model, exposure_var) {
  coef <- coef(model)[exposure_var]
  se <- summary(model)$coefficients[exposure_var, "Std. Error"]
  p_value <- summary(model)$coefficients[exposure_var, "Pr(>|t|)"]
  n_participants <- length(model$model$Bohlin)
  return(c(coef, se, p_value, n_participants))
}

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(15),
  Model = character(15),
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA  # Add a VIF column
)

# Create a data frame to store VIF values
vif_results <- data.frame()


# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_withsmk_noGRAN_", exposure_var, ".pdf", sep = ""))
  
  
  for (j in 1:5) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + mat.smk + ses.average + mat.parity")
    } else if (j == 4) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + ses.average + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    } else if (j == 5) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + mat.smk + ses.average  + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = data, na.action = na.omit)
    results[(i * 5 - (5 - j)), 1] <- exposure_var
    results[(i * 5 - (5 - j)), 2] <- paste("Model", j)
    results[(i * 5 - (5 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "_Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}
write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_lm_DNAmGA_withsmk_noGRAN", Date, ".csv")), row.names = FALSE)

write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_VIF_lm_DNAmGA_withsmk_noGRAN", Date, ".csv")), row.names = FALSE)

################################################################################
#             MALES only, mat.smk as covariate
################################################################################

data_males <- data[data$sex == 1, ] # CHANGE if you use a different category for males

# Create a function to extract coefficients, SE, p-values, and VIF for the exposure variable
extract_exposure_info <- function(model, exposure_var) {
  coef <- coef(model)[exposure_var]
  se <- summary(model)$coefficients[exposure_var, "Std. Error"]
  p_value <- summary(model)$coefficients[exposure_var, "Pr(>|t|)"]
  n_participants <- length(model$model$Bohlin)
  return(c(coef, se, p_value, n_participants))
}

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(15), 
  Model = character(15), 
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA  # Add a VIF column
)

# Create a data frame to store VIF values
vif_results <- data.frame()


# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_withsmk_MALES_", exposure_var, ".pdf", sep = ""))
  
  for (j in 1:5) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + mat.smk + ses.average + mat.parity")
    } else if (j == 4) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    } else if (j == 5) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + mat.smk + ses.average  + mat.parity + bcell + cd4t + cd8t  + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = data_males, na.action = na.omit)
    results[(i * 5 - (5 - j)), 1] <- exposure_var
    results[(i * 5 - (5 - j)), 2] <- paste("MALES_Model", j)
    results[(i * 5 - (5 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "_MALES_Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}
write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_MALES_lm_DNAmGA_withsmk", Date, ".csv")), row.names = FALSE)

write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_MALES_VIF_lm_DNAmGA_withsmk", Date, ".csv")), row.names = FALSE)

################################################################################
#             FEMALES only, mat.smk as covariate
################################################################################

data_females <- data[data$sex == 2, ]  # CHANGE if you use a different category for females

setwd(resultsdir)

# Create a function to extract coefficients, SE, p-values, and VIF for the exposure variable
extract_exposure_info <- function(model, exposure_var) {
  coef <- coef(model)[exposure_var]
  se <- summary(model)$coefficients[exposure_var, "Std. Error"]
  p_value <- summary(model)$coefficients[exposure_var, "Pr(>|t|)"]
  n_participants <- length(model$model$Bohlin)
  return(c(coef, se, p_value, n_participants))
}

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(15), 
  Model = character(15), 
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA  # Add a VIF column
)

# Create a data frame to store VIF values
vif_results <- data.frame()


# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_withsmk_FEMALES_", exposure_var, ".pdf", sep = ""))
  
  for (j in 1:5) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + mat.smk + ses.average + mat.parity")
    } else if (j == 4) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    } else if (j == 5) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + mat.smk + ses.average  + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = data_females, na.action = na.omit)
    results[(i * 5 - (5 - j)), 1] <- exposure_var
    results[(i * 5 - (5 - j)), 2] <- paste("FEMALE_Model", j)
    results[(i * 5 - (5 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "_FEMALE_Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}
write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_FEMALES_lm_DNAmGA_withsmk", Date, ".csv")), row.names = FALSE)

write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_FEMALES_VIF_lm_DNAmGA_withsmk", Date, ".csv")), row.names = FALSE)

################################################################################
#             only in mat.smk == 0 (non smoking mothers)
################################################################################

datanosmk <- data[data$mat.smk == 0, ]
datanosmk <- datanosmk[!is.na(datanosmk$mat.smk), ]

table1 <- CreateTableOne(data = datanosmk[,-1])
tab1 <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(tab1, file.path(resultsdir, paste0(cohort,"_",period, "_tabel1_nonsmk_", Date, ".csv")))

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(9),
  Model = character(9),
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA
)

# Create a data frame to store VIF values
vif_results <- data.frame()

# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_nonsmk_", exposure_var, ".pdf", sep = ""))
  
  
  for (j in 1:3) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + sex + mat.edu + mat.age + ses.average  + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = datanosmk, na.action = na.omit)
    results[(i * 3 - (3 - j)), 1] <- exposure_var
    results[(i * 3 - (3 - j)), 2] <- paste("Model", j+5)
    results[(i * 3 - (3 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "_Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}

write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_lm_DNAmGA_nonsmk", Date, ".csv")), row.names = FALSE)
write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_VIF_lm_DNAmGA_nonsmk", Date, ".csv")), row.names = FALSE)

################################################################################
#             MALES, only in mat.smk == 0 (non smoking mothers)
################################################################################

datanosmk_males <- datanosmk[datanosmk$sex == 1, ]  # CHANGE if you use a different category for males

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(9),
  Model = character(9),
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA
)

# Create a data frame to store VIF values
vif_results <- data.frame()

# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_nonsmk_MALES_", exposure_var, ".pdf", sep = ""))
  
  for (j in 1:3) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average  + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = datanosmk_males, na.action = na.omit)
    results[(i * 3 - (3 - j)), 1] <- exposure_var
    results[(i * 3 - (3 - j)), 2] <- paste("MALES_Model", j+5)
    results[(i * 3 - (3 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "MALES__Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}

write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_MALES_lm_DNAmGA_nonsmk", Date, ".csv")), row.names = FALSE)
write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_MALES_VIF_lm_DNAmGA_nonsmk", Date, ".csv")), row.names = FALSE)

################################################################################
#             FEMALES, only in mat.smk == 0 (non smoking mothers)
################################################################################

datanosmk_females <- datanosmk[datanosmk$sex == 2, ]  # CHANGE if you use a different category for females

# Create a data frame to store the results
results <- data.frame(
  Exposure_Variable = character(9),
  Model = character(9),
  Coefficient = NA, SE = NA, P_Value = NA, N_Participants = NA
)

# Create a data frame to store VIF values
vif_results <- data.frame()

# Loop through exposure variables
for (i in 1:length(exposure_vars)) {
  exposure_var <- exposure_vars[i]
  
  pdf(paste(cohort,"_",period, "_Residual_plots_nonsmk_FEMALES_", exposure_var, ".pdf", sep = ""))
  
  for (j in 1:3) {
    model <- paste("Bohlin ~", exposure_var, "+ Plate + GA") # CHANGE covariates if needed (eg. remove plate if you control for batch in another way)
    
    if (j == 2) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average + mat.parity")
    } else if (j == 3) {
      model <- paste("Bohlin ~", exposure_var, "+ Plate + GA + mat.edu + mat.age + ses.average  + mat.parity + bcell + cd4t + cd8t + nRBC + mono + nk")
    }
    
    model_fit <- lm(model, data = datanosmk_females, na.action = na.omit)
    results[(i * 3 - (3 - j)), 1] <- exposure_var
    results[(i * 3 - (3 - j)), 2] <- paste("FEMALES_Model", j+5)
    results[(i * 3 - (3 - j)), 3:6] <- extract_exposure_info(model_fit, exposure_var)
    
    # Calculate VIF for all variables in the current model
    vif_values <- car::vif(model_fit)
    
    if(!is.vector(vif_values)){
      vif_values <- (vif_values[,3])^2
    }
    
    # Convert the VIF matrix into a data frame and add a column for Model
    vif_df <- as.data.frame(vif_values)
    vif_df$Variable <- paste(rownames(vif_df), "FEMALES_Model_", j,exposure_var)
    
    # Append the VIF data frame to vif_results
    vif_results <- rbind(vif_results, vif_df)
    
    # Generate and display the residual plot with exposure and model number in the title
    res <- resid(model_fit)
    plot(fitted(model_fit), res, main = paste("Exposure:", exposure_var, "| Model:", j))
    abline(0, 0)
  }
  
  # Close the current PDF file for the current exposure variable
  dev.off()
}

write.csv(results, file.path(resultsdir, paste0(cohort,"_",period, "_FEMALES_lm_DNAmGA_nonsmk", Date, ".csv")), row.names = FALSE)
write.csv(vif_results, file.path(resultsdir, paste0(cohort,"_",period, "_FEMALES_VIF_lm_DNAmGA_nonsmk", Date, ".csv")), row.names = FALSE)


###### END
