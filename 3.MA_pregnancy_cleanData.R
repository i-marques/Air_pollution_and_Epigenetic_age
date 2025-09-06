#### Air pollution & Epigenetic Age
#### pregnancy 

setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA")

#### File Harmonization

#ALPAC (no2 pm10 pm25 bc)
ALSPAC <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results_cohorts/ALSPAC_results/pregnancy/ALSPAC_pregnancy_lm_DNAmGA_withsmk_noGRAN2024-05-13.csv")
ALSPAC$cohort <- "ALSPAC"
### this splits the text in the "Exposure_Variable" so that we can get the exact exp name
split_values <- strsplit(as.character(ALSPAC$Exposure_Variable), "_")
### extract the corresponding exposure from the split text
ALSPAC$exp <- sapply(1:nrow(ALSPAC), function(i) {
  if (i >= 16 & i <= 20) {
    return(split_values[[i]][2])
  } else {
    return(split_values[[i]][1])
  }
})
ALSPAC$model_exp <- paste(ALSPAC$Model, ALSPAC$exp, sep = "_")
summary(as.factor(ALSPAC$exp))


# EDEN (bc, no2, pm10, pm25)
EDEN <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results_cohorts/EDEN_310124_noGran/Pregnancy/EDEN_lm_pregDNAmGA_withsmk2024-01-31.csv")
EDEN$cohort <- "EDEN"
split_values <- strsplit(as.character(EDEN$Exposure_Variable), "_")
EDEN$exp <- sapply(split_values, "[[", 2)
summary(as.factor(EDEN$exp))
EDEN$model_exp <- paste(EDEN$Model, EDEN$exp, sep = "_")


# ENVIRONAGE 450K (bc  no2 pm10 pm25)
ENVIRONAGE_450k <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results_cohorts/ENVIRONAGE_results/k450/ENVIRONAGE_lm_pregDNAmGA_withsmk2024-01-25.csv")
ENVIRONAGE_450k$cohort <- "ENVIRONAGE_450k"
split_values <- strsplit(as.character(ENVIRONAGE_450k$Exposure_Variable), "_")
ENVIRONAGE_450k$exp <- sapply(split_values, "[[", 2)
ENVIRONAGE_450k$exp <- sub("pm25abs", "bc", ENVIRONAGE_450k$exp)
summary(as.factor(ENVIRONAGE_450k$exp))
ENVIRONAGE_450k$model_exp <- paste(ENVIRONAGE_450k$Model, ENVIRONAGE_450k$exp, sep = "_")


# ENVIRONAGE EPIC (bc  no2 pm10 pm25)
ENVIRONAGE_EPIC <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results_cohorts/ENVIRONAGE_results/EPIC/ENVIRONAGE_lm_pregDNAmGA_withsmk2024-01-25.csv")
ENVIRONAGE_EPIC$cohort <- "ENVIRONAGE_EPIC"
split_values <- strsplit(as.character(ENVIRONAGE_EPIC$Exposure_Variable), "_")
ENVIRONAGE_EPIC$exp <- sapply(split_values, "[[", 2)
ENVIRONAGE_EPIC$exp <- sub("pm25abs", "bc", ENVIRONAGE_EPIC$exp)
summary(as.factor(ENVIRONAGE_EPIC$exp))
ENVIRONAGE_EPIC$model_exp <- paste(ENVIRONAGE_EPIC$Model, ENVIRONAGE_EPIC$exp, sep = "_")


# GenR 450k (no2  nox pm10 pm25 bc)
GenR_450k <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results/Pregnancy_LifeCycle/GenR_pregnancy_lm_DNAmGA_withsmk_noGRAN2024-01-25.csv")
GenR_450k$cohort <- "GenR_450k"
split_values <- strsplit(as.character(GenR_450k$Exposure_Variable), "_")
GenR_450k$exp <- sapply(split_values, "[[", 2)
summary(as.factor(GenR_450k$exp))
### pm25abs will be renamed to bc
GenR_450k$exp <- sub("pm25abs", "bc", GenR_450k$exp)
summary(as.factor(GenR_450k$exp))
GenR_450k$model_exp <- paste(GenR_450k$Model, GenR_450k$exp, sep = "_")


# GenR EPIC
GenR_EPIC <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results/Pregnancy_EPIC_LifeCycle/GenR_EPIC_pregnancy_lm_DNAmGA_withsmk_noGRAN2024-04-20.csv")
GenR_EPIC$cohort <- "GenR_EPIC"
split_values <- strsplit(as.character(GenR_EPIC$Exposure_Variable), "_")
GenR_EPIC$exp <- sapply(split_values, "[[", 2)
GenR_EPIC$exp <- sub("pm25abs", "bc", GenR_EPIC$exp)
summary(as.factor(GenR_EPIC$exp))
GenR_EPIC$model_exp <- paste(GenR_EPIC$Model, GenR_EPIC$exp, sep = "_")


# INMA (bc  no2 pm10 pm25)
INMA <- read.csv("Z:/1_My_Projects_/5. Air Pollution & EpiGA/Results_cohorts/INMA/pregnancy/INMA_pregnancy_lm_DNAmGA_withsmk2024-08-14.csv")
INMA$cohort <- "INMA"
split_values <- strsplit(as.character(INMA$Exposure_Variable), "_")
INMA$exp <- sapply(split_values, "[[", 1)
INMA$exp <- sub("pm25abs", "bc", INMA$exp)
summary(as.factor(INMA$exp))
INMA$model_exp <- paste(INMA$Model, INMA$exp, sep = "_")


################### Combined data with all cohort models for pregnancy period
setwd("Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy")
library(metafor)
MA_pregnancy <- rbind(ALSPAC, EDEN, ENVIRONAGE_450k, ENVIRONAGE_EPIC, GenR_450k, GenR_EPIC, INMA)
write.csv(MA_pregnancy, file = "Z:/1_My_Projects_/5. Air Pollution & EpiGA/MA/Pregnancy/MA_pregnancy_08-Oct-2024.csv",
          quote = F, row.names = F)

# How many cohorts per model and exposure?
table(MA_pregnancy$model_exp)


