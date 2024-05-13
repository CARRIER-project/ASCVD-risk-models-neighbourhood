# Packages ----

library(shiny)
library(bslib)
library(sf)
library(ggplot2)

# Read saved objects ----

GWB_CODE_2021_GGD_SOUTH_LIMBURG <- readRDS(...)

DATA <- readRDS(...)

# Models ----

## MALE_SDH_8_FUNCTION ----

MALE_SDH_8_FUNCTION <- function(AGE,
                                WEALTH_IMP,
                                EDUCATION_LOW_IMP,
                                EDUCATION_INTERMEDIATE_IMP,
                                EDUCATION_HIGH_IMP,
                                PC_PM25,
                                TIME) {
    # Standardisation
    
    AGE_STD <- (AGE - 55.85) / 8.48 
    WEALTH_IMP_STD <- (WEALTH_IMP - 48.05) / 10.72
    EDUCATION_LOW_IMP_STD <- (EDUCATION_LOW_IMP - 28.17) / 7.52
    EDUCATION_INTERMEDIATE_IMP_STD <- (EDUCATION_INTERMEDIATE_IMP - 41.93) / 4.10
    EDUCATION_HIGH_IMP_STD <- (EDUCATION_HIGH_IMP - 29.90) / 9.36
    PC_PM25_STD <- (PC_PM25 - 60.07) / 26.01
    
    # SES
    
    SES <- 0.4941481 * WEALTH_IMP_STD +
          -0.5297561 * EDUCATION_LOW_IMP_STD +
          -0.3640974 * EDUCATION_INTERMEDIATE_IMP_STD +
           0.5853283 * EDUCATION_HIGH_IMP_STD
    
    # Non-linear terms
    
    DATA_D_MALE_AGE_STD_T1 <- -1.61372
    DATA_D_MALE_AGE_STD_T2 <- -0.7387971
    DATA_D_MALE_AGE_STD_T3 <- -0.01133325
    DATA_D_MALE_AGE_STD_T4 <- 0.7652835
    DATA_D_MALE_AGE_STD_T5 <- 1.571392
    
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T1_TRP3 <- (AGE_STD - DATA_D_MALE_AGE_STD_T1) ** 3
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T1_TRP3 <- ifelse(AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T1_TRP3 <= 0,
                                                        0,
                                                        AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T1_TRP3)
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T2_TRP3 <- (AGE_STD - DATA_D_MALE_AGE_STD_T2) ** 3
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T2_TRP3 <- ifelse(AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T2_TRP3 <= 0,
                                                        0,
                                                        AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T2_TRP3)
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T3_TRP3 <- (AGE_STD - DATA_D_MALE_AGE_STD_T3) ** 3
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T3_TRP3 <- ifelse(AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T3_TRP3 <= 0,
                                                        0,
                                                        AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T3_TRP3)
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 <- (AGE_STD - DATA_D_MALE_AGE_STD_T4) ** 3
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 <- ifelse(AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 <= 0,
                                                        0,
                                                        AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3)
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3 <- (AGE_STD - DATA_D_MALE_AGE_STD_T5) ** 3
    AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3 <- ifelse(AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3 <= 0,
                                                        0,
                                                        AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3)
    
    AGE_STD_TAU <- (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T1) ** 2
    
    AGE_STD_RCS5K_1 <- AGE_STD / AGE_STD_TAU
    AGE_STD_RCS5K_2 <- (AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T1_TRP3 - AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 * (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T1) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4) + AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3  * (DATA_D_MALE_AGE_STD_T4 - DATA_D_MALE_AGE_STD_T1) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4)) / AGE_STD_TAU
    AGE_STD_RCS5K_3 <- (AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T2_TRP3 - AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 * (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T2) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4) + AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3  * (DATA_D_MALE_AGE_STD_T4 - DATA_D_MALE_AGE_STD_T2) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4)) / AGE_STD_TAU
    AGE_STD_RCS5K_4 <- (AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T3_TRP3 - AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 * (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T3) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4) + AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3  * (DATA_D_MALE_AGE_STD_T4 - DATA_D_MALE_AGE_STD_T3) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4)) / AGE_STD_TAU
    
    DATA_D_MALE_SES_2014_T1 <- -2.592317
    DATA_D_MALE_SES_2014_T2 <- -1.117932
    DATA_D_MALE_SES_2014_T3 <- -0.001548897
    DATA_D_MALE_SES_2014_T4 <- 0.9845193
    DATA_D_MALE_SES_2014_T5 <- 2.576786
    
    SES_MINUS_DATA_D_MALE_SES_2014_T1_TRP3 <- (SES - DATA_D_MALE_SES_2014_T1) ** 3
    SES_MINUS_DATA_D_MALE_SES_2014_T1_TRP3 <- ifelse(SES_MINUS_DATA_D_MALE_SES_2014_T1_TRP3 <= 0,
                                                     0,
                                                     SES_MINUS_DATA_D_MALE_SES_2014_T1_TRP3)
    SES_MINUS_DATA_D_MALE_SES_2014_T2_TRP3 <- (SES - DATA_D_MALE_SES_2014_T2) ** 3
    SES_MINUS_DATA_D_MALE_SES_2014_T2_TRP3 <- ifelse(SES_MINUS_DATA_D_MALE_SES_2014_T2_TRP3 <= 0,
                                                     0,
                                                     SES_MINUS_DATA_D_MALE_SES_2014_T2_TRP3)
    SES_MINUS_DATA_D_MALE_SES_2014_T3_TRP3 <- (SES - DATA_D_MALE_SES_2014_T3) ** 3
    SES_MINUS_DATA_D_MALE_SES_2014_T3_TRP3 <- ifelse(SES_MINUS_DATA_D_MALE_SES_2014_T3_TRP3 <= 0,
                                                     0,
                                                     SES_MINUS_DATA_D_MALE_SES_2014_T3_TRP3)
    SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 <- (SES - DATA_D_MALE_SES_2014_T4) ** 3
    SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 <- ifelse(SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 <= 0,
                                                     0,
                                                     SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3)
    SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3 <- (SES - DATA_D_MALE_SES_2014_T5) ** 3
    SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3 <- ifelse(SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3 <= 0,
                                                     0,
                                                     SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3)
    
    SES_TAU <- (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T1) ** 2
    
    SES_RCS5K_1 <- SES / SES_TAU
    SES_RCS5K_2 <- (SES_MINUS_DATA_D_MALE_SES_2014_T1_TRP3 - SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 * (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T1) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4) + SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3  * (DATA_D_MALE_SES_2014_T4 - DATA_D_MALE_SES_2014_T1) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4)) / SES_TAU
    SES_RCS5K_3 <- (SES_MINUS_DATA_D_MALE_SES_2014_T2_TRP3 - SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 * (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T2) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4) + SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3  * (DATA_D_MALE_SES_2014_T4 - DATA_D_MALE_SES_2014_T2) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4)) / SES_TAU
    SES_RCS5K_4 <- (SES_MINUS_DATA_D_MALE_SES_2014_T3_TRP3 - SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 * (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T3) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4) + SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3  * (DATA_D_MALE_SES_2014_T4 - DATA_D_MALE_SES_2014_T3) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4)) / SES_TAU
    
    DATA_D_MALE_PC_PM25_2014_STD_T1 <- -1.732751
    DATA_D_MALE_PC_PM25_2014_STD_T2 <- -0.6177976
    DATA_D_MALE_PC_PM25_2014_STD_T3 <- 0.07424248
    DATA_D_MALE_PC_PM25_2014_STD_T4 <- 0.7662826
    DATA_D_MALE_PC_PM25_2014_STD_T5 <- 1.419876
    
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T1_TRP3 <- (PC_PM25_STD - DATA_D_MALE_PC_PM25_2014_STD_T1) ** 3
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T1_TRP3 <- ifelse(PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T1_TRP3 <= 0,
                                                                     0,
                                                                     PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T1_TRP3)
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T2_TRP3 <- (PC_PM25_STD - DATA_D_MALE_PC_PM25_2014_STD_T2) ** 3
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T2_TRP3 <- ifelse(PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T2_TRP3 <= 0,
                                                                     0,
                                                                     PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T2_TRP3)
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T3_TRP3 <- (PC_PM25_STD - DATA_D_MALE_PC_PM25_2014_STD_T3) ** 3
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T3_TRP3 <- ifelse(PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T3_TRP3 <= 0,
                                                                     0,
                                                                     PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T3_TRP3)
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 <- (PC_PM25_STD - DATA_D_MALE_PC_PM25_2014_STD_T4) ** 3
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 <- ifelse(PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 <= 0,
                                                                     0,
                                                                     PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3)
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3 <- (PC_PM25_STD - DATA_D_MALE_PC_PM25_2014_STD_T5) ** 3
    PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3 <- ifelse(PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3 <= 0,
                                                                     0,
                                                                     PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3)
    
    PC_PM25_STD_TAU <- (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T1) ** 2
    
    PC_PM25_STD_RCS5K_1 <- PC_PM25_STD / PC_PM25_STD_TAU
    PC_PM25_STD_RCS5K_2 <- (PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T1_TRP3 - PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 * (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T1) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4) + PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3  * (DATA_D_MALE_PC_PM25_2014_STD_T4 - DATA_D_MALE_PC_PM25_2014_STD_T1) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4)) / PC_PM25_STD_TAU
    PC_PM25_STD_RCS5K_3 <- (PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T2_TRP3 - PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 * (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T2) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4) + PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3  * (DATA_D_MALE_PC_PM25_2014_STD_T4 - DATA_D_MALE_PC_PM25_2014_STD_T2) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4)) / PC_PM25_STD_TAU
    PC_PM25_STD_RCS5K_4 <- (PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T3_TRP3 - PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 * (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T3) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4) + PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3  * (DATA_D_MALE_PC_PM25_2014_STD_T4 - DATA_D_MALE_PC_PM25_2014_STD_T3) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4)) / PC_PM25_STD_TAU
    
    # Linear predictor
    
    LP <- 1.4618 * AGE_STD +
         -3.4776 * AGE_STD_RCS5K_2 +
         10.1494 * AGE_STD_RCS5K_3 + 
        -10.5380 * AGE_STD_RCS5K_4 + 
         -0.1522 * SES +
          0.0818 * SES_RCS5K_2 +
          0.2130 * SES_RCS5K_3 +
         -0.9444 * SES_RCS5K_4 +
          0.1855 * PC_PM25_STD +
         -0.5144 * PC_PM25_STD_RCS5K_2 +
          2.7768 * PC_PM25_STD_RCS5K_3 +
         -5.2801 * PC_PM25_STD_RCS5K_4
    
    # Time 
    
    if (TIME == 1) {
        PRED_ASCVD_RISK <- 1 - 0.9760018 ** exp(LP)
    } else if (TIME == 2) {
        PRED_ASCVD_RISK <- 1 - 0.9581361 ** exp(LP)
    } else if (TIME == 3) {
        PRED_ASCVD_RISK <- 1 - 0.9394763 ** exp(LP)
    } else if (TIME == 4) {
        PRED_ASCVD_RISK <- 1 - 0.9180089 ** exp(LP) 
    } else {
        print("Please specify a time-point of 1 year, 2 years, 3 years, or 4 years.")
    }
    
    # Return
    
    return(PRED_ASCVD_RISK)
}

## FEMALE_SDH_4_FUNCTION ----

FEMALE_SDH_4_FUNCTION <- function(AGE,
                                  WEALTH_IMP,
                                  EDUCATION_LOW_IMP,
                                  EDUCATION_INTERMEDIATE_IMP,
                                  EDUCATION_HIGH_IMP,
                                  PC_PM25,
                                  TIME) {
    # Standardisation
    
    AGE_STD <- (AGE - 55.84) / 8.47 
    WEALTH_IMP_STD <- (WEALTH_IMP - 48.07) / 10.78
    EDUCATION_LOW_IMP_STD <- (EDUCATION_LOW_IMP - 28.21) / 7.57
    EDUCATION_INTERMEDIATE_IMP_STD <- (EDUCATION_INTERMEDIATE_IMP - 41.86) / 4.09
    EDUCATION_HIGH_IMP_STD <- (EDUCATION_HIGH_IMP - 29.93) / 9.43
    PC_PM25_STD <- (PC_PM25 - 59.99) / 25.90
    
    # SES
    
    SES <- 0.4955965 * WEALTH_IMP_STD +
          -0.529561 * EDUCATION_LOW_IMP_STD +
          -0.364728 * EDUCATION_INTERMEDIATE_IMP_STD +
           0.5838858 * EDUCATION_HIGH_IMP_STD
    
    # Linear predictor  
    
    LP <- 0.6196 * AGE_STD +
         -0.1401 * SES +
          0.0509 * PC_PM25_STD
    
    # Time
    
    if (TIME == 1) {
        PRED_ASCVD_RISK <- 1 - 0.9970609 ** exp(LP)
    } else if (TIME == 2) {
        PRED_ASCVD_RISK <- 1 - 0.9938334 ** exp(LP)
    } else if (TIME == 3) {
        PRED_ASCVD_RISK <- 1 - 0.9908327 ** exp(LP)
    } else if (TIME == 4) {
        PRED_ASCVD_RISK <- 1 - 0.9874609 ** exp(LP) 
    } else {
        print("Please specify a time-point of 1 year, 2 years, 3 years, or 4 years.")
    }
    
    # Return
    
    return(PRED_ASCVD_RISK)
}

# Creating G_NAME_2021_X_Y to add map annotations ----

G_NAME_2021_X_Y <- as.data.frame(matrix(nrow = 16,
                                        ncol = 3))
colnames(G_NAME_2021_X_Y) <- c("G_NAME_2021",
                               "X",
                               "Y")
G_NAME_2021_X_Y[1, "G_NAME_2021"] <- "Beek"
G_NAME_2021_X_Y[1, "X"] <- 5.69
G_NAME_2021_X_Y[1, "Y"] <- 50.94
G_NAME_2021_X_Y[2, "G_NAME_2021"] <- "Beekdaelen"
G_NAME_2021_X_Y[2, "X"] <- 5.97
G_NAME_2021_X_Y[2, "Y"] <- 51.00
G_NAME_2021_X_Y[3, "G_NAME_2021"] <- "Brunssum"
G_NAME_2021_X_Y[3, "X"] <- 6.08
G_NAME_2021_X_Y[3, "Y"] <- 50.95
G_NAME_2021_X_Y[4, "G_NAME_2021"] <- "Eijsden-Margraten"
G_NAME_2021_X_Y[4, "X"] <- 5.78
G_NAME_2021_X_Y[4, "Y"] <- 50.74
G_NAME_2021_X_Y[5, "G_NAME_2021"] <- "Gulpen-Wittem"
G_NAME_2021_X_Y[5, "X"] <- 5.90
G_NAME_2021_X_Y[5, "Y"] <- 50.74
G_NAME_2021_X_Y[6, "G_NAME_2021"] <- "Heerlen"
G_NAME_2021_X_Y[6, "X"] <- 6.08
G_NAME_2021_X_Y[6, "Y"] <- 50.95
G_NAME_2021_X_Y[7, "G_NAME_2021"] <- "Kerkrade"
G_NAME_2021_X_Y[7, "X"] <- 5.14
G_NAME_2021_X_Y[7, "Y"] <- 50.86
G_NAME_2021_X_Y[8, "G_NAME_2021"] <- "Landgraaf"
G_NAME_2021_X_Y[8, "X"] <- 6.08
G_NAME_2021_X_Y[8, "Y"] <- 50.95
G_NAME_2021_X_Y[9, "G_NAME_2021"] <- "Maastricht"
G_NAME_2021_X_Y[9, "X"] <- 5.58
G_NAME_2021_X_Y[9, "Y"] <- 50.85
G_NAME_2021_X_Y[10, "G_NAME_2021"] <- "Meerssen"
G_NAME_2021_X_Y[10, "X"] <- 5.67
G_NAME_2021_X_Y[10, "Y"] <- 50.93
G_NAME_2021_X_Y[11, "G_NAME_2021"] <- "Simpelveld"
G_NAME_2021_X_Y[11, "X"] <- 6.06
G_NAME_2021_X_Y[11, "Y"] <- 50.79
G_NAME_2021_X_Y[12, "G_NAME_2021"] <- "Sittard-Geleen"
G_NAME_2021_X_Y[12, "X"] <- 5.97
G_NAME_2021_X_Y[12, "Y"] <- 51.03
G_NAME_2021_X_Y[13, "G_NAME_2021"] <- "Stein"
G_NAME_2021_X_Y[13, "X"] <- 5.68
G_NAME_2021_X_Y[13, "Y"] <- 50.97
G_NAME_2021_X_Y[14, "G_NAME_2021"] <- "Vaals"
G_NAME_2021_X_Y[14, "X"] <- 6.06
G_NAME_2021_X_Y[14, "Y"] <- 50.76
G_NAME_2021_X_Y[15, "G_NAME_2021"] <- "Valkenburg aan de Geul"
G_NAME_2021_X_Y[15, "X"] <- 5.58
G_NAME_2021_X_Y[15, "Y"] <- 50.90
G_NAME_2021_X_Y[16, "G_NAME_2021"] <- "Voerendaal"
G_NAME_2021_X_Y[16, "X"] <- 6.14
G_NAME_2021_X_Y[16, "Y"] <- 50.86

# UI ----

ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel(
        wellPanel(
            style = "background:#2c3e50",
            h3("Sex-specific models to predict atherosclerotic cardiovascular disease risk based on age and neighbourhood characteristics in South Limburg, the Netherlands",
               style = "color:#fff")
            )
        ),
    fluidRow(
        column(
            width = 4,
            wellPanel(
                h5(strong("Please enter the following information")),
                numericInput("AGE", 
                             label = HTML("<b> Age </b> <br> Please enter a value between 40 and 70 (excl.) years."), 
                             value = NA,
                             min = 40,
                             max = 70),
                radioButtons("MALE", 
                             label = h6(strong("Sex")),
                             choices = list("Male" = 1,
                                            "Female" = 0),
                             selected = NA),
                selectizeInput("PC6",
                               label = HTML("<b> Postal code </b> <br> If the postal code links to multiple neighbourhoods, the average of the neighbourhoods is calculated."),
                               choices = NULL,
                               size = 3,
                               options = list(create = TRUE,
                                              placeholder = "ABCD12",
                                              maxItems = 1)),
                submitButton("Submit")
            )
        ),
        column(
            width = 4,
            h5(strong("Estimated risk")),
            br(),
            htmlOutput("TEXT_RISK"),
            br(),
            textOutput("TEXT_COMPARISON"),
            br(),
            textOutput("TEXT_IMP"),
        ),
        column(
            width = 4,
            plotOutput("MAP")
        ),
    HTML("<i> Please note further research is needed before implementing this model in clinical practice. </i>"),
    )
)
    
# Server ----

server <- function(input, output, session) {
    updateSelectizeInput(session,
                         "PC6",
                         choices = DATA$PC6_2021,
                         selected = NA,
                         server = TRUE)

    output$TEXT_RISK <- renderText({
        if((!isTruthy(input$AGE)) | (!isTruthy(input$MALE)) | (!isTruthy(input$PC6) | (!c(input$PC6 %in% DATA$PC6_2021)) |
           (input$AGE < 40) | (input$AGE > 70))) {
            return("Please submit the requested information.") 
        } else {
            if(input$MALE == 1) {
                RISK <- MALE_SDH_8_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                            WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                            EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_LOW_2014_IMP)) })(),
                                            EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                            EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_HIGH_2014_IMP)) })(),
                                            PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })(),
                                            TIME = 4)
                
                return(paste("The estimated 4-year atherosclerotic cardiovascular disease risk is <b style = 'color:#e74c3c'>", paste0(paste0(round(RISK * 100, digits = 2), "%"), "</b>.")))
              } else {
                RISK <- FEMALE_SDH_4_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                              WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                              EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_LOW_2014_IMP)) })(),
                                              EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                              EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_HIGH_2014_IMP)) })(),
                                              PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })(),
                                              TIME = 4)
                
                return(paste("The estimated 4-year atherosclerotic cardiovascular disease risk is <b style = 'color:#e74c3c'>", paste0(paste0(round(RISK * 100, digits = 2), "%"), "</b>.")))
            }
        }
    })
    
    output$TEXT_COMPARISON <- renderText({
        if((!isTruthy(input$AGE)) | (!isTruthy(input$MALE)) | (!isTruthy(input$PC6) | (!c(input$PC6 %in% DATA$PC6_2021)) |
           (input$AGE < 40) | (input$AGE > 70))) {
            return(NULL) 
        } else {
            if(input$MALE == 1) {
                RISK <- MALE_SDH_8_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                            WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                            EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_LOW_2014_IMP)) })(),
                                            EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                            EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_HIGH_2014_IMP)) })(),
                                            PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })(),
                                            TIME = 4)
                
                TEMP <- DATA[, c("GWB_CODE_2021",
                                 "WEALTH_2014_IMP",
                                 "EDUCATION_LOW_2014_IMP",
                                 "EDUCATION_INTERMEDIATE_2014_IMP",
                                 "EDUCATION_HIGH_2014_IMP",
                                 "PC_PM25_2014")]
                TEMP$AGE <- reactive({ return(input$AGE) })()
                
                TEMP$RISK <- MALE_SDH_8_FUNCTION(AGE = TEMP$AGE,
                                                 WEALTH_IMP = TEMP$WEALTH_2014_IMP,
                                                 EDUCATION_LOW_IMP = TEMP$EDUCATION_LOW_2014_IMP,
                                                 EDUCATION_INTERMEDIATE_IMP = TEMP$EDUCATION_INTERMEDIATE_2014_IMP,
                                                 EDUCATION_HIGH_IMP = TEMP$EDUCATION_HIGH_2014_IMP,
                                                 PC_PM25 = TEMP$PC_PM25_2014,
                                                 TIME = 4)
                
                MIN_RISK <- min(TEMP$RISK)
                
                RISK_DIVIDED_BY_MIN_RISK <- RISK / MIN_RISK
                
                if(RISK_DIVIDED_BY_MIN_RISK == 1) {
                    return(paste(" This is the lowest possible risk of a male of this age.")) 
                    } else if(RISK_DIVIDED_BY_MIN_RISK  <= 1.05) {
                        return(paste(" This is similar to the risk of a male of the same age living in the neighbourhood with the lowest risk.")) 
                        } else {
                            return(paste("This is", round(RISK_DIVIDED_BY_MIN_RISK, digits = 2),"times the risk of a male of the same age living in the neighbourhood with the lowest risk."))
                        }
                } else {
                    RISK <- FEMALE_SDH_4_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                                  WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                                  EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_LOW_2014_IMP)) })(),
                                                  EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                                  EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$EDUCATION_HIGH_2014_IMP)) })(),
                                                  PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })(),
                                                  TIME = 4)
                    TEMP <- DATA[, c("GWB_CODE_2021",
                                     "WEALTH_2014_IMP",
                                     "EDUCATION_LOW_2014_IMP",
                                     "EDUCATION_INTERMEDIATE_2014_IMP",
                                     "EDUCATION_HIGH_2014_IMP",
                                     "PC_PM25_2014")]
                    TEMP$AGE <- reactive({ return(input$AGE) })()
                    
                    TEMP$RISK <- FEMALE_SDH_4_FUNCTION(AGE = TEMP$AGE,
                                                       WEALTH_IMP = TEMP$WEALTH_2014_IMP,
                                                       EDUCATION_LOW_IMP = TEMP$EDUCATION_LOW_2014_IMP,
                                                       EDUCATION_INTERMEDIATE_IMP = TEMP$EDUCATION_INTERMEDIATE_2014_IMP,
                                                       EDUCATION_HIGH_IMP = TEMP$EDUCATION_HIGH_2014_IMP,
                                                       PC_PM25 = TEMP$PC_PM25_2014,
                                                       TIME = 4)
                    
                    MIN_RISK <- min(TEMP$RISK)
                    
                    RISK_DIVIDED_BY_MIN_RISK <- RISK / MIN_RISK
                    
                    if(RISK_DIVIDED_BY_MIN_RISK == 1) {
                        return(paste(" This is the lowest possible risk of a female of this age."))
                    } else if(RISK_DIVIDED_BY_MIN_RISK <= 1.05){
                        return(paste(" This is similar to the risk of a female of the same age living in the neighbourhood with the lowest risk.")) 
                    } else {
                        return(paste("This is", round(RISK_DIVIDED_BY_MIN_RISK, digits = 2),"times the risk of a female of the same age living in the neighbourhood with the lowest risk."))
                    }                
                }
        }
    })
    
    output$TEXT_IMP <- renderText({
        if((!isTruthy(input$AGE)) | (!isTruthy(input$MALE)) | (!isTruthy(input$PC6) | (!c(input$PC6 %in% DATA$PC6_2021)) |
           (input$AGE < 40) | (input$AGE > 70))) {
            return(NULL)
        } else {
            N_IMP <- reactive({ return(sum(is.na(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014))) })
            if(N_IMP() == 0) {
                return(NULL)
            } else {
                return("Please note imputed values concerning the neighbourhood characteristics were used.")
            }
        }
    })
    
    output$MAP <- renderPlot({
        if(isTruthy(input$AGE) & isTruthy(input$MALE) & isTruthy(input$PC6) & (input$PC6 %in% DATA$PC6_2021) 
           & (input$AGE >= 40) & (input$AGE <= 70)) { 
            GWB_CODE_2021_SELECTED <- reactive({ return(subset(DATA, PC6_2021 == input$PC6)$GWB_CODE_2021) })
            G_NAME_2021_SELECTED <- reactive({ return(subset(DATA, PC6_2021 == input$PC6)$G_NAME_2021) })
            GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED <- ifelse(GWB_CODE_2021_GGD_SOUTH_LIMBURG$GWB_CODE_2021 %in% subset(DATA, G_NAME_2021 == G_NAME_2021_SELECTED()[1])$GWB_CODE_2021,
                                                               "G",
                                                               "NOT_SELECTED")
            GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED <- ifelse(GWB_CODE_2021_GGD_SOUTH_LIMBURG$GWB_CODE_2021 %in% as.integer(GWB_CODE_2021_SELECTED()),
                                                               "B",
                                                               GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED)
            
            if(G_NAME_2021_SELECTED()[1] %in% G_NAME_2021_X_Y$G_NAME_2021){
                MAP <- 
                    ggplot() +
                    geom_sf(data = GWB_CODE_2021_GGD_SOUTH_LIMBURG, aes(fill = as.factor(GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED))) +
                    coord_sf(default_crs = sf::st_crs(4326)) +
                    scale_x_continuous(limits = c(5.5, 6.2)) + 
                    scale_y_continuous(limits = c(50.73, 51.07)) +
                    scale_fill_manual(values = c("B" = "#2c3e50",
                                                 "G" = "#b4bcc2",
                                                 "NOT_SELECTED" = "white"),
                                      guide = "none") +
                    annotate("text",
                             x = subset(G_NAME_2021_X_Y, G_NAME_2021_SELECTED()[1] == G_NAME_2021)$X,
                             y = subset(G_NAME_2021_X_Y, G_NAME_2021_SELECTED()[1] == G_NAME_2021)$Y,
                             label = G_NAME_2021_SELECTED()[1]) +
                    theme_void() 
                return(MAP)  
            } 
            } else {
                MAP <- 
                    ggplot() +
                    geom_sf(data = GWB_CODE_2021_GGD_SOUTH_LIMBURG, fill = "white") +
                    coord_sf(default_crs = sf::st_crs(4326)) +
                    scale_x_continuous(limits = c(5.5, 6.2)) + 
                    scale_y_continuous(limits = c(50.73, 51.07)) +
                    theme_void()
                return(MAP)
            }
        })
    }

# Run ----

shinyApp(ui = ui, server = server)
