# Packages ----

library(shiny)
library(bslib)
library(sf)
library(ggplot2)

# Read saved objects ----

GWB_CODE_2021_GGD_SOUTH_LIMBURG <- readRDS(...) # See 2_script

DATA <- readRDS(...) # Combine postal code, GWB, and neighbourhood predictors

# Models ----

## MALE_SDH_8_FUNCTION ----

MALE_SDH_8_FUNCTION <- function(AGE,
                                WEALTH_IMP,
                                PC_EDUCATION_LOW_IMP,
                                PC_EDUCATION_INTERMEDIATE_IMP,
                                PC_EDUCATION_HIGH_IMP,
                                PC_PM25) {
  # Standardisation
  
  AGE_STD <- (AGE - 55.6382) / 8.3622 
  WEALTH_IMP_STD <- (WEALTH_IMP - 48.0596) / 10.7210
  PC_EDUCATION_LOW_IMP_STD <- (PC_EDUCATION_LOW_IMP - 71.7705) / 24.4558
  PC_EDUCATION_INTERMEDIATE_IMP_STD <- (PC_EDUCATION_INTERMEDIATE_IMP - 44.1414) / 21.4385
  PC_EDUCATION_HIGH_IMP_STD <- (PC_EDUCATION_HIGH_IMP - 35.9869) / 26.6516
  PC_PM25_STD <- (PC_PM25 - 59.5623) / 26.0015
  
  # SES
  
  SES <- 0.4819 * WEALTH_IMP_STD +
        -0.5411 * PC_EDUCATION_LOW_IMP_STD +
        -0.3451 * PC_EDUCATION_INTERMEDIATE_IMP_STD +
         0.5964 * PC_EDUCATION_HIGH_IMP_STD
  
  # Non-linear terms
  
  DATA_D_MALE_AGE_STD_T1 <- -1.611008
  DATA_D_MALE_AGE_STD_T2 <- -0.7340432
  DATA_D_MALE_AGE_STD_T3 <- -0.00656125
  DATA_D_MALE_AGE_STD_T4 <- 0.7607827
  DATA_D_MALE_AGE_STD_T5 <- 1.577954
  
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
  
  AGE_STD_RCS5K_2 <- (AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T1_TRP3 - AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 * (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T1) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4) + AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3  * (DATA_D_MALE_AGE_STD_T4 - DATA_D_MALE_AGE_STD_T1) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4)) / AGE_STD_TAU
  AGE_STD_RCS5K_3 <- (AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T2_TRP3 - AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 * (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T2) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4) + AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3  * (DATA_D_MALE_AGE_STD_T4 - DATA_D_MALE_AGE_STD_T2) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4)) / AGE_STD_TAU
  AGE_STD_RCS5K_4 <- (AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T3_TRP3 - AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T4_TRP3 * (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T3) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4) + AGE_STD_MINUS_DATA_D_MALE_AGE_STD_T5_TRP3  * (DATA_D_MALE_AGE_STD_T4 - DATA_D_MALE_AGE_STD_T3) / (DATA_D_MALE_AGE_STD_T5 - DATA_D_MALE_AGE_STD_T4)) / AGE_STD_TAU
  
  DATA_D_MALE_SES_2014_T1 <- -2.151085
  DATA_D_MALE_SES_2014_T2 <- -1.344966
  DATA_D_MALE_SES_2014_T3 <- -0.1849183
  DATA_D_MALE_SES_2014_T4 <- 1.057879
  DATA_D_MALE_SES_2014_T5 <- 2.870147
  
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
  
  SES_RCS5K_2 <- (SES_MINUS_DATA_D_MALE_SES_2014_T1_TRP3 - SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 * (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T1) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4) + SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3  * (DATA_D_MALE_SES_2014_T4 - DATA_D_MALE_SES_2014_T1) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4)) / SES_TAU
  SES_RCS5K_3 <- (SES_MINUS_DATA_D_MALE_SES_2014_T2_TRP3 - SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 * (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T2) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4) + SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3  * (DATA_D_MALE_SES_2014_T4 - DATA_D_MALE_SES_2014_T2) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4)) / SES_TAU
  SES_RCS5K_4 <- (SES_MINUS_DATA_D_MALE_SES_2014_T3_TRP3 - SES_MINUS_DATA_D_MALE_SES_2014_T4_TRP3 * (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T3) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4) + SES_MINUS_DATA_D_MALE_SES_2014_T5_TRP3  * (DATA_D_MALE_SES_2014_T4 - DATA_D_MALE_SES_2014_T3) / (DATA_D_MALE_SES_2014_T5 - DATA_D_MALE_SES_2014_T4)) / SES_TAU
  
  DATA_D_MALE_PC_PM25_2014_STD_T1 <- -1.752299
  DATA_D_MALE_PC_PM25_2014_STD_T2 <- -0.5985177
  DATA_D_MALE_PC_PM25_2014_STD_T3 <- 0.09375118
  DATA_D_MALE_PC_PM25_2014_STD_T4 <- 0.7475607
  DATA_D_MALE_PC_PM25_2014_STD_T5 <- 1.40137
  
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
  
  PC_PM25_STD_RCS5K_2 <- (PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T1_TRP3 - PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 * (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T1) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4) + PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3  * (DATA_D_MALE_PC_PM25_2014_STD_T4 - DATA_D_MALE_PC_PM25_2014_STD_T1) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4)) / PC_PM25_STD_TAU
  PC_PM25_STD_RCS5K_3 <- (PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T2_TRP3 - PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 * (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T2) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4) + PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3  * (DATA_D_MALE_PC_PM25_2014_STD_T4 - DATA_D_MALE_PC_PM25_2014_STD_T2) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4)) / PC_PM25_STD_TAU
  PC_PM25_STD_RCS5K_4 <- (PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T3_TRP3 - PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T4_TRP3 * (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T3) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4) + PC_PM25_STD_MINUS_DATA_D_MALE_PC_PM25_2014_STD_T5_TRP3  * (DATA_D_MALE_PC_PM25_2014_STD_T4 - DATA_D_MALE_PC_PM25_2014_STD_T3) / (DATA_D_MALE_PC_PM25_2014_STD_T5 - DATA_D_MALE_PC_PM25_2014_STD_T4)) / PC_PM25_STD_TAU
  
  # Linear predictor
  
  LP <- 1.4548 * AGE_STD +
       -3.4572 * AGE_STD_RCS5K_2 +
       10.0237 * AGE_STD_RCS5K_3 + 
      -10.2369 * AGE_STD_RCS5K_4 + 
       -0.2900 * SES +
        1.2026 * SES_RCS5K_2 +
       -1.8944 * SES_RCS5K_3 +
        0.4418 * SES_RCS5K_4 +
        0.1869 * PC_PM25_STD +
       -0.5651 * PC_PM25_STD_RCS5K_2 +
        3.4596 * PC_PM25_STD_RCS5K_3 +
       -7.1875 * PC_PM25_STD_RCS5K_4
  
  # Predicted risk 
  
  PRED_RISK <- 1 - 0.9357485 ** exp(LP) 
  
  # Return
  
  return(PRED_RISK)
}

## FEMALE_SDH_4_FUNCTION ----

FEMALE_SDH_4_FUNCTION <- function(AGE,
                                  WEALTH_IMP,
                                  PC_EDUCATION_LOW_IMP,
                                  PC_EDUCATION_INTERMEDIATE_IMP,
                                  PC_EDUCATION_HIGH_IMP,
                                  PC_PM25) {
  # Standardisation
  
  AGE_STD <- (AGE - 55.6368) / 8.3682 
  WEALTH_IMP_STD <- (WEALTH_IMP - 48.0748) / 10.7743
  PC_EDUCATION_LOW_IMP_STD <- (PC_EDUCATION_LOW_IMP - 71.7464) / 24.5332
  PC_EDUCATION_INTERMEDIATE_IMP_STD <- (PC_EDUCATION_INTERMEDIATE_IMP - 43.6705) / 21.2367
  PC_EDUCATION_HIGH_IMP_STD <- (PC_EDUCATION_HIGH_IMP - 36.1346) / 26.7807
  PC_PM25_STD <- (PC_PM25 - 59.5074) / 25.8898
  
  # SES
  
  SES <- 0.4834 * WEALTH_IMP_STD +
        -0.5415 * PC_EDUCATION_LOW_IMP_STD +
        -0.3448 * PC_EDUCATION_INTERMEDIATE_IMP_STD +
         0.5951 * PC_EDUCATION_HIGH_IMP_STD
  
  # Linear predictor  
  
  LP <- 0.6134 * AGE_STD +
       -0.1386 * SES +
        0.0530 * PC_PM25_STD
  
  # Predicted risk
  
  PRED_RISK <- 1 - 0.9877781 ** exp(LP) 
  
  # Return
  
  return(PRED_RISK)
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
G_NAME_2021_X_Y[7, "X"] <- 6.14
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
      h3("Sex-specific models to predict 4-year atherosclerotic cardiovascular disease risk based on age and neighbourhood characteristics in South Limburg, the Netherlands",
         style = "color:#fff")
    )
  ),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        h5(strong("Please enter the following information")),
        numericInput("AGE", 
                     label = HTML("<b>Age</b><br/>Please enter a value between 40 and 70 (excl.) years."), 
                     value = NA,
                     min = 40,
                     max = 70),
        radioButtons("MALE", 
                     label = h6(strong("Sex")),
                     choices = list("Male" = 1,
                                    "Female" = 0),
                     selected = NA),
        selectizeInput("PC6",
                       label = HTML("<b>Postal code</b><br/>If the postal code links to multiple neighbourhoods, the average of the neighbourhoods is calculated."),
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
    )
  ),
  HTML("<i>Please note further research is needed before implementing this model in clinical practice.
        <br/>To enhance data protection, please download the source code <a href = https://github.com/CARRIER-project/ASCVD-risk-models-neighbourhood/tree/main/app> here</a> and run the app locally.
        <br/>
        <br/>DISCLAIMER: THE AUTHORS WAIVE RESPONSIBILITY FOR ANY HARMS CAUSED BY THE USE OF THIS MODEL, SOFTWARE, OR WEBSITE.</i> 
        <br/>
        <br/>")
)
  
# Server ----

server <- function(input, output, session) {
    updateSelectizeInput(session,
                         "PC6",
                         choices = DATA$PC6_2021,
                         selected = NA,
                         server = TRUE)

    output$TEXT_RISK <- renderText({
        if ((!isTruthy(input$AGE)) 
            | 
            (input$AGE < 40)
            | 
            (input$AGE >= 70)
            |
            (!isTruthy(input$MALE)) 
            | 
            (!isTruthy(input$PC6)) 
            | 
            (!c(input$PC6 %in% DATA$PC6_2021))) {
            return("Please submit the requested information.") 
        } else if (input$MALE == 1) {
            RISK <- MALE_SDH_8_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                        WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                        PC_EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_LOW_2014_IMP)) })(),
                                        PC_EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                        PC_EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_HIGH_2014_IMP)) })(),
                                        PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })())
            
            TEXT_RISK <- paste("The estimated 4-year atherosclerotic cardiovascular disease risk is <b style = 'color:#e74c3c'>", paste0(paste0(format(round(RISK * 100, digits = 2), nsmall = 2), "%"), "</b>."))
            
            return(TEXT_RISK)
        } else {
            RISK <- FEMALE_SDH_4_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                          WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                          PC_EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_LOW_2014_IMP)) })(),
                                          PC_EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                          PC_EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_HIGH_2014_IMP)) })(),
                                          PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })())
            
            TEXT_RISK <- paste("The estimated 4-year atherosclerotic cardiovascular disease risk is <b style = 'color:#e74c3c'>", paste0(paste0(format(round(RISK * 100, digits = 2), nsmall = 2), "%"), "</b>."))
            
            return(TEXT_RISK)
        }
    })
    
    output$TEXT_COMPARISON <- renderText({
        if ((!isTruthy(input$AGE)) 
            | 
            (input$AGE < 40)
            | 
            (input$AGE >= 70)
            |
            (!isTruthy(input$MALE)) 
            | 
            (!isTruthy(input$PC6)) 
            | 
            (!c(input$PC6 %in% DATA$PC6_2021))) {
            return(NULL) 
        } else if (input$MALE == 1) {
            RISK <- MALE_SDH_8_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                        WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                        PC_EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_LOW_2014_IMP)) })(),
                                        PC_EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                        PC_EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_HIGH_2014_IMP)) })(),
                                        PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })())
            
            TEMP <- DATA[, c("GWB_CODE_2021",
                             "WEALTH_2014_IMP",
                             "PC_EDUCATION_LOW_2014_IMP",
                             "PC_EDUCATION_INTERMEDIATE_2014_IMP",
                             "PC_EDUCATION_HIGH_2014_IMP",
                             "PC_PM25_2014")]
            TEMP$AGE <- reactive({ return(input$AGE) })()
            
            TEMP$RISK <- MALE_SDH_8_FUNCTION(AGE = TEMP$AGE,
                                             WEALTH_IMP = TEMP$WEALTH_2014_IMP,
                                             PC_EDUCATION_LOW_IMP = TEMP$PC_EDUCATION_LOW_2014_IMP,
                                             PC_EDUCATION_INTERMEDIATE_IMP = TEMP$PC_EDUCATION_INTERMEDIATE_2014_IMP,
                                             PC_EDUCATION_HIGH_IMP = TEMP$PC_EDUCATION_HIGH_2014_IMP,
                                             PC_PM25 = TEMP$PC_PM25_2014)
            
            MIN_RISK <- min(TEMP$RISK)
            
            RISK_DIVIDED_BY_MIN_RISK <- RISK / MIN_RISK
            
            if (RISK_DIVIDED_BY_MIN_RISK == 1) {
              TEXT_COMPARISON <- paste(" This is the lowest possible risk of a male of this age.") 
              
              return(TEXT_COMPARISON) 
              } else if (RISK_DIVIDED_BY_MIN_RISK  <= 1.05) {
                TEXT_COMPARISON <- paste(" This is similar to the risk of a male of the same age living in the neighbourhood with the lowest risk.")
                
                return(TEXT_COMPARISON) 
                } else {
                  TEXT_COMPARISON <- paste("This is", format(round(RISK_DIVIDED_BY_MIN_RISK, digits = 2), nsmall = 2), "times the risk of a male of the same age living in the neighbourhood with the lowest risk.")
                  
                  return(TEXT_COMPARISON)
                  } 
        } else {
          RISK <- FEMALE_SDH_4_FUNCTION(AGE = reactive({ return(input$AGE) })(),
                                        WEALTH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014_IMP)) })(),
                                        PC_EDUCATION_LOW_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_LOW_2014_IMP)) })(),
                                        PC_EDUCATION_INTERMEDIATE_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_INTERMEDIATE_2014_IMP)) })(),
                                        PC_EDUCATION_HIGH_IMP = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_EDUCATION_HIGH_2014_IMP)) })(),
                                        PC_PM25 = reactive({ return(mean(subset(DATA, PC6_2021 == input$PC6)$PC_PM25_2014)) })())
          TEMP <- DATA[, c("GWB_CODE_2021",
                           "WEALTH_2014_IMP",
                           "PC_EDUCATION_LOW_2014_IMP",
                           "PC_EDUCATION_INTERMEDIATE_2014_IMP",
                           "PC_EDUCATION_HIGH_2014_IMP",
                           "PC_PM25_2014")]
          TEMP$AGE <- reactive({ return(input$AGE) })()
          
          TEMP$RISK <- FEMALE_SDH_4_FUNCTION(AGE = TEMP$AGE,
                                             WEALTH_IMP = TEMP$WEALTH_2014_IMP,
                                             PC_EDUCATION_LOW_IMP = TEMP$PC_EDUCATION_LOW_2014_IMP,
                                             PC_EDUCATION_INTERMEDIATE_IMP = TEMP$PC_EDUCATION_INTERMEDIATE_2014_IMP,
                                             PC_EDUCATION_HIGH_IMP = TEMP$PC_EDUCATION_HIGH_2014_IMP,
                                             PC_PM25 = TEMP$PC_PM25_2014)
          
          MIN_RISK <- min(TEMP$RISK)
          
          RISK_DIVIDED_BY_MIN_RISK <- RISK / MIN_RISK
          
          if(RISK_DIVIDED_BY_MIN_RISK == 1) {
            TEXT_COMPARISON <- paste(" This is the lowest possible risk of a female of this age.")
            
            return(TEXT_COMPARISON)
          } else if(RISK_DIVIDED_BY_MIN_RISK <= 1.05) {
            TEXT_COMPARISON <- paste(" This is similar to the risk of a female of the same age living in the neighbourhood with the lowest risk.")
            
            return(TEXT_COMPARISON) 
          } else {
            TEXT_COMPARISON <- paste("This is", format(round(RISK_DIVIDED_BY_MIN_RISK, digits = 2), nsmall = 2), "times the risk of a female of the same age living in the neighbourhood with the lowest risk.")
            
            return(TEXT_COMPARISON)
          }                
        }
    })
      
    output$TEXT_IMP <- renderText({
        if ((!isTruthy(input$AGE)) 
            | 
            (input$AGE < 40)
            | 
            (input$AGE >= 70)
            |
            (!isTruthy(input$MALE)) 
            | 
            (!isTruthy(input$PC6)) 
            | 
            (!c(input$PC6 %in% DATA$PC6_2021))) {
            return(NULL)
        } else {
            N_IMP <- reactive({ return(sum(is.na(subset(DATA, PC6_2021 == input$PC6)$WEALTH_2014))) })
            if (N_IMP() == 0) {
                return(NULL)
            } else {
              TEXT_IMP <- paste("Please note imputed values concerning the neighbourhood characteristics were used.")
              
              return(TEXT_IMP)
            }
        }
    })
    
    output$MAP <- renderPlot({
        if (isTruthy(input$AGE) 
            & 
            (input$AGE >= 40) 
            & 
            (input$AGE < 70)
            &
            isTruthy(input$MALE) 
            & 
            isTruthy(input$PC6) 
            & 
            (input$PC6 %in% DATA$PC6_2021)) { 
            GWB_CODE_2021_SELECTED <- reactive({ return(subset(DATA, PC6_2021 == input$PC6)$GWB_CODE_2021) })
            G_NAME_2021_SELECTED <- reactive({ return(subset(DATA, PC6_2021 == input$PC6)$G_NAME_2021) })
            GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED <- ifelse(GWB_CODE_2021_GGD_SOUTH_LIMBURG$GWB_CODE_2021 %in% subset(DATA, G_NAME_2021 == G_NAME_2021_SELECTED()[1])$GWB_CODE_2021,
                                                               "G",
                                                               "NOT_SELECTED")
            GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED <- ifelse(GWB_CODE_2021_GGD_SOUTH_LIMBURG$GWB_CODE_2021 %in% as.integer(GWB_CODE_2021_SELECTED()),
                                                               "B",
                                                               GWB_CODE_2021_GGD_SOUTH_LIMBURG$SELECTED)
            
            if (G_NAME_2021_SELECTED()[1] %in% G_NAME_2021_X_Y$G_NAME_2021) {
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
