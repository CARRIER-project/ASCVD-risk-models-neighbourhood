# Data ----

WEALTHEDUCATION20142019 <- read.csv(...)

# Creating objects wealth indicators per GWB-code in 2021 per year ----
# Imputation: missing values at buurt level are imputed with corresponding wijk level; if these are missing as well, imputation with corresponding gemeente level.

WEALTH20142019 <- WEALTHEDUCATION20142019[, c(2, 3, 5, 6)]
colnames(WEALTH20142019) <- c("GWB_CODE_MIXED_2021",
                              "YEAR",
                              "N_HOUSEHOLDS",
                              "WEALTH")
WEALTH20142019$YEAR <- as.numeric(substr(WEALTH20142019$YEAR, 1, 4)) 

## 2014 ----

GWB_CODE_2021_WEALTH_2014 <- subset(WEALTH20142019, 
                                    (YEAR == 2014) 
                                    & 
                                    (substr(GWB_CODE_MIXED_2021, 1, 2) == "BU"))
colnames(GWB_CODE_2021_WEALTH_2014)[1] <- "GWB_CODE_2021"
colnames(GWB_CODE_2021_WEALTH_2014)[4] <- "WEALTH_2014"

table(is.na(GWB_CODE_2021_WEALTH_2014$WEALTH_2014))

GWB_CODE_2021_WEALTH_2014$WEALTH_2014_IMP <- GWB_CODE_2021_WEALTH_2014$WEALTH_2014

GW_CODE_2021_WEALTH_2014 <- subset(WEALTH20142019, 
                                   (YEAR == 2014) 
                                   & 
                                   (substr(GWB_CODE_MIXED_2021, 1, 2) == "WK"))
colnames(GW_CODE_2021_WEALTH_2014)[1] <- "GW_CODE_2021"
colnames(GW_CODE_2021_WEALTH_2014)[4] <- "WEALTH_2014"
for (i in 1:nrow(GWB_CODE_2021_WEALTH_2014)) {
  if (is.na(GWB_CODE_2021_WEALTH_2014[i, "WEALTH_2014_IMP"])) {
    GWB_CODE_2021_WEALTH_2014[i, "WEALTH_2014_IMP"] <- subset(GW_CODE_2021_WEALTH_2014,
                                                              substr(GW_CODE_2021, 3, 8) 
                                                              == 
                                                              substr(GWB_CODE_2021_WEALTH_2014[i, "GWB_CODE_2021"], 3, 8))[, "WEALTH_2014"]
  }
}

table(is.na(GWB_CODE_2021_WEALTH_2014$WEALTH_2014_IMP))

G_CODE_2021_WEALTH_2014 <- subset(WEALTH20142019, 
                                  (YEAR == 2014) 
                                  &
                                  (substr(GWB_CODE_MIXED_2021, 1, 2) == "GM"))
colnames(G_CODE_2021_WEALTH_2014)[1] <- "G_CODE_2021"
colnames(G_CODE_2021_WEALTH_2014)[4] <- "WEALTH_2014"
for (i in 1:nrow(GWB_CODE_2021_WEALTH_2014)) {
  if (is.na(GWB_CODE_2021_WEALTH_2014[i, "WEALTH_2014_IMP"])) {
    GWB_CODE_2021_WEALTH_2014[i, "WEALTH_2014_IMP"] <- subset(G_CODE_2021_WEALTH_2014,
                                                              substr(G_CODE_2021, 3, 6) 
                                                              == 
                                                              substr(GWB_CODE_2021_WEALTH_2014[i, "GWB_CODE_2021"], 3, 6))[, "WEALTH_2014"]
  }
}

table(is.na(GWB_CODE_2021_WEALTH_2014$WEALTH_2014_IMP))

GWB_CODE_2021_WEALTH_2014$GWB_CODE_2021 <- as.integer(substr(GWB_CODE_2021_WEALTH_2014$GWB_CODE_2021, 3, 10))
GWB_CODE_2021_WEALTH_2014 <- GWB_CODE_2021_WEALTH_2014[, c("GWB_CODE_2021",
                                                           "WEALTH_2014",
                                                           "WEALTH_2014_IMP")]

write.csv(...)

## 2015 ----

GWB_CODE_2021_WEALTH_2015 <- subset(WEALTH20142019, 
                                    (YEAR == 2015) 
                                    &
                                    (substr(GWB_CODE_MIXED_2021, 1, 2) == "BU"))
colnames(GWB_CODE_2021_WEALTH_2015)[1] <- "GWB_CODE_2021"
colnames(GWB_CODE_2021_WEALTH_2015)[4] <- "WEALTH_2015"

table(is.na(GWB_CODE_2021_WEALTH_2015$WEALTH_2015))

GWB_CODE_2021_WEALTH_2015$WEALTH_2015_IMP <- GWB_CODE_2021_WEALTH_2015$WEALTH_2015

GW_CODE_2021_WEALTH_2015 <- subset(WEALTH20142019, 
                                   (YEAR == 2015) 
                                   &
                                   (substr(GWB_CODE_MIXED_2021, 1, 2) == "WK"))
colnames(GW_CODE_2021_WEALTH_2015)[1] <- "GW_CODE_2021"
colnames(GW_CODE_2021_WEALTH_2015)[4] <- "WEALTH_2015"
for (i in 1:nrow(GWB_CODE_2021_WEALTH_2015)) {
  if (is.na(GWB_CODE_2021_WEALTH_2015[i, "WEALTH_2015_IMP"])) {
    GWB_CODE_2021_WEALTH_2015[i, "WEALTH_2015_IMP"] <- subset(GW_CODE_2021_WEALTH_2015,
                                                              substr(GW_CODE_2021, 3, 8) 
                                                              == 
                                                              substr(GWB_CODE_2021_WEALTH_2015[i, "GWB_CODE_2021"], 3, 8))[, "WEALTH_2015"]
  }
}

table(is.na(GWB_CODE_2021_WEALTH_2015$WEALTH_2015_IMP))

G_CODE_2021_WEALTH_2015 <- subset(WEALTH20142019, 
                                  (YEAR == 2015) 
                                  &
                                  (substr(GWB_CODE_MIXED_2021, 1, 2) == "GM"))
colnames(G_CODE_2021_WEALTH_2015)[1] <- "G_CODE_2021"
colnames(G_CODE_2021_WEALTH_2015)[4] <- "WEALTH_2015"
for (i in 1:nrow(GWB_CODE_2021_WEALTH_2015)) {
  if (is.na(GWB_CODE_2021_WEALTH_2015[i, "WEALTH_2015_IMP"])) {
    GWB_CODE_2021_WEALTH_2015[i, "WEALTH_2015_IMP"] <- subset(G_CODE_2021_WEALTH_2015,
                                                              substr(G_CODE_2021, 3, 6) 
                                                              == 
                                                              substr(GWB_CODE_2021_WEALTH_2015[i, "GWB_CODE_2021"], 3, 6))[, "WEALTH_2015"]
  }
}

table(is.na(GWB_CODE_2021_WEALTH_2015$WEALTH_2015_IMP))

GWB_CODE_2021_WEALTH_2015$GWB_CODE_2021 <- as.integer(substr(GWB_CODE_2021_WEALTH_2015$GWB_CODE_2021, 3, 10))
GWB_CODE_2021_WEALTH_2015 <- GWB_CODE_2021_WEALTH_2015[, c("GWB_CODE_2021",
                                                           "WEALTH_2015",
                                                           "WEALTH_2015_IMP")]

write.csv(...)

# Creating objects education indicators per GWB-code in 2021 per year ----
# Imputation: missing values at buurt level are imputed with corresponding wijk level; if these are missing as well, imputation with corresponding gemeente level.

EDUCATION20142019 <- WEALTHEDUCATION20142019[, c(2, 3, 5, 7, 8, 9)]
colnames(EDUCATION20142019) <- c("GWB_CODE_MIXED_2021",
                                 "YEAR",
                                 "N_HOUSEHOLDS",
                                 "EDUCATION_LOW",
                                 "EDUCATION_INTERMEDIATE",
                                 "EDUCATION_HIGH")

EDUCATION20142019$YEAR <- as.numeric(substr(EDUCATION20142019$YEAR, 1, 4)) 

## 2014 ----

GWB_CODE_2021_EDUCATION_2014 <- subset(EDUCATION20142019, 
                                       (YEAR == 2014) 
                                       &
                                       (substr(GWB_CODE_MIXED_2021, 1, 2) == "BU"))
colnames(GWB_CODE_2021_EDUCATION_2014)[1] <- "GWB_CODE_2021"
colnames(GWB_CODE_2021_EDUCATION_2014)[4] <- "EDUCATION_LOW_2014"
colnames(GWB_CODE_2021_EDUCATION_2014)[5] <- "EDUCATION_INTERMEDIATE_2014"
colnames(GWB_CODE_2021_EDUCATION_2014)[6] <- "EDUCATION_HIGH_2014"

table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014))
table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014))
table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014))

GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014_IMP <- GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014
GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014_IMP <- GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014
GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014_IMP <- GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014

GW_CODE_2021_EDUCATION_2014 <- subset(EDUCATION20142019, 
                                      (YEAR == 2014) 
                                      &
                                      (substr(GWB_CODE_MIXED_2021, 1, 2) == "WK"))
colnames(GW_CODE_2021_EDUCATION_2014)[1] <- "GW_CODE_2021"
colnames(GW_CODE_2021_EDUCATION_2014)[4] <- "EDUCATION_LOW_2014"
colnames(GW_CODE_2021_EDUCATION_2014)[5] <- "EDUCATION_INTERMEDIATE_2014"
colnames(GW_CODE_2021_EDUCATION_2014)[6] <- "EDUCATION_HIGH_2014"
for (i in 1:nrow(GWB_CODE_2021_EDUCATION_2014)) {
  if (is.na(GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_LOW_2014_IMP"])) {
    GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_LOW_2014_IMP"] <- subset(GW_CODE_2021_EDUCATION_2014,
                                                                        substr(GW_CODE_2021, 3, 8) 
                                                                        == 
                                                                        substr(GWB_CODE_2021_EDUCATION_2014[i, "GWB_CODE_2021"], 3, 8))[, "EDUCATION_LOW_2014"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_INTERMEDIATE_2014_IMP"])) {
    GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_INTERMEDIATE_2014_IMP"] <- subset(GW_CODE_2021_EDUCATION_2014,
                                                                                 substr(GW_CODE_2021, 3, 8) 
                                                                                 == 
                                                                                 substr(GWB_CODE_2021_EDUCATION_2014[i, "GWB_CODE_2021"], 3, 8))[, "EDUCATION_INTERMEDIATE_2014"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_HIGH_2014_IMP"])) {
    GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_HIGH_2014_IMP"] <- subset(GW_CODE_2021_EDUCATION_2014,
                                                                         substr(GW_CODE_2021, 3, 8) 
                                                                         == 
                                                                         substr(GWB_CODE_2021_EDUCATION_2014[i, "GWB_CODE_2021"], 3, 8))[, "EDUCATION_HIGH_2014"]
  }
}

table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014_IMP))

G_CODE_2021_EDUCATION_2014 <- subset(EDUCATION20142019, 
                                     (YEAR == 2014) 
                                     &
                                     (substr(GWB_CODE_MIXED_2021, 1, 2) == "GM"))
colnames(G_CODE_2021_EDUCATION_2014)[1] <- "G_CODE_2021"
colnames(G_CODE_2021_EDUCATION_2014)[4] <- "EDUCATION_LOW_2014"
colnames(G_CODE_2021_EDUCATION_2014)[5] <- "EDUCATION_INTERMEDIATE_2014"
colnames(G_CODE_2021_EDUCATION_2014)[6] <- "EDUCATION_HIGH_2014"
for (i in 1:nrow(GWB_CODE_2021_EDUCATION_2014)) {
  if (is.na(GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_LOW_2014_IMP"])) {
    GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_LOW_2014_IMP"] <- subset(G_CODE_2021_EDUCATION_2014,
                                                                        substr(G_CODE_2021, 3, 6) 
                                                                        == 
                                                                        substr(GWB_CODE_2021_EDUCATION_2014[i, "GWB_CODE_2021"], 3, 6))[, "EDUCATION_LOW_2014"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_INTERMEDIATE_2014_IMP"])) {
    GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_INTERMEDIATE_2014_IMP"] <- subset(G_CODE_2021_EDUCATION_2014,
                                                                                 substr(G_CODE_2021, 3, 6) 
                                                                                 == 
                                                                                 substr(GWB_CODE_2021_EDUCATION_2014[i, "GWB_CODE_2021"], 3, 6))[, "EDUCATION_INTERMEDIATE_2014"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_HIGH_2014_IMP"])) {
    GWB_CODE_2021_EDUCATION_2014[i, "EDUCATION_HIGH_2014_IMP"] <- subset(G_CODE_2021_EDUCATION_2014,
                                                                         substr(G_CODE_2021, 3, 6) 
                                                                         == 
                                                                         substr(GWB_CODE_2021_EDUCATION_2014[i, "GWB_CODE_2021"], 3, 6))[, "EDUCATION_HIGH_2014"]
  }
}

table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014_IMP))

PC_EDUCATION_LOW_2014_IMP <- ecdf(GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014_IMP)(GWB_CODE_2021_EDUCATION_2014$EDUCATION_LOW_2014_IMP)
PC_EDUCATION_LOW_2014_IMP <- round(PC_EDUCATION_LOW_2014_IMP * 100, digits = 0)
GWB_CODE_2021_EDUCATION_2014 <- cbind(GWB_CODE_2021_EDUCATION_2014,
                                      PC_EDUCATION_LOW_2014_IMP)

PC_EDUCATION_INTERMEDIATE_2014_IMP <- ecdf(GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014_IMP)(GWB_CODE_2021_EDUCATION_2014$EDUCATION_INTERMEDIATE_2014_IMP)
PC_EDUCATION_INTERMEDIATE_2014_IMP <- round(PC_EDUCATION_INTERMEDIATE_2014_IMP * 100, digits = 0)
GWB_CODE_2021_EDUCATION_2014 <- cbind(GWB_CODE_2021_EDUCATION_2014,
                                      PC_EDUCATION_INTERMEDIATE_2014_IMP)

PC_EDUCATION_HIGH_2014_IMP <- ecdf(GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014_IMP)(GWB_CODE_2021_EDUCATION_2014$EDUCATION_HIGH_2014_IMP)
PC_EDUCATION_HIGH_2014_IMP <- round(PC_EDUCATION_HIGH_2014_IMP * 100, digits = 0)
GWB_CODE_2021_EDUCATION_2014 <- cbind(GWB_CODE_2021_EDUCATION_2014,
                                      PC_EDUCATION_HIGH_2014_IMP)

GWB_CODE_2021_EDUCATION_2014$GWB_CODE_2021 <- as.integer(substr(GWB_CODE_2021_EDUCATION_2014$GWB_CODE_2021, 3, 10))
GWB_CODE_2021_EDUCATION_2014 <- GWB_CODE_2021_EDUCATION_2014[, c("GWB_CODE_2021",
                                                                 "EDUCATION_LOW_2014",
                                                                 "EDUCATION_LOW_2014_IMP",
                                                                 "EDUCATION_INTERMEDIATE_2014",
                                                                 "EDUCATION_INTERMEDIATE_2014_IMP",
                                                                 "EDUCATION_HIGH_2014",
                                                                 "EDUCATION_HIGH_2014_IMP",
                                                                 "PC_EDUCATION_LOW_2014_IMP",
                                                                 "PC_EDUCATION_INTERMEDIATE_2014_IMP",
                                                                 "PC_EDUCATION_HIGH_2014_IMP")]

write.csv(...)

## 2015 ----

GWB_CODE_2021_EDUCATION_2015 <- subset(EDUCATION20142019, 
                                       (YEAR == 2015) 
                                       &
                                       (substr(GWB_CODE_MIXED_2021, 1, 2) == "BU"))
colnames(GWB_CODE_2021_EDUCATION_2015)[1] <- "GWB_CODE_2021"
colnames(GWB_CODE_2021_EDUCATION_2015)[4] <- "EDUCATION_LOW_2015"
colnames(GWB_CODE_2021_EDUCATION_2015)[5] <- "EDUCATION_INTERMEDIATE_2015"
colnames(GWB_CODE_2021_EDUCATION_2015)[6] <- "EDUCATION_HIGH_2015"

table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015))
table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015))
table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015))

GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015_IMP <- GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015
GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015_IMP <- GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015
GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015_IMP <- GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015

GW_CODE_2021_EDUCATION_2015 <- subset(EDUCATION20142019, 
                                      (YEAR == 2015) 
                                      &
                                      (substr(GWB_CODE_MIXED_2021, 1, 2) == "WK"))
colnames(GW_CODE_2021_EDUCATION_2015)[1] <- "GW_CODE_2021"
colnames(GW_CODE_2021_EDUCATION_2015)[4] <- "EDUCATION_LOW_2015"
colnames(GW_CODE_2021_EDUCATION_2015)[5] <- "EDUCATION_INTERMEDIATE_2015"
colnames(GW_CODE_2021_EDUCATION_2015)[6] <- "EDUCATION_HIGH_2015"
for (i in 1:nrow(GWB_CODE_2021_EDUCATION_2015)) {
  if (is.na(GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_LOW_2015_IMP"])) {
    GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_LOW_2015_IMP"] <- subset(GW_CODE_2021_EDUCATION_2015,
                                                                        substr(GW_CODE_2021, 3, 8) 
                                                                        == 
                                                                        substr(GWB_CODE_2021_EDUCATION_2015[i, "GWB_CODE_2021"], 3, 8))[, "EDUCATION_LOW_2015"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_INTERMEDIATE_2015_IMP"])) {
    GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_INTERMEDIATE_2015_IMP"] <- subset(GW_CODE_2021_EDUCATION_2015,
                                                                                 substr(GW_CODE_2021, 3, 8) 
                                                                                 == 
                                                                                 substr(GWB_CODE_2021_EDUCATION_2015[i, "GWB_CODE_2021"], 3, 8))[, "EDUCATION_INTERMEDIATE_2015"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_HIGH_2015_IMP"])) {
    GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_HIGH_2015_IMP"] <- subset(GW_CODE_2021_EDUCATION_2015,
                                                                         substr(GW_CODE_2021, 3, 8) 
                                                                         == 
                                                                         substr(GWB_CODE_2021_EDUCATION_2015[i, "GWB_CODE_2021"], 3, 8))[, "EDUCATION_HIGH_2015"]
  }
}

table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015_IMP))

G_CODE_2021_EDUCATION_2015 <- subset(EDUCATION20142019, 
                                     (YEAR == 2015) 
                                     &
                                     (substr(GWB_CODE_MIXED_2021, 1, 2) == "GM"))
colnames(G_CODE_2021_EDUCATION_2015)[1] <- "G_CODE_2021"
colnames(G_CODE_2021_EDUCATION_2015)[4] <- "EDUCATION_LOW_2015"
colnames(G_CODE_2021_EDUCATION_2015)[5] <- "EDUCATION_INTERMEDIATE_2015"
colnames(G_CODE_2021_EDUCATION_2015)[6] <- "EDUCATION_HIGH_2015"
for (i in 1:nrow(GWB_CODE_2021_EDUCATION_2015)) {
  if (is.na(GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_LOW_2015_IMP"])) {
    GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_LOW_2015_IMP"] <- subset(G_CODE_2021_EDUCATION_2015,
                                                                        substr(G_CODE_2021, 3, 6) 
                                                                        == 
                                                                        substr(GWB_CODE_2021_EDUCATION_2015[i, "GWB_CODE_2021"], 3, 6))[, "EDUCATION_LOW_2015"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_INTERMEDIATE_2015_IMP"])) {
    GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_INTERMEDIATE_2015_IMP"] <- subset(G_CODE_2021_EDUCATION_2015,
                                                                                 substr(G_CODE_2021, 3, 6) 
                                                                                 == 
                                                                                 substr(GWB_CODE_2021_EDUCATION_2015[i, "GWB_CODE_2021"], 3, 6))[, "EDUCATION_INTERMEDIATE_2015"]
  }
  if (is.na(GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_HIGH_2015_IMP"])) {
    GWB_CODE_2021_EDUCATION_2015[i, "EDUCATION_HIGH_2015_IMP"] <- subset(G_CODE_2021_EDUCATION_2015,
                                                                         substr(G_CODE_2021, 3, 6) 
                                                                         == 
                                                                         substr(GWB_CODE_2021_EDUCATION_2015[i, "GWB_CODE_2021"], 3, 6))[, "EDUCATION_HIGH_2015"]
  }
}

table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015_IMP))
table(is.na(GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015_IMP))

PC_EDUCATION_LOW_2015_IMP <- ecdf(GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015_IMP)(GWB_CODE_2021_EDUCATION_2015$EDUCATION_LOW_2015_IMP)
PC_EDUCATION_LOW_2015_IMP <- round(PC_EDUCATION_LOW_2015_IMP * 100, digits = 0)
GWB_CODE_2021_EDUCATION_2015 <- cbind(GWB_CODE_2021_EDUCATION_2015,
                                      PC_EDUCATION_LOW_2015_IMP)

PC_EDUCATION_INTERMEDIATE_2015_IMP <- ecdf(GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015_IMP)(GWB_CODE_2021_EDUCATION_2015$EDUCATION_INTERMEDIATE_2015_IMP)
PC_EDUCATION_INTERMEDIATE_2015_IMP <- round(PC_EDUCATION_INTERMEDIATE_2015_IMP * 100, digits = 0)
GWB_CODE_2021_EDUCATION_2015 <- cbind(GWB_CODE_2021_EDUCATION_2015,
                                      PC_EDUCATION_INTERMEDIATE_2015_IMP)

PC_EDUCATION_HIGH_2015_IMP <- ecdf(GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015_IMP)(GWB_CODE_2021_EDUCATION_2015$EDUCATION_HIGH_2015_IMP)
PC_EDUCATION_HIGH_2015_IMP <- round(PC_EDUCATION_HIGH_2015_IMP * 100, digits = 0)
GWB_CODE_2021_EDUCATION_2015 <- cbind(GWB_CODE_2021_EDUCATION_2015,
                                      PC_EDUCATION_HIGH_2015_IMP)

GWB_CODE_2021_EDUCATION_2015$GWB_CODE_2021 <- as.integer(substr(GWB_CODE_2021_EDUCATION_2015$GWB_CODE_2021, 3, 10))
GWB_CODE_2021_EDUCATION_2015 <- GWB_CODE_2021_EDUCATION_2015[, c("GWB_CODE_2021",
                                                                 "EDUCATION_LOW_2015",
                                                                 "EDUCATION_LOW_2015_IMP",
                                                                 "EDUCATION_INTERMEDIATE_2015",
                                                                 "EDUCATION_INTERMEDIATE_2015_IMP",
                                                                 "EDUCATION_HIGH_2015",
                                                                 "EDUCATION_HIGH_2015_IMP",
                                                                 "PC_EDUCATION_LOW_2015_IMP",
                                                                 "PC_EDUCATION_INTERMEDIATE_2015_IMP",
                                                                 "PC_EDUCATION_HIGH_2015_IMP")]

write.csv(...)

# Session info ----

sessionInfo()
