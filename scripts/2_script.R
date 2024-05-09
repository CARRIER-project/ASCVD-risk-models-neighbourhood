# Packages ----

library(raster)
library(sf)
library(stars)

# Data ----

PM25_2014 <- raster(...)
PM25_2015 <- raster(...)

GWB_CODE_2021 <- st_read(...)
GWB_CODE_2021 <- GWB_CODE_2021[, c('BU_CODE', 'geometry')]
colnames(GWB_CODE_2021)[1] <- "GWB_CODE_2021"
GWB_CODE_2021$GWB_CODE_2021 <- as.integer(substr(GWB_CODE_2021$GWB_CODE_2021, 3, 10))

GGDG_2021 <- read.csv(...)  
GGDG_2021 <- GGDG_2021[, c("Code_1",
                           "Naam_2",
                           "Code_14",
                           "Naam_15")]
colnames(GGDG_2021) <- c("G_CODE_2021",
                         "G_NAME_2021",
                         "GGD_CODE_2021",
                         "GGD_NAME_2021")
GGDG_2021$G_CODE_2021 <- as.numeric(substr(GGDG_2021$G_CODE_2021, 3, 6))
GGDG_2021$GGD_CODE_2021 <- as.numeric(substr(GGDG_2021$GGD_CODE_2021, 3, 6))

# Subsetting GGD South Limburg in map of the Netherlands 2021 ----

GWB_CODE_2021_GGD_SOUTH_LIMBURG <- GWB_CODE_2021
GWB_CODE_2021_GGD_SOUTH_LIMBURG$GGD_SOUTH_LIMBURG_2021 <- NA
GGDG_2021_GGD_SOUTH_LIMBURG <- subset(GGDG_2021, GGD_CODE_2021 == 6106)
for (i in 1:nrow(GWB_CODE_2021_GGD_SOUTH_LIMBURG)) {
  if(nchar(st_drop_geometry(GWB_CODE_2021_GGD_SOUTH_LIMBURG)[i, "GWB_CODE_2021"]) == 6) {
    GWB_CODE_2021_GGD_SOUTH_LIMBURG[i, "GGD_SOUTH_LIMBURG_2021"] <- ifelse(substr(st_drop_geometry(GWB_CODE_2021_GGD_SOUTH_LIMBURG)[i, "GWB_CODE_2021"], 1, 2)
                                                                           %in%
                                                                             GGDG_2021_GGD_SOUTH_LIMBURG$G_CODE_2021,
                                                                           TRUE,
                                                                           FALSE)
  }
  if(nchar(st_drop_geometry(GWB_CODE_2021_GGD_SOUTH_LIMBURG)[i, "GWB_CODE_2021"]) == 7) {
    GWB_CODE_2021_GGD_SOUTH_LIMBURG[i, "GGD_SOUTH_LIMBURG_2021"] <- ifelse(substr(st_drop_geometry(GWB_CODE_2021_GGD_SOUTH_LIMBURG)[i, "GWB_CODE_2021"], 1, 3)
                                                                           %in%
                                                                             GGDG_2021_GGD_SOUTH_LIMBURG$G_CODE_2021,
                                                                           TRUE,
                                                                           FALSE)
  }
  if(nchar(st_drop_geometry(GWB_CODE_2021_GGD_SOUTH_LIMBURG)[i, "GWB_CODE_2021"]) == 8) {
    GWB_CODE_2021_GGD_SOUTH_LIMBURG[i, "GGD_SOUTH_LIMBURG_2021"] <- ifelse(substr(st_drop_geometry(GWB_CODE_2021_GGD_SOUTH_LIMBURG)[i, "GWB_CODE_2021"], 1, 4)
                                                                           %in%
                                                                             GGDG_2021_GGD_SOUTH_LIMBURG$G_CODE_2021,
                                                                           TRUE,
                                                                           FALSE)
  }
}
GWB_CODE_2021_GGD_SOUTH_LIMBURG <- subset(GWB_CODE_2021_GGD_SOUTH_LIMBURG, GGD_SOUTH_LIMBURG_2021 == TRUE)
GWB_CODE_2021_GGD_SOUTH_LIMBURG <- GWB_CODE_2021_GGD_SOUTH_LIMBURG[, -3]
GWB_CODE_2021_GGD_SOUTH_LIMBURG$GWB_CODE_2021 <- as.factor(GWB_CODE_2021_GGD_SOUTH_LIMBURG$GWB_CODE_2021)

st_write(...)

# Creating objects average and percentile PM25 per GWB-code in 2021 per year for GGD South-Limburg ----

## 2014 ----

crs(PM25_2014) <- "+init=epsg:28992"
PM25_2014 <- st_as_sf(st_as_stars(PM25_2014))
TEMP_2014 <- st_join(GWB_CODE_2021_GGD_SOUTH_LIMBURG,
                     PM25_2014,
                     left = TRUE)
TEMP_2014 <- st_set_geometry(TEMP_2014, NULL)
GWB_CODE_2021_PM25_2014 <- aggregate(TEMP_2014[, "conc_pm25_2014"], 
                                     list(factor(TEMP_2014[, "GWB_CODE_2021"])),
                                     mean)
colnames(GWB_CODE_2021_PM25_2014) <- c("GWB_CODE_2021",
                                       "PM25_2014")
PC_PM25_2014 <- ecdf(GWB_CODE_2021_PM25_2014$PM25_2014)(GWB_CODE_2021_PM25_2014$PM25_2014)
PC_PM25_2014 <- round(PC_PM25_2014 * 100, digits = 0)
GWB_CODE_2021_PM25_2014 <- cbind(GWB_CODE_2021_PM25_2014,
                                 PC_PM25_2014)

write.csv(...)

## 2015 ----

crs(PM25_2015) <- "+init=epsg:28992"
PM25_2015 <- st_as_sf(st_as_stars(PM25_2015))
TEMP_2015 <- st_join(GWB_CODE_2021_GGD_SOUTH_LIMBURG,
                     PM25_2015,
                     left = TRUE)
TEMP_2015 <- st_set_geometry(TEMP_2015, NULL)
GWB_CODE_2021_PM25_2015 <- aggregate(TEMP_2015[, "conc_pm25_2015"], 
                                     list(factor(TEMP_2015[, "GWB_CODE_2021"])),
                                     mean)
colnames(GWB_CODE_2021_PM25_2015) <- c("GWB_CODE_2021",
                                       "PM25_2015")
PC_PM25_2015 <- ecdf(GWB_CODE_2021_PM25_2015$PM25_2015)(GWB_CODE_2021_PM25_2015$PM25_2015)
PC_PM25_2015 <- round(PC_PM25_2015 * 100, digits = 0)
GWB_CODE_2021_PM25_2015 <- cbind(GWB_CODE_2021_PM25_2015,
                                 PC_PM25_2015)

write.csv(...)

# Session info ----

sessionInfo()
