#=======================
# 00_setup_v1
# Elias, Lucas, Lydia, Justin
# BIOL 412 
# Phenology Work for BIOL 412 Trillium Ovatum
#=======================
# Setup ####
# importing packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)

# Importing plant specific phenology data that we created from individual analysis
t.ovatum <- read.csv("data/t.ovatum.csv")
v.adunca <- read.csv("data/v.adunca.csv")
c.uniflora <- read.csv("data/c.uniflora.csv")

colnames(v.adunca)
colnames(t.ovatum)

# Processing of Dataframes so that they match each other
## Very Elaborate Column Mapping ####
# mapping between column names in v.adunca and t.ovatum
v.adunca_column_mapping <- c(
  "X" = "X",
  "Herbarium" = "Herbarium",
  "Accession" = "Accession",
  "Genus" = "Genus",
  "Species" = "Species",
  "Phenology" = "Phenology",
  "Day" = "Day",
  "Month" = "Month",
  "Year" = "Year",
  "DOY" = "DOY",
  "Country" = "Country",
  "State_Province" = "State_Province",
  "County" = "County",
  "Elevation_m" = "Elevation_m",
  "Latitude" = "Latitude",
  "Longitude" = "Longitude",
  "MeanTave_01" = "Mean_Tave01",
  "MeanTave_02" = "Mean_Tave02",
  "MeanTave_03" = "Mean_Tave03",
  "MeanTave_04" = "Mean_Tave04",
  "MeanTave_05" = "Mean_Tave05",
  "MeanTave_06" = "Mean_Tave06",
  "MeanTave_07" = "Mean_Tave07",
  "MeanTave_08" = "Mean_Tave08",
  "MeanTave_09" = "Mean_Tave09",
  "MeanTave_10" = "Mean_Tave10",
  "MeanTave_11" = "Mean_Tave11",
  "MeanTave_12" = "Mean_Tave12",
  "MeanPrec_01" = "Mean_PPT01",
  "MeanPrec_02" = "Mean_PPT02",
  "MeanPrec_03" = "Mean_PPT03",
  "MeanPrec_04" = "Mean_PPT04",
  "MeanPrec_05" = "Mean_PPT05",
  "MeanPrec_06" = "Mean_PPT06",
  "MeanPrec_07" = "Mean_PPT07",
  "MeanPrec_08" = "Mean_PPT08",
  "MeanPrec_09" = "Mean_PPT09",
  "MeanPrec_10" = "Mean_PPT10",
  "MeanPrec_11" = "Mean_PPT11",
  "MeanPrec_12" = "Mean_PPT12",
  "Tave_01" = "Tave01",
  "Tave_02" = "Tave02",
  "Tave_03" = "Tave03",
  "Tave_04" = "Tave04",
  "Tave_05" = "Tave05",
  "Tave_06" = "Tave06",
  "Tave_07" = "Tave07",
  "Tave_08" = "Tave08",
  "Tave_09" = "Tave09",
  "Tave_10" = "Tave10",
  "Tave_11" = "Tave11",
  "Tave_12" = "Tave12",
  "Prec_01" = "PPT01",
  "Prec_02" = "PPT02",
  "Prec_03" = "PPT03",
  "Prec_04" = "PPT04",
  "Prec_05" = "PPT05",
  "Prec_06" = "PPT06",
  "Prec_07" = "PPT07",
  "Prec_08" = "PPT08",
  "Prec_09" = "PPT09",
  "Prec_10" = "PPT10",
  "Prec_11" = "PPT11",
  "Prec_12" = "PPT12",
  "Mean_DOY" = "Mean_DOY",
  "Annual_Spring_T" = "Tave_Spring",
  "Normal_Spring_T" = "Mean_Tave_Spring",
  "Annual_Prec_Spring" = "PPT_Spring",
  "Normal_Prec_Spring" = "Mean_PPT_Spring",
  "Spring_T_Anomaly" = "Spring_T_Anomaly"
)

# Create mapping for c.uniflora
c.uniflora_column_mapping <- c(
  "X" = "X",
  "Herbarium" = "Herbarium",
  "Accession" = "Accession",
  "Genus" = "Genus",
  "Phenology" = "Phenology",
  "DOY" = "DOY",
  "elev.true" = "Elevation_m", 
  "Latitude" = "Latitude",
  "Longitude" = "Longitude",
  "SpringTemp.Modern" = "Tave_Spring", 
  "SpringTemp.Historic" = "Mean_Tave_Spring", 
  "SpringPrec.Modern" = "PPT_Spring", 
  "SpringPrec.Historic" = "Mean_PPT_Spring", 
  "Anomaly" = "Spring_T_Anomaly",  
  "PPT.Anomaly" = NA,  # No direct match found
  "Year.Collected" = "Year",
  "Month.Collected" = "Month",
  "spec.epithet" = "Species"
)
#####
## Use Reverse Mapping to Match Column Names
for (i in seq_along(colnames(v.adunca))) {
  colname <- colnames(v.adunca)[i]
  if (colname %in% names(v.adunca_column_mapping)) {
    colnames(v.adunca)[i] <- v.adunca_column_mapping[[colname]]
  }
}
for (i in seq_along(colnames(c.uniflora))) {
  colname <- colnames(c.uniflora)[i]
  if (colname %in% names(c.uniflora_column_mapping)) {
    colnames(c.uniflora)[i] <- c.uniflora_column_mapping[[colname]]
  }
}
# Missing lydias data

# Selecting for relevent data
## Joining datasets into total df
total.dataframe <- bind_rows(t.ovatum, v.adunca, c.uniflora, .id = "source")

## Creating a list of all the columns that every data set should have
column.list <- c("X", "Herbarium", "Accession", "Genus", "Species", "Phenology", "Day", "Month", "Year", "DOY", "Country", "State_Province", "County", "Elevation_m", "Latitude", "Longitude", "Month_str", "Mean_DOY", "Mean_Tave_Spring", "Mean_PPT_Spring", "Tave_Spring", "PPT_Spring", "Spring_T_Anomaly", "Mean_Tave01", "Mean_Tave02", "Mean_Tave03", "Mean_Tave04", "Mean_Tave05", "Mean_Tave06", "Mean_Tave07", "Mean_Tave08", "Mean_Tave09", "Mean_Tave10", "Mean_Tave11", "Mean_Tave12", "Mean_PPT01", "Mean_PPT02", "Mean_PPT03", "Mean_PPT04", "Mean_PPT05", "Mean_PPT06", "Mean_PPT07", "Mean_PPT08", "Mean_PPT09","Mean_PPT10", "Mean_PPT11", "Mean_PPT12", "Tave01", "Tave02", "Tave03", "Tave04", "Tave05", "Tave06", "Tave07", "Tave08", "Tave09", "Tave10", "Tave11", "Tave12", "PPT01", "PPT02", "PPT03", "PPT04", "PPT05", "PPT06", "PPT07", "PPT08", "PPT09", "PPT10", "PPT11", "PPT12")

## Selecting for the columns
group.data <- total.dataframe %>%
  select(all_of(column.list))

## Recoding phenology
group.data$Phenology <- ifelse(grepl("Fruit", group.data$Phenology, ignore.case = TRUE), "Fruiting", group.data$Phenology)
group.data$Phenology <- ifelse(grepl("Flower", group.data$Phenology, ignore.case = TRUE), "Flowering", group.data$Phenology)
group.data$Phenology <- ifelse(grepl("Vegetative", group.data$Phenology, ignore.case = TRUE), "Vegetative", group.data$Phenology)

# Extracting sensitivity values for each species

# 

