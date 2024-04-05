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

# Importing plant specific phenology data that we created from individual analysis
t.ovatum <- read.csv("data/t.ovatum.csv")

# Processing of Dataframes so that they match each other

# Joining datasets into total df
total.dataframe <- rbind(t.ovatum, ...)

# Extracting sensitivity values for each species

# 

