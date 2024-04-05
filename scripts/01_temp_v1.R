#=======================
# 01_temp_v1
# Elias Bowman
# BIOL 412 
# This script imports relevant temperature data (2015-2022)
# It then joins it with existing records, and currently selects for only monthly temperature data
#=======================

# New home for program on computer: C:\Users\elias\OneDrive\Documents\University\BIOL 412\phenology\412_individual\ClimateNA_v742


# importing packages
# install.packages("C:/Users/elias/Downloads/ClimateNAr_1.2.0.zip", repos = NULL, type = "win.binary")
library(ClimateNAr)


# Specify the desired file path to write the CSV file
output_dir <- "C:/Users/elias/OneDrive/Documents/University/BIOL 412/phenology/412_individual/data/raw"

# Define periods
periods <- unique(phenology.data$Year_Collected)

# Loop through each period
for (period in periods) {
  # Filter dataframe for the current period
  subset_df <- subset(phenology.data, Year == period)
  
  # Prepare input dataframe to match CSV formatting requirements
  input_df <- data.frame(ID1 = subset_df$OccurrenceID,
                         ID2 = subset_df$Year_Collected,
                         lat = subset_df$Latitude,
                         long = subset_df$Longitude,
                         el = subset_df$Elevation)
  
  # Write input dataframe to the specified CSV file path
  input_file <- file.path(output_dir, paste0("climate_input_", period, ".csv"))
  write.csv(input_df, input_file, row.names = FALSE)
  
  ## Now I use the exported input dataframes in the ClimateNA.exe program with the annual database (monthly metrics), for each year
}

##################
# Only run this once the .csvs have been made and processed by CLimateNA.exe

## For Normal Data ####
# List all the CSV files in the directory
file_list2 <- list.files(path = output_dir, pattern = "climate_input_\\d{4}_Normal_1961_1990MSY\\.csv$", full.names = TRUE)

# Initialize an empty list to store the climate data for different years
normal_data_list <- list()

# Loop through each CSV file and read it into a dataframe
for (file in file_list2) {
  # Extract the year from the file name
  year <- gsub(".*climate_input_(\\d+)_.*", "\\1", file)
  
  # Read the CSV file into a dataframe
  temp_df <- read.csv(file)
  
  # Add a column for the year

  # Add the dataframe to the list
  normal_data_list[[length(normal_data_list) + 1]] <- temp_df
}

# Combine all dataframes into a single dataframe
normal.data.after2015 <- do.call(rbind, normal_data_list)

colnames(normal.data.after2015)[c(1,2)] <- c("OccurrenceID", "Year")

columns = c("OccurrenceID", "Year", "Elevation", "Latitude", "Longitude")

normal.climate <- normal.data.after2015 %>%
  select(all_of(columns), matches("^(Tave|PPT)\\d+$"))

normal.climate <- normal.climate %>%
  rename_with(~ gsub("^Tave", "Mean_Tave", .x), starts_with("Tave")) %>%
  rename_with(~ gsub("^PPT", "Mean_PPT", .x), starts_with("PPT"))

## For Annual Data ####
file_list <- list.files(path = output_dir, pattern = "climate_input_\\d{4}_Year_\\d{4}MSY\\.csv$", full.names = TRUE)

# Initialize an empty list to store the climate data for different years
climate_data_list <- list()

# Loop through each CSV file and read it into a dataframe
for (file in file_list) {
  # Extract the year from the file name
  year <- gsub(".*climate_input_(\\d+)_.*", "\\1", file)
  
  # Read the CSV file into a dataframe
  temp_df <- read.csv(file)
  
  # Add a column for the year
  
  # Add the dataframe to the list
  climate_data_list[[length(climate_data_list) + 1]] <- temp_df
}

# Combine all dataframes into a single dataframe
climate.after2015 <- do.call(rbind, climate_data_list)

colnames(climate.after2015)[c(1,2)] <- c("OccurrenceID", "Year")

columns = c("OccurrenceID", "Year", "Elevation", "Latitude", "Longitude")

annual.climate <- climate.after2015 %>%
   select(all_of(columns), matches("^(Tave|PPT|Mean_Tave)\\d+$"))


### Joining the two datasets
# pheno.data <- full_join(phenology.data, climate.after2015, by = "OccurrenceID", suffix = c("", ".y")) %>% select(names(phenology.data), -ends_with(".y"))

after2015 <- inner_join(normal.climate, annual.climate)

result <- left_join(phenology.data, after2015)

# Identify duplicate columns
dup_cols <- intersect(names(phenology.data), paste0(names(climate.after2015), "_y"))

# Remove duplicate columns
result <- result %>% select(-one_of(dup_cols))
pheno.data <- result %>% select(-ends_with("_y"))
colnames(pheno.data)[colnames(pheno.data) == "Elevation"] <- "Elevation_m"

# Joining pre 2015 and post 2015 ##########
columns = c("Herbarium", "Accession", "Genus", "Species", "Phenology", "Day", "Month", "Year", "DOY", "Country", "State_Province", "County", "Elevation_m", "Latitude", "Longitude")
T.ovatum <- bind_rows(pheno.data, class.data)# by = joining_columns)
T.ovatum <- T.ovatum %>%
  mutate(DOY = yday(as.Date(paste(.$Year, .$Month, .$Day, sep = "-"), format = "%Y-%m-%d")))

T.ovatum.phenology <- T.ovatum %>%
  select(all_of(columns), matches("^(Tave|PPT|Mean_Tave|Mean_PPT)\\d+$")) %>%
  filter(!is.na(Mean_Tave01))

month_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
month_order <- c("January", "February", "March", "April", "May", "June", 
     "July", "August", "September", "October", "November", "December")

# Add a new column "Month_str" to the dataframe with month names
T.ovatum.phenology$Month_str <- month_names[T.ovatum.phenology$Month]
T.ovatum.phenology$Month_str <- factor(T.ovatum.phenology$Month_str, levels = month_order)


### Generating the necessary Metrics ####
# Overall Metrics
## Mean Flowering Day
T.ovatum.phenology$Mean_DOY <- mean(T.ovatum.phenology$DOY)

## Mean Spring Temp (Historical)
T.ovatum.phenology$Mean_Tave_Spring <- (T.ovatum.phenology$Mean_Tave03 + T.ovatum.phenology$Mean_Tave04 + T.ovatum.phenology$Mean_Tave05) / 3
T.ovatum.phenology$Mean_PPT_Spring <- (T.ovatum.phenology$Mean_PPT03 + T.ovatum.phenology$Mean_PPT04 + T.ovatum.phenology$Mean_PPT05)

## Mean Spring Temp (Each sample)
T.ovatum.phenology$Tave_Spring <- (T.ovatum.phenology$Tave03 + T.ovatum.phenology$Tave04 + T.ovatum.phenology$Tave05) / 3
T.ovatum.phenology$PPT_Spring <- (T.ovatum.phenology$PPT03 + T.ovatum.phenology$PPT04 + T.ovatum.phenology$PPT05)

## Temperature Anomoly
T.ovatum.phenology$Spring_T_Anomaly <- T.ovatum.phenology$Tave_Spring - T.ovatum.phenology$Mean_Tave_Spring