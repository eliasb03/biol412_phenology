#=======================
# 01_analysis_v0
# Elias Bowman
# BIOL 412
# 
# 
#=======================
# analysis.data - a 4 observation dataframe with latitude summaries and sensitivities 
# group.data - a large dataframe with all of our group data bound together

# Analysis for analysis.data  ####
    #Linear models relating Latitude summaries to sensitivities
## Sensitivity vs Mean Latitude ####
mean.lat.lm <- lm(sensitivity ~ mean_latitude, data = analysis.data)
summary(mean.lat.lm)

# Create a scatterplot with points and the linear model
mean.lat.plot <- ggplot(analysis.data, aes(x = mean_latitude, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Mean Latitude vs. Sensitivity",
       x = "Mean Latitude",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)
## Sensitivity vs Min Latitude ####
min.lat.lm <- lm(sensitivity ~ min_latitude, data = analysis.data)
summary(min.lat.lm)

# Create a scatterplot with points and the linear model
min.lat.plot <- ggplot(analysis.data, aes(x = min_latitude, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Min Latitude vs. Sensitivity",
       x = "Min Latitude",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)
## Sensitivity vs Max Latitude ####
max.lat.lm <- lm(sensitivity ~ max_latitude, data = analysis.data)
summary(max.lat.lm)

# Create a scatterplot with points and the linear model
max.lat.plot <- ggplot(analysis.data, aes(x = max_latitude, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Max Latitude vs. Sensitivity",
       x = "Max Latitude",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)

## Sensitivity vs Latitudinal Range ####
range.lat.lm <- lm(sensitivity ~ latitude_range, data = analysis.data)
summary(range.lat.lm)

# Create a scatterplot with points and the linear model
range.lat.plot <- ggplot(analysis.data, aes(x = latitude_range, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Latitudinal Range vs. Sensitivity",
       x = "Latitudinal Range",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)

## Sensitivity vs Mean Elevation ####
mean.el.lm <- lm(sensitivity ~ mean_elevation, data = analysis.data)
summary(mean.el.lm)

# Create a scatterplot with points and the linear model
mean.el.plot <- ggplot(analysis.data, aes(x = mean_elevation, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Mean Elevation vs. Sensitivity",
       x = "Mean Elevation",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)

## Sensitivity vs Max Elevation ####
max.el.lm <- lm(sensitivity ~ max_elevation, data = analysis.data)
summary(max.el.lm)

# Create a scatterplot with points and the linear model
max.el.plot <- ggplot(analysis.data, aes(x = max_elevation, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Max Elevation vs. Sensitivity",
       x = "Max Elevation",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)

## Sensitivity vs Min Elevation ####
min.el.lm <- lm(sensitivity ~ min_elevation, data = analysis.data)
summary(max.el.lm)

# Create a scatterplot with points and the linear model
min.el.plot <- ggplot(analysis.data, aes(x = min_elevation, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Min Elevation vs. Sensitivity",
       x = "Min Elevation",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)

## Sensitivity vs Elevation Range ####
range.el.lm <- lm(sensitivity ~ elevation_range, data = analysis.data)
summary(range.el.lm)

# Create a scatterplot with points and the linear model
range.el.plot <- ggplot(analysis.data, aes(x = elevation_range, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Elevational Range vs. Sensitivity",
       x = "Elevation Range",
       y = "Sensitivity (day/degree Celsius)") +
  theme_minimal(base_size = 20)

## Saving Plots ####
grid_plot_latitude <- plot_grid(mean.lat.plot, max.lat.plot, min.lat.plot, range.lat.plot,
                       label_size = 15, label_fontface = "bold")
grid_plot_elevation <- plot_grid(mean.el.plot, max.el.plot, min.el.plot, range.el.plot,
                                 label_size = 15, label_fontface = "bold")
ggsave("figures/mean.latitude.png", plot = mean.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/min.latitude.png", plot = min.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/max.latitude.png", plot = max.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/range.latitude.png", plot = range.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/latitude.linear.models.png", plot = grid_plot_latitude, bg = "white", width = 10, height = 6, units = "in")

ggsave("figures/mean.elevation.png", plot = mean.el.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/min.elevation.png", plot = min.el.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/max.elevation.png", plot = max.el.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/range.elevation.png", plot = range.el.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/elevation.linear.models.png", plot = grid_plot_elevation, bg = "white", width = 10, height = 6, units = "in")

## Saving Models ####
# Extract coefficients from each linear model
lm_lat_coef_mean <- tidy(mean.lat.lm)
lm_lat_coef_max <- tidy(max.lat.lm)
lm_lat_coef_min <- tidy(min.lat.lm)
lm_lat_coefs_range <- tidy(range.lat.lm)

# Combine the coefficients into a single dataframe
lat_lm_coef <- bind_rows(lm_lat_coef_mean, lm_lat_coef_max, lm_lat_coef_min, lm_lat_coefs_range)
lat_kable_table <- kable(lat_lm_coef, format = "markdown")
lat_html_table <- kable_styling(lat_kable_table)
# Totally failed to save or format this table nicely, just did it manually

lm_el_coef_mean <- tidy(mean.el.lm)
lm_el_coef_max <- tidy(max.el.lm)
lm_el_coef_min <- tidy(min.el.lm)
lm_el_coefs_range <- tidy(range.el.lm)

# Combine the coefficients into a single dataframe
el_lm_coef <- bind_rows(lm_el_coef_mean, lm_el_coef_max, lm_el_coef_min, lm_el_coefs_range)
el_kable_table <- kable(el_lm_coef, format = "markdown")
el_html_table <- kable_styling(el_kable_table)


# Plan for group.data ####
## Compare mean, min, and max latitude
### Box plot chart comparing latitude values of different species 

## analyze distributions of other variables

# Calculate means
group.summary.stats <- group.data %>%
  summarise(
    Mean_DOY = mean(DOY),
    Mean_Year = mean(Year),
    Mean_Elevation = mean(Elevation_m),
    Mean_Latitude = mean(Latitude),
    Mean_Longitude = mean(Longitude)
  )
group.summary.stats <- t(group.summary.stats)

### Histograms ####
### list of variables to create histograms for
variables <-
  c("Elevation_m", "DOY", "Month", "Month_str", "Year", "County", "Herbarium", "PPT_Spring", "Tave_Spring", "Spring_T_Anomaly", "Latitude")

# loop to create histograms
for (variable in variables) {
  if (variable == "Elevation_m") {
    x_lab = "Elevation of Collection (metres)"
    option = 1
    title = "Elevations of Collection"
    bin_size = 50

  } else if (variable == "DOY") {
    x_lab = "Day of Collection (Day of Year)"
    option = 3
    title = "Days of Collection"
    bin_size = 50

  } else if (variable == "Month") {
    x_lab = "Month of Collection"
    option = 2
    title = "Months of Collection"

  } else if (variable == "Month_Str") {
    x_lab = "Month of Collection"
    option = 2
    title = "Months of Collection"

  } else if (variable == "Year") {
    x_lab = "Year of Collection"
    option = 4
    title = "Years of Collection"
    bin_size = 40

  } else if (variable == "County") {
    x_lab = "County of Collection"
    option = 2
    title = "Counties of Collection"

  } else if (variable == "Herbarium") {
    x_lab = "Herbarium of Collection"
    option = 2
    title = "Herbariums of Collection"

  } else if (variable == "PPT_Spring") {
    x_lab = "Spring Precipitation (mm)"
    option = 1
    title = "Spring Precipitation"

  } else if (variable == "Tave_Spring") {
    x_lab = "Spring Average Temperature (degrees Celsius)"
    option = 1
    title = "Spring Average Temperature"

  } else if (variable == "Spring_T_Anomaly") {
    x_lab = "Spring Temperature Anomaly (degrees Celsius)"
    option = 1
    title = "Spring Temperature Anomaly"

  } else if (variable == "Latitude") {
    x_lab = "Latitude of Collection"
    option = 1
    title = "Latitudes of Collection"
    bin_size = 30
  }

  if (option == 1) {
    chart <- ggplot(group.data, aes(x = !!as.name(variable))) +
      geom_histogram(fill = "skyblue", color = "black", bins = bin_size) +
      labs(
        title = paste("Histogram of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal()
  } else if (option == 2) {
    chart <- ggplot(group.data, aes(x = !!as.name(variable))) +
      geom_bar(fill = "skyblue", color = "black") +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
      labs(
        title = paste("Bar Chart of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  } else if (option == 3) {
    chart <- ggplot(group.data, aes(x = !!as.name(variable))) +
      geom_histogram(fill = "skyblue", color = "black", bins = bin_size) +
      labs(
        title = paste("Histogram of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 365))
  } else if (option == 4) {
    chart <- ggplot(group.data, aes(x = !!as.name(variable))) +
      geom_histogram(fill = "skyblue", color = "black", bins = bin_size) +
      labs(
        title = paste("Histogram of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(1915, 2021))
  }

  ggsave(paste0("figures/", variable, "_histogram.png"), plot = chart, bg = "white", width = 10, height = 6, units = "in")
}

# Scatterplot ####
## Mean Annual Temp (for all groups, coloured) vs Year
MAT.year.lm <- lm(Tave_Spring ~ Year + Species, data = group.data)
summary(MAT.year.lm)

# Create a scatterplot with points and the linear model
MAT.year.plot <- ggplot(group.data, aes(x = Year, y = Tave_Spring)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Mean Spring Temperature vs. Year",
       x = "Year",
       y = "Mean Spring Temperature (degrees)") +
  theme_minimal(base_size = 20)

## Mean Precipation (for all groups, coloured) vs Year
MPPT.year.lm <- lm(PPT_Spring ~ Year + Species, data = group.data)
summary(MPPT.year.lm)

MPPT.year.plot <- ggplot(group.data, aes(x = Year, y = PPT_Spring)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Mean Spring Precipitation vs. Year",
       x = "Year",
       y = "Mean Spring Temperature (degrees)") +
  theme_minimal(base_size = 20)

## Spring Anomaly vs Year
STA.year.lm <- lm(Spring_T_Anomaly ~ Year + Species, data = group.data)
summary(STA.year.lm)

anomaly.year.plot <- ggplot(group.data, aes(x = Year, y = Spring_T_Anomaly)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Spring Temperature Anomaly vs. Year",
       x = "Year",
       y = "Spring Temperature Anomaly (degrees from historic)") +
  theme_minimal(base_size = 20)

# Saving Plots
ggsave("figures/MAT.year.png", plot = MAT.year.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/MPPT.year.png", plot = MPPT.year.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/anomaly.year.png", plot = anomaly.year.plot, bg = "white", width = 10, height = 6, units = "in")

## Saving Models ####
# Extract coefficients from each linear model
lm_MAT_coef <- tidy(MAT.year.lm)
lm_MPPT_coef <- tidy(MPPT.year.lm)
lm_STA_coef <- tidy(STA.year.lm)

# Combine the coefficients into a single dataframe
MAT_lm_coef <- lm_MAT_coef
MAT_kable_table <- kable(MAT_lm_coef, format = "markdown")
MATp_html_table <- kable_styling(MAT_kable_table)

PPT_lm_coef <- lm_MPPT_coef
PPT_kable_table <- kable(PPT_lm_coef, format = "markdown")
PPT_html_table <- kable_styling(PPT_kable_table)

STA_lm_coef <- lm_STA_coef
STA_kable_table <- kable(STA_lm_coef, format = "markdown")
STA_html_table <- kable_styling(STA_kable_table)

## LM of DOY over latitude and range
plot_list <- list()
# Loop through each unique species
for(species in unique(group.data$Species)) {
  # Subset data for the current species
  species_data <- subset(group.data, Species == species)
  
  # Fit linear model
  model <- lm(DOY ~ Latitude, data = species_data)
  
  # Print model summary
  print(summary(model))
  
  # Create plot
  plot <- ggplot(species_data, aes(x = Latitude, y = DOY)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(title = paste("Scatterplot of Day of Year vs. Latitude for", unique(species_data$Genus), species),
         x = "Latitude",
         y = "Day of Year") +
    theme_minimal() 
  
  # Add plot to list
  plot_list[[length(plot_list) + 1]] <- plot
}

# Combine plots into a single cowplot
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)

# Save the combined plot
filename_combined <- "figures/doy_latitude_plots.png"
ggsave(filename_combined, combined_plot, width = 12, height = 8)

## LM of DOY over elevation and range
plot_list <- list()
# Loop through each unique species
for(species in unique(group.data$Species)) {
  # Subset data for the current species
  species_data <- subset(group.data, Species == species)
  
  # Fit linear model
  model <- lm(DOY ~ Elevation_m, data = species_data)
  
  # Print model summary
  print(summary(model))
  
  # Create plot
  plot <- ggplot(species_data, aes(x = Elevation_m, y = DOY)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(title = paste("Scatterplot of Day of Year vs. Elevation for", unique(species_data$Genus), species),
         x = "Elevation (m)",
         y = "Day of Year") +
    theme_minimal() 
  
  # Add plot to list
  plot_list[[length(plot_list) + 1]] <- plot
}

# Combine plots into a single cowplot
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)

# Save the combined plot
filename_combined <- "figures/doy_elevation_plots.png"
ggsave(filename_combined, combined_plot, width = 12, height = 8)

# Saving Linear Models ####
# Function to generate formatted output for linear models
generate_lm_output <- function(model) {
  # Extract formula
  formula_text <- deparse(formula(model))
  
  # Extract model coefficients
  coef_names <- names(coef(model))
  coef_values <- coef(model)
  
  # Extract p-values
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  
  # Extract F-statistics and R-squared
  f_statistic <- summary(model)$fstatistic[1]
  r_squared <- summary(model)$r.squared
  
  # Create output text for coefficients and p-values
  coef_output <- paste("Coefficients:\n")
  for (i in seq_along(coef_names)) {
    coef_output <- paste(coef_output, paste(coef_names[i], ":", coef_values[i], "\n", sep = ""), sep = "")
  }
  
  pval_output <- paste("P-values:\n")
  for (i in seq_along(coef_names)) {
    pval_output <- paste(pval_output, paste(coef_names[i], ":", p_values[i], "\n", sep = ""), sep = "")
  }
  
  # Combine output text
  output_text <- paste("Formula:", formula_text, "\n\n", 
                       coef_output, "\n\n", 
                       pval_output, "\n\n", 
                       "F-statistic:", f_statistic, "\n", 
                       "R-squared:", r_squared)
  
  return(output_text)
}

# Function to save model output as PNG
save_lm_output_as_png <- function(model, filename) {
  lm_results <- generate_lm_output(model)
  png(filename, width = 800, height = 600, units = "px")
  plot.new()
  text(0.5, 0.5, lm_results, cex = 1.2, family = "mono")
  dev.off()
}

# Create a folder called "models" if it doesn't exist
if (!file.exists("models")) {
  dir.create("models")
}

# Example list of linear models
list_of_models <- list(mean.lat.lm, min.lat.lm, max.lat.lm, range.lat.lm, mean.el.lm, max.el.lm, min.el.lm, range.el.lm, MAT.year.lm, MPPT.year.lm, anomaly.year.lm)

# Loop through each model, generate output, and save as PNG in the "models" folder
for (i in seq_along(list_of_models)) {
  filename <- paste("models/lm_result_", i, ".png", sep = "")
  save_lm_output_as_png(list_of_models[[i]], filename)
}




# Scatterplots - trendlines grouped by species 
## Mean Annual Temp (for all groups, coloured) vs Year
# List to store linear model results
lm_results <- list()

# Loop through each unique species
unique_species <- unique(group.data$Species)
for (species in unique_species) {
  # Create linear model
  lm_name <- paste("MAT.year.", species, sep = "")
  lm_model <- lm(Tave_Spring ~ Year, data = filter(group.data, Species == species))
  
  # Store linear model result
  lm_results[[lm_name]] <- lm_model
}


# Data frame to store model information
model_info <- data.frame(
  Model = character(),
  Formula = character(),
  Coefficients = character(),
  P_value = numeric(),
  Intercept = numeric(),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Populate model information
for (i in seq_along(lm_results)) {
  model_name <- (names(lm_results))[i]
  model <- lm_results[[i]]
  
  model_info <- rbind(model_info, data.frame(
    Model = model_name,
    Formula = as.character(formula(model)),
    Coefficients = paste(coef(model), collapse = ", "),
    P_value = summary(model)$coefficients[2, "Pr(>|t|)"],
    Intercept = coef(model)[1]
  ))
}

unique_models <- unique(model_info$Model)
merged_model_info <- data.frame(
  Model = character(),
  Formula = character(),
  Coefficient = character(),
  Intercept = character(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in unique_models) {
  subset_model_info <- subset(model_info, Model == model_name)
  merged_formula <- paste(unique(subset_model_info$Formula), collapse = " ")
  merged_coefficients <- unique(subset_model_info$Coefficients)
  merged_p_value <- unique(subset_model_info$P_value)
  merged_intercept <- gsub("(.+), (.+)", "\\1", merged_coefficients)  # Extracting intercept
  merged_coefficient <- gsub("(.+), (.+)", "\\2", merged_coefficients)  # Extracting coefficient
  merged_formula <- gsub("^~ ", "", merged_formula)  # Remove leading ~
  merged_model_info <- rbind(merged_model_info, data.frame(
    Model = model_name,
    Formula = merged_formula,
    Coefficient = merged_coefficient,
    Intercept = merged_intercept,
    P_value = merged_p_value
  ))
}


results <- merged_model_info

knitr::kable(results, format = "markdown")
writeLines(kable(results, format = "markdown"), "temp_model_results.md")


# Create a scatterplot with points and the linear model
MAT.year.plot.spp <- ggplot(group.data, aes(x = Year, y = Tave_Spring, color = Species)) +
  #geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Year",
       y = "Mean Spring Temperature (degrees)") +
  theme_minimal(base_size = 20)


## Mean Precipation (for all groups, coloured) vs Year
# MPPT.year.lm <- lm(PPT_Spring ~ Year + Species, data = group.data)
# summary(MPPT.year.lm)

# List to store linear model results
lm_results <- list()

# Loop through each unique species
unique_species <- unique(group.data$Species)
for (species in unique_species) {
  # Create linear model
  lm_name <- paste("MAT.year.", species, sep = "")
  lm_model <- lm(PPT_Spring ~ Year, data = filter(group.data, Species == species))
  
  # Store linear model result
  lm_results[[lm_name]] <- lm_model
}


# Data frame to store model information
model_info <- data.frame(
  Model = character(),
  Formula = character(),
  Coefficients = character(),
  P_value = numeric(),
  Intercept = numeric(),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Populate model information
for (i in seq_along(lm_results)) {
  model_name <- (names(lm_results))[i]
  model <- lm_results[[i]]
  
  model_info <- rbind(model_info, data.frame(
    Model = model_name,
    Formula = as.character(formula(model)),
    Coefficients = paste(coef(model), collapse = ", "),
    P_value = summary(model)$coefficients[2, "Pr(>|t|)"],
    Intercept = coef(model)[1]
  ))
}

unique_models <- unique(model_info$Model)
merged_model_info <- data.frame(
  Model = character(),
  Formula = character(),
  Coefficient = character(),
  Intercept = character(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in unique_models) {
  subset_model_info <- subset(model_info, Model == model_name)
  merged_formula <- paste(unique(subset_model_info$Formula), collapse = " ")
  merged_coefficients <- unique(subset_model_info$Coefficients)
  merged_p_value <- unique(subset_model_info$P_value)
  merged_intercept <- gsub("(.+), (.+)", "\\1", merged_coefficients)  # Extracting intercept
  merged_coefficient <- gsub("(.+), (.+)", "\\2", merged_coefficients)  # Extracting coefficient
  merged_formula <- gsub("^~ ", "", merged_formula)  # Remove leading ~
  merged_model_info <- rbind(merged_model_info, data.frame(
    Model = model_name,
    Formula = merged_formula,
    Coefficient = merged_coefficient,
    Intercept = merged_intercept,
    P_value = merged_p_value
  ))
}


results <- merged_model_info

knitr::kable(results, format = "markdown")
writeLines(kable(results, format = "markdown"), "precip_model_results.md")


MPPT.year.plot.spp <- ggplot(group.data, aes(x = Year, y = PPT_Spring, color = Species)) +
  #geom_point(alpha = 0.45, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Year",
       y = "Mean Precipitation Temperature (degrees)") +
  theme_minimal(base_size = 20)


## Spring Anomaly vs Year
# anomaly.year.lm <- lm(Spring_T_Anomaly ~ Year + Species, data = group.data)
# summary(STA.year.lm)

# List to store linear model results
lm_results <- list()

# Loop through each unique species
unique_species <- unique(group.data$Species)
for (species in unique_species) {
  # Create linear model
  lm_name <- paste("MAT.year.", species, sep = "")
  lm_model <- lm(Spring_T_Anomaly ~ Year, data = filter(group.data, Species == species))
  
  # Store linear model result
  lm_results[[lm_name]] <- lm_model
}


# Data frame to store model information
model_info <- data.frame(
  Model = character(),
  Formula = character(),
  Coefficients = character(),
  P_value = numeric(),
  Intercept = numeric(),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Populate model information
for (i in seq_along(lm_results)) {
  model_name <- (names(lm_results))[i]
  model <- lm_results[[i]]
  
  model_info <- rbind(model_info, data.frame(
    Model = model_name,
    Formula = as.character(formula(model)),
    Coefficients = paste(coef(model), collapse = ", "),
    P_value = summary(model)$coefficients[2, "Pr(>|t|)"],
    Intercept = coef(model)[1]
  ))
}

unique_models <- unique(model_info$Model)
merged_model_info <- data.frame(
  Model = character(),
  Formula = character(),
  Coefficient = character(),
  Intercept = character(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in unique_models) {
  subset_model_info <- subset(model_info, Model == model_name)
  merged_formula <- paste(unique(subset_model_info$Formula), collapse = " ")
  merged_coefficients <- unique(subset_model_info$Coefficients)
  merged_p_value <- unique(subset_model_info$P_value)
  merged_intercept <- gsub("(.+), (.+)", "\\1", merged_coefficients)  # Extracting intercept
  merged_coefficient <- gsub("(.+), (.+)", "\\2", merged_coefficients)  # Extracting coefficient
  merged_formula <- gsub("^~ ", "", merged_formula)  # Remove leading ~
  merged_model_info <- rbind(merged_model_info, data.frame(
    Model = model_name,
    Formula = merged_formula,
    Coefficient = merged_coefficient,
    Intercept = merged_intercept,
    P_value = merged_p_value
  ))
}


results <- merged_model_info

knitr::kable(results, format = "markdown")

writeLines(kable(results, format = "markdown"), "anamoly_model_results.md")


anomaly.year.plot.spp <- ggplot(group.data, aes(x = Year, y = Spring_T_Anomaly, color = Species)) +
  #geom_point(alpha = 0.45, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "Scatterplot of Spring Temperature Anomaly vs. Year",
       x = "Year",
       y = "Spring Temperature Anomaly (degrees from historic)") +
  theme_minimal(base_size = 20)

ggsave("figures/mat.year.spp.png", plot = MAT.year.plot.spp, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/anomaly.year.spp.png", plot = anomaly.year.plot.spp, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/precip.year.spp.png", plot = MPPT.year.plot.spp, bg = "white", width = 10, height = 6, units = "in")
