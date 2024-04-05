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
       y = "Sensitivity") +
  theme_minimal()
## Sensitivity vs Min Latitude ####
min.lat.lm <- lm(sensitivity ~ min_latitude, data = analysis.data)
summary(min.lat.lm)

# Create a scatterplot with points and the linear model
min.lat.plot <- ggplot(analysis.data, aes(x = min_latitude, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Min Latitude vs. Sensitivity",
       x = "Min Latitude",
       y = "Sensitivity") +
  theme_minimal()
## Sensitivity vs Max Latitude ####
max.lat.lm <- lm(sensitivity ~ max_latitude, data = analysis.data)
summary(max.lat.lm)

# Create a scatterplot with points and the linear model
max.lat.plot <- ggplot(analysis.data, aes(x = max_latitude, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Max Latitude vs. Sensitivity",
       x = "Max Latitude",
       y = "Sensitivity") +
  theme_minimal()

## Sensitivity vs Latitudinal Range ####
range.lat.lm <- lm(sensitivity ~ latitude_range, data = analysis.data)
summary(range.lat.lm)

# Create a scatterplot with points and the linear model
range.lat.plot <- ggplot(analysis.data, aes(x = latitude_range, y = sensitivity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Latitudinal Range vs. Sensitivity",
       x = "Latitudinal Range",
       y = "Sensitivity") +
  theme_minimal()

## Saving Plots ####
grid_plot <- plot_grid(mean.lat.plot, max.lat.plot, min.lat.plot, range.lat.plot,
                       label_size = 15, label_fontface = "bold")
ggsave("figures/mean.latitude.png", plot = mean.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/min.latitude.png", plot = min.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/max.latitude.png", plot = max.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/range.latitude.png", plot = range.lat.plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/linear.models.png", plot = grid_plot, bg = "white", width = 10, height = 6, units = "in")

## Saving Models ####
# Extract coefficients from each linear model
lm_coef_mean <- tidy(mean.lat.lm)
lm_coef_max <- tidy(max.lat.lm)
lm_coef_min <- tidy(min.lat.lm)
lm_coefs_range <- tidy(range.lat.lm)

# Combine the coefficients into a single dataframe
all_lm_coef <- bind_rows(lm_coef_mean, lm_coef_max, lm_coef_min, lm_coefs_range)
kable_table <- kable(all_lm_coef, format = "markdown")
html_table <- kable_styling(kable_table)
# Totally failed to save or format this table nicely, just did it manually



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

# ############ Old work from elias' file might be helpful later #################
# # Some Preliminary Scatter Plots
# # Scatterplot of Day of Year vs. Year with linear regression line and annotations
# day_year_plot <- ggplot(T.ovatum.phenology, aes(x = Year, y = DOY)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
#   labs(
#     title = "Year Collected vs. Day of Year Collected",
#     x = "Year",
#     y = "Day of Year"
#   ) +
#   theme_minimal() +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$DOY) - 0.1 * diff(range(T.ovatum.phenology$DOY)),
#     label = paste("R^2 =", round(summary(lm(DOY ~ Year, data = T.ovatum.phenology))$r.squared, 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$DOY) - 0.15 * diff(range(T.ovatum.phenology$DOY)),
#     label = paste("p =", round(summary(lm(DOY ~ Year, data = T.ovatum.phenology))$coef[2, 4], 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$DOY) - 0.2 * diff(range(T.ovatum.phenology$DOY)),
#     label = paste("y =", round(coef(lm(DOY ~ Year, data = T.ovatum.phenology))[1], 2), "+",
#                   round(coef(lm(DOY ~ Year, data = T.ovatum.phenology))[2], 2), "x")
#   )
# 
# # Scatterplot of Collection Date vs. Elevation with linear regression line and annotations
# date_elevation_plot <- ggplot(T.ovatum.phenology, aes(x = DOY, y = Elevation_m)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
#   labs(
#     title = "Elevation vs. Collection Date",
#     x = "Collection Date",
#     y = "Elevation (m)"
#   ) +
#   theme_minimal() +
#   annotate(
#     "text", x = min(T.ovatum.phenology$DOY) + 0.1 * diff(range(T.ovatum.phenology$DOY)),
#     y = max(T.ovatum.phenology$Elevation_m) - 0.1 * diff(range(T.ovatum.phenology$Elevation_m)),
#     label = paste("R^2 =", round(summary(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))$r.squared, 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$DOY) + 0.1 * diff(range(T.ovatum.phenology$DOY)),
#     y = max(T.ovatum.phenology$Elevation_m) - 0.15 * diff(range(T.ovatum.phenology$Elevation_m)),
#     label = paste("p =", round(summary(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))$coef[2, 4], 20))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$DOY) + 0.1 * diff(range(T.ovatum.phenology$DOY)),
#     y = max(T.ovatum.phenology$Elevation_m) - 0.2 * diff(range(T.ovatum.phenology$Elevation_m)),
#     label = paste("y =", round(coef(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))[1], 2), "+",
#                   round(coef(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))[2], 2), "x")
#   )
# 
# # Scatterplot of Elevation vs. Year with linear regression line and annotations
# elevation_year_plot <- ggplot(T.ovatum.phenology, aes(x = Year, y = Elevation_m)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
#   labs(
#     title = "Elevation vs. Year Collected",
#     y = "Elevation (m)",
#     x = "Year Collected"
#   ) +
#   theme_minimal() +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$Elevation_m) - 0.1 * diff(range(T.ovatum.phenology$Elevation_m)),
#     label = paste("R^2 =", round(summary(lm(Elevation_m ~ Year, data = T.ovatum.phenology))$r.squared, 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$Elevation_m) - 0.15 * diff(range(T.ovatum.phenology$Elevation_m)),
#     label = paste("p =", round(summary(lm(Elevation_m ~ Year, data = T.ovatum.phenology))$coef[2, 4], 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$Elevation_m) - 0.2 * diff(range(T.ovatum.phenology$Elevation_m)),
#     label = paste("y =", round(coef(lm(Elevation_m ~ Year, data = T.ovatum.phenology))[1], 2), "+",
#                   round(coef(lm(Elevation_m ~ Year, data = T.ovatum.phenology))[2], 2), "x")
#   )
# 
# # Scatterplot of Mean Spring Temperature vs. Year with linear regression line and annotations
# spring_temp_year_plot <- ggplot(T.ovatum.phenology, aes(x = Year, y = Tave_Spring)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
#   labs(
#     title = "Mean Spring Temperature vs. Year",
#     y = "Mean Spring Temperature",
#     x = "Year"
#   ) +
#   theme_minimal() +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$Tave_Spring) - 0.1 * diff(range(T.ovatum.phenology$Tave_Spring)),
#     label = paste("R^2 =", round(summary(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))$r.squared, 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$Tave_Spring) - 0.15 * diff(range(T.ovatum.phenology$Tave_Spring)),
#     label = paste("p =", round(summary(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))$coef[2, 4], 5))
#   ) +
#   annotate(
#     "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
#     y = max(T.ovatum.phenology$Tave_Spring) - 0.2 * diff(range(T.ovatum.phenology$Tave_Spring)),
#     label = paste("y =", round(coef(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))[1], 2), "+",
#                   round(coef(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))[2], 2), "x")
#   )
# 
# # Save scatterplots
# ggsave("figures/day_year_scatterplot.png", plot = day_year_plot, bg = "white", width = 10, height = 6, units = "in")
# ggsave("figures/date_elevation_scatterplot.png", plot = date_elevation_plot, bg = "white", width = 10, height = 6, units = "in")
# ggsave("figures/elevation_year_scatterplot.png", plot = elevation_year_plot, bg = "white", width = 10, height = 6, units = "in")
# ggsave("figures/spring_temp_year_scatterplot.png", plot = spring_temp_year_plot, bg = "white", width = 10, height = 6, units = "in")
# 
# 
# 
# # Temperature Sensitivity
# T.ovatum.flowering <- T.ovatum.phenology %>%
#   filter(Phenology == "Flowering")
# 
# write.csv(T.ovatum.flowering, "data/processed/t.ovatum.csv")
# 
# # Fit linear regression model with reversed relationship
# sensitivty_model <- lm(DOY ~ Spring_T_Anomaly, data = T.ovatum.flowering)
# 
# # Get coefficient, p-value, and R-squared value
# coef <- coef(sensitivty_model)[2]
# p_value <- summary(sensitivty_model)$coefficients[2, 4]
# r_squared <- summary(sensitivty_model)$r.squared
# 
# # Create plot with reversed relationship
# spring_temp_anomaly_doy_plot <- ggplot(T.ovatum.flowering, aes(x = Spring_T_Anomaly, y = DOY)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
#   annotate("text", x = min(T.ovatum.flowering$Spring_T_Anomaly) + 1, y = max(T.ovatum.flowering$DOY) - 15,
#            label = paste("Slope =", round(coef, 2), "\nP-value =", round(p_value, 3), "\nR-squared =", round(r_squared, 3))) +  # Add annotation for slope, p-value, and R-squared
#   labs(
#     title = "T. ovatum Sensitivity: Day of Year Flowering Plants Collected vs. Spring Temperature Anomaly",
#     x = "Spring Temperature Anomaly",
#     y = "Day of Year Flowering Plants Collected"
#   ) +
#   theme_minimal()
# 
# # Save the plot with annotations
# ggsave("figures/spring_temp_anomaly_doy_plot.png", plot = spring_temp_anomaly_doy_plot, bg = "white", width = 10, height = 6, units = "in")
# 
# 
# # Quick Map Work
# # Install and load the necessary packages
# library(leaflet)
# library(mapview)
# 
# # Create a leaflet map
# map <- leaflet(data = T.ovatum.phenology) %>%
#   addTiles() %>%
#   addMarkers(~Longitude, ~Latitude, popup = ~paste("Longitude:", Longitude, "<br>", "Latitude:", Latitude))
# 
# # Save the map as a PNG file
# mapshot(map, file = "figures/map_output.png", width = 800, height = 600)
# 
# # Creating a Table for Means
# library(kableExtra)
# library(webshot)
# 

