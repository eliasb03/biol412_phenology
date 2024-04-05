#=======================
# 02_analysis_v0
# Elias Bowman
# BIOL 412
#
#=======================
library(ggplot2)
# Histograms ####
# creating a series of histograms, one for each relevant variable in T.ovatum.phenology
# list of variables to create histograms for
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
    chart <- ggplot(T.ovatum.phenology, aes(x = !!as.name(variable))) +
      geom_histogram(fill = "skyblue", color = "black", bins = bin_size) +
      labs(
        title = paste("Histogram of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal()
  } else if (option == 2) {
    chart <- ggplot(T.ovatum.phenology, aes(x = !!as.name(variable))) +
      geom_bar(fill = "skyblue", color = "black") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
      labs(
        title = paste("Bar Chart of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  } else if (option == 3) {
    chart <- ggplot(T.ovatum.phenology, aes(x = !!as.name(variable))) +
      geom_histogram(fill = "skyblue", color = "black", bins = bin_size) +
      labs(
        title = paste("Histogram of", title),
        x = x_lab,
        y = "Frequency"
      ) +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 365))
  } else if (option == 4) {
    chart <- ggplot(T.ovatum.phenology, aes(x = !!as.name(variable))) +
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


# Some Preliminary Scatter Plots ####
# Scatterplot of Day of Year vs. Year with linear regression line and annotations
day_year_plot <- ggplot(T.ovatum.phenology, aes(x = Year, y = DOY)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(
    title = "Year Collected vs. Day of Year Collected",
    x = "Year",
    y = "Day of Year"
  ) +
  theme_minimal() +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$DOY) - 0.1 * diff(range(T.ovatum.phenology$DOY)),
    label = paste("R^2 =", round(summary(lm(DOY ~ Year, data = T.ovatum.phenology))$r.squared, 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$DOY) - 0.15 * diff(range(T.ovatum.phenology$DOY)),
    label = paste("p =", round(summary(lm(DOY ~ Year, data = T.ovatum.phenology))$coef[2, 4], 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$DOY) - 0.2 * diff(range(T.ovatum.phenology$DOY)),
    label = paste("y =", round(coef(lm(DOY ~ Year, data = T.ovatum.phenology))[1], 2), "+", 
                  round(coef(lm(DOY ~ Year, data = T.ovatum.phenology))[2], 2), "x")
  )

# Scatterplot of Collection Date vs. Elevation with linear regression line and annotations
date_elevation_plot <- ggplot(T.ovatum.phenology, aes(x = DOY, y = Elevation_m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(
    title = "Elevation vs. Collection Date",
    x = "Collection Date",
    y = "Elevation (m)"
  ) +
  theme_minimal() +
  annotate(
    "text", x = min(T.ovatum.phenology$DOY) + 0.1 * diff(range(T.ovatum.phenology$DOY)),
    y = max(T.ovatum.phenology$Elevation_m) - 0.1 * diff(range(T.ovatum.phenology$Elevation_m)),
    label = paste("R^2 =", round(summary(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))$r.squared, 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$DOY) + 0.1 * diff(range(T.ovatum.phenology$DOY)),
    y = max(T.ovatum.phenology$Elevation_m) - 0.15 * diff(range(T.ovatum.phenology$Elevation_m)),
    label = paste("p =", round(summary(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))$coef[2, 4], 20))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$DOY) + 0.1 * diff(range(T.ovatum.phenology$DOY)),
    y = max(T.ovatum.phenology$Elevation_m) - 0.2 * diff(range(T.ovatum.phenology$Elevation_m)),
    label = paste("y =", round(coef(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))[1], 2), "+", 
                  round(coef(lm(Elevation_m ~ DOY, data = T.ovatum.phenology))[2], 2), "x")
  )

# Scatterplot of Elevation vs. Year with linear regression line and annotations
elevation_year_plot <- ggplot(T.ovatum.phenology, aes(x = Year, y = Elevation_m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(
    title = "Elevation vs. Year Collected",
    y = "Elevation (m)",
    x = "Year Collected"
  ) +
  theme_minimal() +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$Elevation_m) - 0.1 * diff(range(T.ovatum.phenology$Elevation_m)),
    label = paste("R^2 =", round(summary(lm(Elevation_m ~ Year, data = T.ovatum.phenology))$r.squared, 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$Elevation_m) - 0.15 * diff(range(T.ovatum.phenology$Elevation_m)),
    label = paste("p =", round(summary(lm(Elevation_m ~ Year, data = T.ovatum.phenology))$coef[2, 4], 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$Elevation_m) - 0.2 * diff(range(T.ovatum.phenology$Elevation_m)),
    label = paste("y =", round(coef(lm(Elevation_m ~ Year, data = T.ovatum.phenology))[1], 2), "+", 
                  round(coef(lm(Elevation_m ~ Year, data = T.ovatum.phenology))[2], 2), "x")
  )

# Scatterplot of Mean Spring Temperature vs. Year with linear regression line and annotations
spring_temp_year_plot <- ggplot(T.ovatum.phenology, aes(x = Year, y = Tave_Spring)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(
    title = "Mean Spring Temperature vs. Year",
    y = "Mean Spring Temperature",
    x = "Year"
  ) +
  theme_minimal() +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$Tave_Spring) - 0.1 * diff(range(T.ovatum.phenology$Tave_Spring)),
    label = paste("R^2 =", round(summary(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))$r.squared, 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$Tave_Spring) - 0.15 * diff(range(T.ovatum.phenology$Tave_Spring)),
    label = paste("p =", round(summary(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))$coef[2, 4], 5))
  ) +
  annotate(
    "text", x = min(T.ovatum.phenology$Year) + 0.1 * diff(range(T.ovatum.phenology$Year)),
    y = max(T.ovatum.phenology$Tave_Spring) - 0.2 * diff(range(T.ovatum.phenology$Tave_Spring)),
    label = paste("y =", round(coef(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))[1], 2), "+", 
                  round(coef(lm(Tave_Spring ~ Year, data = T.ovatum.phenology))[2], 2), "x")
  )

# Save scatterplots
ggsave("figures/day_year_scatterplot.png", plot = day_year_plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/date_elevation_scatterplot.png", plot = date_elevation_plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/elevation_year_scatterplot.png", plot = elevation_year_plot, bg = "white", width = 10, height = 6, units = "in")
ggsave("figures/spring_temp_year_scatterplot.png", plot = spring_temp_year_plot, bg = "white", width = 10, height = 6, units = "in")



# Temperature Sensitivity ####
T.ovatum.flowering <- T.ovatum.phenology %>%
  filter(Phenology == "Flowering")

write.csv(T.ovatum.flowering, "data/processed/t.ovatum.csv")

# Fit linear regression model with reversed relationship
sensitivty_model <- lm(DOY ~ Spring_T_Anomaly, data = T.ovatum.flowering)

# Get coefficient, p-value, and R-squared value
coef <- coef(sensitivty_model)[2]
p_value <- summary(sensitivty_model)$coefficients[2, 4]
r_squared <- summary(sensitivty_model)$r.squared

# Create plot with reversed relationship
spring_temp_anomaly_doy_plot <- ggplot(T.ovatum.flowering, aes(x = Spring_T_Anomaly, y = DOY)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  annotate("text", x = min(T.ovatum.flowering$Spring_T_Anomaly) + 1, y = max(T.ovatum.flowering$DOY) - 15, 
           label = paste("Slope =", round(coef, 2), "\nP-value =", round(p_value, 3), "\nR-squared =", round(r_squared, 3))) +  # Add annotation for slope, p-value, and R-squared
  labs(
    title = "T. ovatum Sensitivity: Day of Year Flowering Plants Collected vs. Spring Temperature Anomaly",
    x = "Spring Temperature Anomaly",
    y = "Day of Year Flowering Plants Collected"
  ) +
  theme_minimal()

# Save the plot with annotations
ggsave("figures/spring_temp_anomaly_doy_plot.png", plot = spring_temp_anomaly_doy_plot, bg = "white", width = 10, height = 6, units = "in")


# Quick Map Work ####
# Install and load the necessary packages
library(leaflet)
library(mapview)

# Create a leaflet map
map <- leaflet(data = T.ovatum.phenology) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~paste("Longitude:", Longitude, "<br>", "Latitude:", Latitude))

# Save the map as a PNG file
mapshot(map, file = "figures/map_output.png", width = 800, height = 600)

# Creating a Table for Means ####
library(kableExtra)
library(webshot)

# Calculate means
means_df <- T.ovatum.phenology %>%
  summarise(
    Sensitivity = coef,
    Mean_DOY = mean(DOY),
    Mean_Year = mean(Year),
    Mean_Elevation = mean(Elevation_m),
    Mean_Latitude = mean(Latitude),
    Mean_Longitude = mean(Longitude)
  )

means_df <- t(means_df)

# Create a table with kableExtra
table <- kable(means_df, format = "html", caption = "Means of Collection Data") %>%
  kable_styling(full_width = TRUE, bootstrap_options = "striped", font_size = 14) %>%
  column_spec(1:2, background = "white")%>%
  column_spec(1, width = "40%")

# Save the table as an HTML file
html_file <- "table_output.html"
writeLines(as.character(table), html_file)

# Save the table as PNG
webshot(html_file, file = "figures/table_output.png", delay = 2, cliprect = "viewport")
