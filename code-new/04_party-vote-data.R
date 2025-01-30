### ----------------------------------------------------------
### Process Party Vote Data for Visualization
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Load Latest Forecast Data ------------------------------

# Get the most recent forecast file
forecast_files <- list.files("output", full.names = TRUE) %>% 
  str_subset("forecast_draws_")

# Select the latest forecast based on date in filename
latest_date <- max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))
forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == latest_date]
forecast <- readRDS(forecast_files)

### 2. Define Party Information ------------------------------

# Order parties by size (based on median forecast)
ordered_party <- names(sort(-apply(forecast, 2, median)))
forecast <- forecast[, ordered_party]

# Define party names and mappings
party_names <- data.frame(
  full_name = c("CDU/CSU", "SPD", "Grüne", "FDP", "AfD", "Linke", "BSW", "Andere"),
  full_name_eng = c("CDU/CSU", "SPD", "Greens", "FDP", "AfD", "Left", "BSW", "Other"),
  short_name = c("cdu", "spd", "gru", "fdp", "afd", "lin", "bsw", "oth")
)

# Define party colors
party_colors <- c(
  "CDU/CSU" = "#000000",
  "SPD" = "#DD0000",
  "Grüne" = "#4C9A2A",
  "FDP" = "#FFCC00",
  "AfD" = "#009EE0",
  "Linke" = "purple",
  "BSW" = "#FF6A13"
)

### 3. Process Forecast Data --------------------------------

# Calculate forecast statistics
forecast_party_vote <- data.frame(
  # Calculate mean and confidence intervals
  y = apply(forecast, 2, mean),
  ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
  ci95 = t(apply(forecast, 2, function(x) quantile(x, c(0.025, 0.975))))
)

# Convert to percentages and round
forecast_party_vote <- round(forecast_party_vote * 100, 1)
colnames(forecast_party_vote) <- c("value", "low", "high", "low95", "high95")

# Add party names and colors
forecast_party_vote$name <- party_names$full_name[match(rownames(forecast_party_vote), 
                                                      party_names$short_name)]
forecast_party_vote$name_eng <- party_names$full_name_eng[match(rownames(forecast_party_vote), 
                                                              party_names$short_name)]

# Remove "Andere" (Others) category and add colors
forecast_party_vote <- filter(forecast_party_vote, name != "Andere")
forecast_party_vote$color <- party_colors[forecast_party_vote$name]

# Add positioning for visualization
forecast_party_vote$y <- forecast_party_vote$value
forecast_party_vote$x <- seq(0, 6, 1)

### 4. Save Results ----------------------------------------

saveRDS(forecast_party_vote, file = "output/forecast_party_vote.rds")

