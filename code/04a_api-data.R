# Get the latest forecast file
(forecast_files <- list.files("/mnt/forecasts/prediction-2025/forecast", full.names = T) %>% str_subset("forecast_draws_"))
(forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))])

forecast <- readRDS(forecast_files)

# Adjust Order to size of party
ordered_party <- names(sort(-apply(forecast,2,median)))
forecast <- forecast[, ordered_party]

# Save newest draws to api folder
message("Saving the draws for API.")
saveRDS(as.data.frame(forecast), file = "api/forecast_draws.rds")


# Define names
party_names <- data.frame("full_name" = c("CDU/CSU", "SPD",  "Grüne", "FDP", "AfD" ,"Linke", "BSW", "Andere"),
                          "full_name_eng" = c("CDU/CSU", "SPD",  "Greens", "FDP", "AfD" ,"Left", "BSW", "Other"),
                          "short_name" =  c("cdu","spd","gru","fdp","afd","lin","bsw","oth"))
party_colors <- c(
  "CDU/CSU" = "#000000",
  "SPD" = "#DD0000",
  "Grüne" = "#4C9A2A",
  "FDP" = "#FFCC00",
  "AfD" = "#009EE0",
  "Linke" = "purple",
  "BSW" = "#FF6A13"
)


# Create Forcast CI dataframe
df_forecast <- data.frame(y = apply(forecast, 2,  mean),
                          ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
                          ci95 = t(apply(forecast,2, function(x) quantile(x, c(0.025, 0.975)))))
df_forecast <- round(df_forecast*100, 1)
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")

df_forecast$name <- party_names$full_name[match(rownames(df_forecast),party_names$short_name)]
df_forecast$name_eng <- party_names$full_name_eng[match(rownames(df_forecast),party_names$short_name)]

# Drop Andere
df_forecast <- filter(df_forecast, name != "Andere")
df_forecast$color <- party_colors[df_forecast$name]

df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0, 6, 1)



# Output the forecast data for API
message("Saving the forecast for API.")
saveRDS(df_forecast, file = "output/forecasts/forecast_api.rds")
file.copy("output/forecasts/forecast_api.rds", "api/forecast_api.rds", overwrite = T)


# Add timestamp
last_updated <- Sys.time()
saveRDS(last_updated, file = "api/last_updated.rds")
