##### Forecast Trend

message("Generating trend plot.")

(forecast_files <- list.files("/mnt/forecasts/prediction-2025/forecast", full.names = T) %>% str_subset("forecast_draws_"))

forecast_trend <- data.frame()

for (i in 1:length(forecast_files)) {
  forecast <- forecast_files[i] %>% readRDS()
  
  # Adjust Order to size of party
  ordered_party <- names(sort(-apply(forecast,2,median)))
  forecast <- forecast[, ordered_party]
  
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
  
  df_forecast$date <- forecast_files[i] %>% str_extract(".{10}(?=\\.rds)") %>% ymd
  
  forecast_trend <- bind_rows(forecast_trend, df_forecast)
}



# Do something like above, but as a line plot with x being date
# ggplot(forecast_trend, aes(x = date, y = y, color = name, fill = name)) +
#   geom_step() +
#   geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, color = NA) +
#   scale_color_manual(values = df_forecast$color, breaks = df_forecast$name) +
#   scale_fill_manual(values = df_forecast$color, breaks = df_forecast$name) +
#   theme_minimal() +
#   theme(
#     # legend.position = "none",
#     plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5),
#     axis.text.x = element_text(size = 8, face = "bold", color = "black"),
#     axis.ticks.y = element_blank(),
#     axis.line.y = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     plot.margin = margin(10, 10, 10, 10)
#   ) +
#   labs(
#     x = NULL,
#     y = "%"
#   )


# Generate a sequence of weekly dates from the minimum to maximum date
weekly_ticks <- seq(
  from = min(forecast_trend$date), 
  to = max(forecast_trend$date), 
  by = "7 days"
)

# Ensure the most recent date is included, even if it's not part of the sequence
if (max(forecast_trend$date) != weekly_ticks[length(weekly_ticks)]) {
  weekly_ticks <- c(weekly_ticks, max(forecast_trend$date))
}

# Format tick text
weekly_tick_text <- format(weekly_ticks, "%d.%m.")


# Plotly plot with weekly ticks
plotly_plot <- plot_ly(
  forecast_trend,
  x = ~date,
  y = ~y,
  color = ~name,
  fill = ~name,
  type = 'scatter',
  mode = 'lines',
  line = list(shape = "hv"),
  fillcolor = ~color,
  colors = setNames(df_forecast$color, df_forecast$name),  # Ensure correct color mapping
  showlegend = F,
  hovertemplate = "%{y}%<extra></extra>"  # Display percentage value on hover
) %>% 
  layout(
    xaxis = list(
      title = "",
      tickformat = "%d/%m",  # Day and month in "dd/mm" format
      tickvals = weekly_ticks,  # Use filtered weekly tick values
      ticktext = weekly_tick_text  # Corresponding formatted tick text
    ),
    yaxis = list(title = "%"),
    margin = list(l = 50, r = 50, b = 50, t = 50),
    legend = list(orientation = "h", x = 0.5, y = -0.1)
  ) %>% 
  plotly::config(displayModeBar = F)
    # Disable the display bar


# %>%  
#   config(displayModeBar = F)

# plotly_plot <- plot_ly(
#   forecast_trend,
#   x = ~date,
#   y = ~y,
#   type = 'scatter',
#   mode = 'lines',
#   line = list(shape = "hv"),
#   color = ~name,  # Map only to 'name'
#   colors = setNames(df_forecast$color, df_forecast$name),  # Map custom colors to names
#   showlegend = TRUE,  # Enable legend
#   hovertemplate = "%{y}%<extra></extra>"  # Display percentage value on hover
# ) %>% 
#   layout(
#     xaxis = list(
#       title = "",
#       tickformat = "%d/%m",  # Day and month in "dd/mm" format
#       tickvals = forecast_trend$date,  # Ensure tick values are set
#       ticktext = format(forecast_trend$date, "%d.%m.")  # Format dates as day/month
#     ),
#     yaxis = list(title = "%"),
#     margin = list(l = 50, r = 50, b = 50, t = 50),
#     legend = list(
#       orientation = "h",  # Horizontal orientation
#       x = 0.5,            # Center the legend
#       y = -0.3,           # Place it below the plot
#       xanchor = "center", # Align the legend horizontally
#       yanchor = "top"     # Align the legend vertically
#     )
#   ) %>% 
#   plotly::config(displayModeBar = FALSE)  # Disable display mode bar

plotly_plot


saveWidget(plotly_plot, "api/forecast_trend.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()



# Run every time