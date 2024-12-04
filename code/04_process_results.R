
### ----------------------------------------------------------
### Election polling forecasting 
### 
### 
### Lukas Stoetzer & Marcel Neunhoeffer
### 
### 
###
### ----------------------------------------------------------

source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions


# Figure

# potential_file <- list.files("output/draws/", full.names = T) %>% str_subset("res_brw_2025")

# Get the latestet results if they aren't loaded yet
# if(!exists(results)) results <- readRDS(file="output/draws/res_brw_2025_2024-11-20.rds")

# Diagnostics
# check_hmc_diagnostics(results) # do not look all to good


# Load the latest Forecasts
# str_c("/mnt/forecasts/zweitstimme/zweitstimme_output_", Sys.Date(),".rds")

# Get the latest forecast file


(forecast_files <- list.files("/mnt/forecasts/prediction-2025/forecast", full.names = T) %>% str_subset("forecast_draws_"))
(forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))])

forecast <- readRDS(forecast_files)

# Save newest draws to api folder
saveRDS(as.data.frame(forecast), file = "api/forecast_draws.rds")


# Adjust Order to size of party
ordered_party <- names(sort(-apply(forecast,2,median)))
forecast <- forecast[, ordered_party]

# Define names
party_names <- data.frame("full_name" = c("CDU/CSU", "SPD",  "Grüne", "FDP", "AfD" ,"Linke", "BSW", "Andere"),
                          "full_name_eng" = c("CDU/CSU", "SPD",  "Grüne", "FDP", "AfD" ,"Linke", "BSW", "Andere"),
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
rownames(df_forecast) <- party_names$full_name[match(rownames(df_forecast),party_names$short_name)]



# Drop Andere
df_forecast <- filter(df_forecast, name != "Andere")

df_forecast$color <- party_colors[df_forecast$name]

df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0, 6, 1)



# Output the forecast data for API
saveRDS(df_forecast, file = "output/forecasts/forecast_api.rds")
file.copy("output/forecasts/forecast_api.rds", "api/forecast_api.rds", overwrite = T)



# # Plot
# pdf("output/fig/figure_forecast.pdf", width = 12, height = 8)
# par(mar=c(5,5,0,0)+.1)
# 
# x_val <- 1:7
# plot(x = x_val,
#      y = df_forecast$value, col = "white", 
#      type = "n", bty = "n", 
#      ylim = c(0,45), xlim = c(0,7.5),
#      xlab = "",
#      ylab = "",
#      yaxt = "n",
#      xaxt = "n",
#      cex.axis = 1.2)
# abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
# abline(h = c(0), lty = "solid", col = "lightgrey")
# abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
# 
# #segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low95, y1 = df_forecast$high95, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.6), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low, y1 = df_forecast$high, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.99), lend = 1)
# 
# points(x = x_val,
#        y = df_forecast$value, col = "white", lwd = 2)
# text(y = df_forecast$value, x = x_val-0.35, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
# #text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = x_val+0.35, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
# axis(1, at = x_val, labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
# axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
# mtext("Vote Share (%)", side=2, line=3, cex=1.2)
# 
# 
# 
# dev.off()
# 
# png("output/fig/figure_forecast.png", width = 1000, height = 600)
# par(mar=c(5,5,0,0)+.1)
# x_val <- 1:7
# plot(x = x_val,
#      y = df_forecast$value, col = "white", 
#      type = "n", bty = "n", 
#      ylim = c(0,45), xlim = c(0,7.5),
#      xlab = "",
#      ylab = "",
#      yaxt = "n",
#      xaxt = "n",
#      cex.axis = 1.2)
# abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
# abline(h = c(0), lty = "solid", col = "lightgrey")
# abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
# 
# #segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low95, y1 = df_forecast$high95, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.6), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low, y1 = df_forecast$high, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.99), lend = 1)
# 
# points(x = x_val,
#        y = df_forecast$value, col = "white", lwd = 2)
# text(y = df_forecast$value, x = x_val-0.35, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
# #text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = x_val+0.35, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
# axis(1, at = x_val, labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
# axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
# mtext("Vote Share (%)", side=2, line=3, cex=1.2)
# dev.off()

df_forecast %>% mutate(value_label = round(value, 1),
                       value_label = ifelse(value_label %% 1 == 0, str_c(value_label, ".0"), value_label)) %>% 
  ggplot(aes(x = reorder(name, -value), y = value, color = name, fill = name)) +
  geom_hline(yintercept = 5, linetype = "dotted", size = .5, color = "black") +
  geom_linerange(aes(ymin = low95, ymax = high95), linewidth = 10, alpha = 0.3, position=position_dodge(width=.5)) + # , col = party_colors[all_fcst$party_name]) +
  geom_linerange(aes(ymin = low, ymax = high), linewidth = 10, alpha = 0.7, position=position_dodge(width=.5)) + # , col = party_colors[all_fcst$party_name]) +
  geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
  geom_point(size = 2, fill = "white", shape = 21) +
  scale_color_manual(values = df_forecast$color, breaks = df_forecast$name) +
  scale_fill_manual(values = df_forecast$color, breaks = df_forecast$name) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 8, face = "bold", color = "black"),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    x = NULL,
    y = "%"
  ) +
  geom_text(aes(label = value_label, y = value), hjust = 2, size = 3, color = "black") 

ggsave(filename = "output/fig/figure_forecast.pdf", height = 6, width = 6*1.5)
ggsave(filename = "output/fig/figure_forecast.png", device = "png", dpi = 300,  height = 5, width = 5*1.5, bg = "white")

# Copy files into the api folder
file.copy("output/fig/figure_forecast.png", "api/figure_forecast.png", overwrite = T)
file.copy("output/fig/figure_forecast.pdf", "api/figure_forecast.pdf", overwrite = T)


# Add timestamp
last_updated <- Sys.time()
saveRDS(last_updated, file = "api/last_updated.rds")



library(highcharter)
library(htmlwidgets)


# Add value labels with one decimal
df_forecast <- df_forecast %>%
  mutate(
    value_label = round(value, 1),
    value_label = ifelse(value_label %% 1 == 0, paste0(value_label, ".0"), as.character(value_label))
  )

# Get the current date in German
current_date <- Sys.Date()


# Manually translate to German
months <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")

# Format date manually
formatted_date <- paste0(format(as.POSIXlt(current_date), "%d"), ". ", 
                         months[as.POSIXlt(current_date)$mon + 1], " ",
                         format(as.POSIXlt(current_date), "%Y"))


# 
# # Highchart Visualization
# highchart_plot <- highchart() %>%
#   hc_chart(type = "column") %>%
#   hc_title(text = "Wahlprognose von zweitstimme.org") %>%
#   hc_subtitle(text = paste0("Letzte Prognose vom ", formatted_date)) %>%
#   hc_xAxis(
#     categories = df_forecast$name,
#     title = list(text = NULL)
#   ) %>%
#   hc_yAxis(
#     title = list(text = "%"),
#     plotLines = list(list(
#       value = 5,
#       color = "black",
#       width = 1,
#       dashStyle = "Dot",
#       label = list(text = "5%-Hürde", align = "left", style = list(color = "black"))
#     ))
#   ) %>%
#   
#   hc_add_series(
#     name = "Konfidenzintervall",
#     type = "columnrange",
#     data = transpose(list(df_forecast$low, df_forecast$high)),
#     colorByPoint = TRUE, # Assign different colors per bar
#     colors = df_forecast$color, # Use colors from the dataframe
#     borderWidth = 0
#   ) %>%
#   # Mean Points
#   hc_add_series(
#     name = "Vorhersage",
#     type = "scatter",
#     data = df_forecast$value,
#     color = df_forecast$color,
#     marker = list(
#       radius = 6, # Adjust size of point
#       fillColor = "white", # White background
#       lineWidth = 2,
#       lineColor = df_forecast$color # Match border with series color
#     ),
#     tooltip = list(
#       pointFormat = "<b>{series.name}:</b> {point.y}%<br/>"
#     )
#   ) %>%
#   # Annotations for Values
#   hc_plotOptions(
#     columnrange = list(
#       dataLabels = list(enabled = FALSE) # Turn off static labels
#     )
#   ) %>%
#   hc_colors(df_forecast$color) %>%
#   hc_legend(enabled = FALSE) %>%
#   hc_tooltip(
#     shared = TRUE,
#     useHTML = TRUE, # Allows for more customizable content
#     headerFormat = "<b>{point.x}</b><br/>", # Show the category name
#     pointFormat = "<b>{series.name}:</b> {point.low}% - {point.high}%<br/>"
#   ) %>%
#   hc_add_theme(hc_theme_flat())
# 
# 
# saveWidget(highchart_plot, "api/forecast.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org")
# 


library(plotly)
library(htmlwidgets)

df_forecast$name <- factor(df_forecast$name, levels = df_forecast$name)

plotly_plot <- plot_ly(
  data = df_forecast, 
  x = ~name, 
  y = ~value,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = ~color, size = 12, line = list(width = 2, color = 'white')),
  hoverinfo = 'text',  
  text = ~paste0('Vorhersage: ', round(value, 1), '%', "\nKonfidenzintervall: ", round(low, 1), "% - ", round(high, 1), "%") %>% str_replace_all("\\.", ",")
) %>% layout(
  title = str_c("Wahlprognose von zweitstimme.org\n", "vom ", formatted_date),
  xaxis = list(title = "",
               fixedrange = TRUE),
  yaxis = list(title = "%",
               fixedrange = TRUE),
  showlegend = FALSE,
  hovermode = 'closest',
  displayModeBar = FALSE,  # Disable the mode bar
  bargap = 0.6,  # Reduce the gap between bars
  shapes = list(
    # Horizontal dashed line at 5%
    list(
      type = "line",
      x0 = -.5,
      x1 = 7,
      y0 = 5,
      y1 = 5,
      line = list(
        color = "grey",  # Color of the line
        width = 2,        # Line width
        dash = "dash"     # Dashed line
      ),
      layer = "below"  # This places the line below the bars (in the background)
    )
  ),
  annotations = list(
    # Label for the 5% line
    list(
      x = 0.02,  # Position the text on the left
      y = 5.5,  # Align it with the 5% line
      xref = "paper",  # Use "paper" to position in the plot area (relative coordinates)
      yref = "y",      # Refer to the y-axis for positioning
      text = "5% Hürde",  # The label text
      showarrow = FALSE,  # No arrow
      font = list(
        size = 12,
        color = "grey"
      ),
      align = "left"
    )
  )
) %>%
  add_trace(
    x = df_forecast$name,
    y = df_forecast$high - df_forecast$low,  
    type = 'bar',
    name = 'Konfidenzintervall',
    marker = list(
      color = df_forecast$color,  
      line = list(width = 0)
    ),
    base = df_forecast$low,  
    orientation = 'v',  
    opacity = 0.4,
    text = NA,
    hoverinfo = 'text'  
  ) %>%  config(displayModeBar = FALSE)
  


# plotly_plot

saveWidget(plotly_plot, "api/forecast.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org")




