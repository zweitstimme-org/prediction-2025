
message("Creating plotly.")

# Load data
df_forecast <- readRDS("output/forecasts/forecast_api.rds")

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



df_forecast$name <- factor(df_forecast$name, levels = df_forecast$name)

plotly_plot <- plot_ly(
  data = df_forecast, 
  x = ~name, 
  y = ~value,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = ~color, size = 18, line = list(width = 4, color = 'white')),
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
  # displayModeBar = FALSE,  # Disable the mode bar
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
    opacity = 0.7,
    text = NA,
    hoverinfo = 'text'  
  ) %>%  
  config(displayModeBar = FALSE)



# plotly_plot

saveWidget(plotly_plot, "api/forecast.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()


