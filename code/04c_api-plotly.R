


message("Creating plotly.")

font_family <- "'Segoe UI', -apple-system, sans-serif"

# Load data
df_forecast <- readRDS("output/forecasts/forecast_api.rds")

# Add value labels with one decimal
df_forecast <- df_forecast %>%
  mutate(
    value_label = round(value, 1),
    value_label = ifelse(value_label %% 1 == 0, paste0(value_label, ".0"), as.character(value_label))
  )

# Get the current date in German
current_date <-  Sys.Date() # "2024-12-08" #

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
  marker = list(color = ~color, size = 17, line = list(width = 4, color = 'white')),
  hoverinfo = 'text',  
  text = ~paste0(round(value, 1), '%', " (", round(low, 1), "% - ", round(high, 1), "%)") %>% str_replace_all("\\.", ",")
) %>%

layout(
  # title = str_c("Wahlprognose von zweitstimme.org\n", "vom ", formatted_date),
  xaxis = list(
    title = "",
    fixedrange = TRUE,
    tickfont = list(family = font_family),  # Add font for tick labels
    tickangle = 0,
    showline = TRUE,     # Show x-axis line
    linecolor = "black", # Color of x-axis line
    linewidth = 1
  ),
  yaxis = list(
    title = "Stimmanteil in %",
    fixedrange = TRUE,
    tickfont = list(family = font_family)  # Add font for tick labels
  ),
  showlegend = FALSE,
  hovermode = 'closest',
  bargap = 0.6,
  font = list(family = font_family,
              size = 16),  # Global font family
  shapes = list(
    list(
      type = "line",
      x0 = -.5,
      x1 = 7,
      y0 = 5,
      y1 = 5,
      line = list(
        color = "grey",
        width = 2,
        dash = "dash"
      ),
      layer = "below"
    )
  ),
  annotations = list(
    list(
      x = 0.02,
      y = 7,
      xref = "paper",
      yref = "y",
      text = "5%-Hürde",
      showarrow = FALSE,
      font = list(
        family = font_family,  # Add font for annotation
        size = 16,
        color = "grey"
      ),
      align = "left"
    )
  )
) %>%
  # margin = list(t = 80)  # Increase the top margin (default is usually 20)
 
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
  plotly::config(displayModeBar = FALSE)






plotly_plot

# plotly_plot

saveWidget(plotly_plot, "api/forecast.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()


# Create mobile version
plotly_plot_mobile <- plot_ly(
  data = df_forecast, 
  x = ~name, 
  y = ~value,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = ~color, size = 8, line = list(width = 3, color = 'white')), # Smaller dots
  hoverinfo = 'text',  
  text = ~paste0(round(value, 1), '%', " (", round(low, 1), "% - ", round(high, 1), "%)") %>% str_replace_all("\\.", ",")
) %>%
  
  layout(
    xaxis = list(
      title = "",
      fixedrange = TRUE,
      tickfont = list(family = font_family, size = 14),  # Smaller font
      tickangle = -45,  # Vertical labels
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    yaxis = list(
      title = "Anteil in %",
      fixedrange = TRUE,
      tickfont = list(family = font_family, size = 14)  # Smaller font
    ),
    showlegend = FALSE,
    hovermode = 'closest',
    bargap = 0.6,
    font = list(family = font_family,
                size = 14),  # Smaller global font
    shapes = list(
      list(
        type = "line",
        x0 = -.5,
        x1 = 7,
        y0 = 5,
        y1 = 5,
        line = list(
          color = "grey",
          width = 2,
          dash = "dash"
        ),
        layer = "below"
      )
    ),
    annotations = list(
      list(
        x = 0.02,
        y = 10,
        xref = "paper",
        yref = "y",
        text = "5%-Hürde",
        showarrow = FALSE,
        font = list(
          family = font_family,
          size = 14,  # Smaller font
          color = "grey"
        ),
        align = "left"
      )
    ),
    margin = list(b = 0)  # Increased bottom margin for rotated labels
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
  plotly::config(displayModeBar = FALSE)

plotly_plot_mobile

# Save mobile version
saveWidget(plotly_plot_mobile, "api/forecast_mobile.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()

