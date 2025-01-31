### ----------------------------------------------------------
### Create Party Vote Share Forecast Visualization
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Load Data --------------------------------------------

# Load forecast data
forecast_party_vote <- readRDS("output/forecast_party_vote.rds")

### 2. Create Main Forecast Plot ----------------------------

# Create visualization of party vote shares
ggplot_forecast <- forecast_party_vote %>% 
  mutate(
    # Format value labels
    value_label = round(value, 1),
    value_label = ifelse(
      value_label %% 1 == 0, 
      str_c(value_label, ".0"), 
      value_label
    )
  ) %>% 
  ggplot(aes(x = reorder(name, -value), 
             y = value, 
             color = name, 
             fill = name)) +
  # Add 5% threshold line
  geom_hline(
    yintercept = 5, 
    linetype = "dotted", 
    linewidth = .5, 
    color = "black"
  ) +
  # Add confidence intervals
  geom_linerange(
    aes(ymin = low95, ymax = high95), 
    linewidth = 10, 
    alpha = 0.3, 
    position = position_dodge(width = .5)
  ) +
  geom_linerange(
    aes(ymin = low, ymax = high), 
    linewidth = 10, 
    alpha = 0.7, 
    position = position_dodge(width = .5)
  ) + 
  # Add points
  geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
  geom_point(size = 2, fill = "white", shape = 21) +
  # Add value labels
  geom_text(
    aes(label = value_label, y = value), 
    hjust = 2, 
    size = 3, 
    color = "black"
  ) +
  # Set colors
  scale_color_manual(
    values = forecast_party_vote$color, 
    breaks = forecast_party_vote$name
  ) +
  scale_fill_manual(
    values = forecast_party_vote$color, 
    breaks = forecast_party_vote$name
  ) +
  # Customize theme
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
  # Set labels
  labs(x = NULL, y = "%")

### 3. Save Plots ------------------------------------------

# Create output directory if it doesn't exist
if (!dir.exists("output/plots")) {
  dir.create("output/plots")
}

# Save as PDF
ggsave(
  filename = "output/plots/figure_forecast_party_vote.pdf",
  plot = ggplot_forecast,
  height = 6,
  width = 6 * 1.5
)

# Save as PNG
ggsave(
  filename = "output/plots/figure_forecast_party_vote.png",
  plot = ggplot_forecast,
  device = "png",
  dpi = 300,
  height = 5,
  width = 5 * 1.5,
  bg = "white"
)


message("Interactive plots.")


font_family <- "'Segoe UI', -apple-system, sans-serif"

# Add value labels with one decimal
forecast_party_vote <- forecast_party_vote %>%
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



forecast_party_vote$name <- factor(forecast_party_vote$name, levels = forecast_party_vote$name)


plotly_plot <- plot_ly(
  data = forecast_party_vote, 
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
    x = forecast_party_vote$name,
    y = forecast_party_vote$high - forecast_party_vote$low,  
    type = 'bar',
    name = 'Konfidenzintervall',
    marker = list(
      color = forecast_party_vote$color,  
      line = list(width = 0)
    ),
    base = forecast_party_vote$low,  
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
  data = forecast_party_vote, 
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
    x = forecast_party_vote$name,
    y = forecast_party_vote$high - forecast_party_vote$low,  
    type = 'bar',
    name = 'Konfidenzintervall',
    marker = list(
      color = forecast_party_vote$color,  
      line = list(width = 0)
    ),
    base = forecast_party_vote$low,  
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


## ADD ZWEITSTIMME PLOTS




