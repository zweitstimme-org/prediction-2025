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

