### ----------------------------------------------------------
### Create District-Level Election Forecast Visualizations
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Load District Forecast Data --------------------------

# Load district forecast from most recent cutoff date
districts <- readRDS(str_c("output/prediction_data_districts.rds"))

### 2. Define Color Scheme ---------------------------------

# Custom color scheme for political parties
primary_color <- list(
  "afd" = "#0489DB",  # AfD Blue
  "spd" = "#E3000F",  # SPD Red
  "cdu" = "#000000",  # CDU Black
  "fdp" = "#FFEF00",  # FDP Yellow
  "lin" = "#C13197",  # Left Purple
  "gru" = "#1AA037",  # Green
  "bsw" = "#8037DE",  # BSW Purple
  "oth" = "grey"      # Others Grey
)

### 3. Process Geographic Data -----------------------------

# Set random seed for reproducibility
set.seed(123)

# Filter for winning districts
gdf <- districts %>% 
  filter(winner == 1)

# Load and process shapefile data
wahlkreise_sf <- st_read("data/btw25_geometrie_wahlkreise_shp/btw25_geometrie_wahlkreise_shp.shp")
gdf_sf <- merge(wahlkreise_sf, gdf, by.x = "WKR_NR", by.y = "wkr")

### 4. Create Party Labels --------------------------------

# Compute districts won per party and create labels
party_labels <- gdf %>%
  group_by(party) %>%
  dplyr::summarize(won_districts = sum(winner, na.rm = TRUE)) %>%
  mutate(
    label = paste0(
      party %>% str_replace_all(c(
        "afd" = "AfD",
        "cdu" = "CDU/CSU",
        "bsw" = "BSW",
        "gru" = "Grüne",
        "spd" = "SPD",
        "lin" = "Linke"
      )),
      " (",
      won_districts,
      ")"
    )
  ) %>%
  pull(label, name = party)

### 5. Create District Map Plot ---------------------------

district_map <- ggplot(gdf_sf) +
  # Add district polygons
  geom_sf(
    aes(
      fill = factor(party),
      alpha = probability
    )
  ) +
  # Set color scheme
  scale_fill_manual(
    values = primary_color,
    name = "",
    labels = party_labels
  ) +
  # Customize theme
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  # Remove alpha legend
  guides(alpha = "none") +
  # Prevent extra space around plot
  coord_sf(expand = FALSE)

### 6. Save Plot -----------------------------------------

# Save as PNG
ggsave(
  filename = "output/plots/figure_forecast_districts.png",
  plot = district_map,
  device = "png",
  dpi = 300,
  height = 5,
  width = 5,
  bg = "white"
)

