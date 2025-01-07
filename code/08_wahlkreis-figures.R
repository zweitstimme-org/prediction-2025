message("Erststimme plot.")


districts <- readRDS("api/forecast_districts.rds")

# Create a new column for the type of the forecast

winners <- districts %>% filter(winner == 1)

library(pacman)
p_load(tidyverse, rio, sf, patchwork, httr, jsonlite, plotly)

districts <- readRDS("api/forecast_districts.rds")

# Create a new column for the type of the forecast

# parties <- c("SPD", "CDU", "Die Linke", "AfD", "BSW", "FDP", "Die Grünen")
# wks <- seq(1, 299)

set.seed(123)
# df_prognosen <- expand.grid(Party = unique(districts$partei), Wahlkreis = wks) %>%
#   mutate(mandate_probability = runif(n()))

# Load shapefile
wahlkreise_sf <- st_read("data/btw25_geometrie_wahlkreise_shp/btw25_geometrie_wahlkreise_shp.shp")

gdf <- wahlkreise_sf %>% left_join(districts %>% filter(winner == 1), by = c("WKR_NR" = "wkr")) 


# Custom color scheme
primary_color <- list(
  "afd" = "#0489DB",
  "spd" = "#E3000F",
  "cdu" = "#000000",
  "fdp" = "#FFEF00",
  "lin" = "#C13197",
  "gru" = "#1AA037",
  "bsw" = "#8037DE",
  "oth" = "grey"
)

# ggplot with custom colors applied

# Compute the number of districts won per party
party_labels <- gdf %>%
  group_by(party) %>%
  summarize(won_districts = sum(winner, na.rm = TRUE)) %>%
  mutate(label = paste0(party %>% str_replace_all(c("afd" = "AfD", "cdu" = "CDU/CSU", "bsw" = "BSW", "gru" = "Greens", "spd" = "SPD", "lin" = "Linke")), " (", won_districts, ")")) %>%
  pull(label, name = party)

# Plot with dynamic labels
ggplot(gdf) +
  geom_sf(aes(fill = factor(party), alpha = probability)) +
  scale_fill_manual(
    values = primary_color,
    name = "",
    labels = party_labels
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(alpha = "none") +
  coord_sf(expand = FALSE)  # Prevent extra space around the plot


ggsave(
  filename = "api/figure_forecast_districts.png",
  device = "png",
  dpi = 300,
  height = 5,  # Adjust to fit your map's aspect ratio
  width = 5,
  bg = "white"
)


# Assuming gdf has 'party', 'geometry' and 'probability' columns, and party is factorized properly
# Ensure gdf$party is a factor and it matches the primary_color keys
gdf$party <- factor(gdf$party)



# Define the primary color scheme
primary_color <- c(
  "afd" = "#0489DB",
  "spd" = "#E3000F",
  "cdu" = "#000000",
  "fdp" = "#FFEF00",
  "lin" = "#C13197",
  "gru" = "#1AA037",
  "bsw" = "#8037DE"
)

# # Check the 'party' column to ensure correct matching
# unique(gdf$party)  # Make sure these match the names in primary_color
# 
# # Create the ggplot map with custom colors
# ggplot_map <- ggplot(gdf) +
#   geom_sf(aes(fill = party)) +  # 'party' as the factor for fill
#   scale_fill_manual(
#     values = primary_color,  # Map party names to custom colors
#     name = "Party"  # Label for the legend
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "none",  # Hide the legend
#     plot.title = element_text(hjust = 0.5)  # Center-align the title
#   )
# 
# 
# 
# # Create a custom text column for hover info
# gdf$text_label <- paste(gdf$party %>% str_replace_all(c("afd" = "AfD", "cdu" = "CDU/CSU", "bsw" = "BSW", "gru" = "Grüne", "spd" = "SPD", "lin" = "Linke")), " (", gdf$probability, "%)\n", gdf$WKR_NR, " - ", gdf$WKR_NAME, sep = "")
# 
# # Create ggplot map
# ggplot_map <- ggplot(gdf) +
#   geom_sf(aes(fill = party, alpha = probability, text = text_label), color = NA, size = .01) +  # Include custom text for hover
#   scale_fill_manual(
#     values = primary_color,
#     name = "",
#     labels = party_labels
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5),
#     panel.grid = element_blank(),  # Remove gridlines
#     axis.ticks = element_blank(),  # Remove axis ticks
#     axis.text = element_blank()    # Remove axis labels
#   ) +
#   guides(alpha = "none") +
#   coord_sf(expand = FALSE)  # Prevent extra space around the plot
# 
# # Convert the ggplot object to plotly for interactivity
# plotly_map <- ggplotly(ggplot_map, tooltip = c("fill", "text")) %>%  # Only show the custom text
#   layout(
#     autosize = TRUE,
#     height = 200*2,  # Make the map taller
#     width = 200*2,   # Make the map narrower
#     margin = list(t = 0, b = 0, l = 0, r = 0),  # Remove margins
#     xaxis = list(
#       visible = FALSE,        # Hide x-axis (longitude)
#       showgrid = FALSE,       # Remove gridlines
#       zeroline = FALSE,       # Remove zero line
#       title = NULL,           # Remove axis title
#       showticklabels = FALSE, # Remove tick labels
#       tickvals = NULL,        # Ensure no ticks are drawn
#       showline = FALSE        # Hide the axis line
#     ),
#     yaxis = list(
#       visible = FALSE,        # Hide y-axis (latitude)
#       showgrid = FALSE,       # Remove gridlines
#       zeroline = FALSE,       # Remove zero line
#       title = NULL,           # Remove axis title
#       showticklabels = FALSE, # Remove tick labels
#       tickvals = NULL,        # Ensure no ticks are drawn
#       showline = FALSE        # Hide the axis line
#     ),
#     plot_bgcolor = "white",  # Set the background to white
#     paper_bgcolor = "white"  # Set the paper background to white
#   ) %>%  
#   plotly::config(displayModeBar = FALSE)   # %>% style(hoveron="fills")
# 
# # Show the plotly map
# plotly_map
# 
# # Save mobile version
# saveWidget(plotly_map, "api/map.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
#   suppressWarnings()



# Load the GeoJSON file
geojson_file <- "data/germany-25-wahlkreise.geojson"


# Now, try parsing
library(rjson)
data <- rjson::fromJSON(paste(geojson_text, collapse = ""))
data$features[[1]]

# Add an ID variable to each feature
for (i in seq_along(data$features)) {
  data$features[[i]]$properties$id <- data$features[[i]]$properties$WKR_NR
  data$features[[i]]$id <- data$features[[i]]$properties$WKR_NR
}

# Define the primary color scheme
primary_color <- c(
  "afd" = "#0489DB",
  "spd" = "#E3000F",
  "cdu" = "#000000",
  "fdp" = "#FFEF00",
  "lin" = "#C13197",
  "gru" = "#1AA037",
  "bsw" = "#8037DE"
)

gdf$WKR_NR
gdf$party
gdf$probability


# Assuming `data` is your GeoJSON data and `locations` and `z` are correctly defined
fig <- plot_ly() 

fig <- fig %>% add_trace(
  type = "choropleth",
  geojson = data,
  locations = 1:299,  # Adjust based on your data
  z = 1:299,          # Adjust based on your data
  colorscale = "Viridis",
  zmin = 0,
  zmax = 299,
  marker = list(line = list(width = 1), opacity = 0.5)
)

# Define layout to customize the map
fig <- fig %>% layout(
  geo = list(
    showcoastlines = FALSE,         # Hide coastlines
    coastlinecolor = "white",       # Set coastlines to white (or any other color)
    showland = FALSE,              # Hide land
    landcolor = "white",           # Set land color to white (or any other color)
    projection = list(type = "mercator"),  # Set the map projection (you can change this)
    center = list(lon = mean(c(9.708774, 10.5)), lat = mean(c(54.830533, 53.0))),  # Automatically center around your data
    fitbounds = "locations",       # Automatically fit the bounds of the data
    showcountries = FALSE,         # Hide country borders
    countrycolor = "white"         # Set country borders color to white
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0)  # Remove margins
)

# Show the map
fig


# Assuming your new dataframe looks like this
# df_party <- data.frame(WKR_NR = c(1, 2, 3), party = c('afd', 'spd', 'cdu'), probability = c(90, 75, 60))

# Define the primary color palette for the parties
primary_color <- c(
  "afd" = "#0489DB",
  "spd" = "#E3000F",
  "cdu" = "#000000",
  "fdp" = "#FFEF00",
  "lin" = "#C13197",
  "gru" = "#1AA037",
  "bsw" = "#8037DE"
)

# Map each party to a unique number
party_numbers <- c("afd" = 1, "spd" = 2, "cdu" = 3, "fdp" = 4, "lin" = 5, "gru" = 6, "bsw" = 7)

# Create a vector of numbers corresponding to each party in your data
z_values <- sapply(gdf$party, function(x) party_numbers[x])

# Create a custom colorscale
colorscale <- list(
  list(0, primary_color["afd"]),
  list(1/6, primary_color["spd"]),
  list(2/6, primary_color["cdu"]),
  list(3/6, primary_color["fdp"]),
  list(4/6, primary_color["lin"]),
  list(5/6, primary_color["gru"]),
  list(1, primary_color["bsw"])
)

# Create the Plotly map
fig <- plot_ly()

fig <- fig %>% add_trace(
  type = "choropleth",
  geojson = data,
  locations = gdf$WKR_NR,  # Use the WKR_NR from merged data
  z = z_values,  # Use the numeric vector for color mapping
  colorscale = colorscale,  # Use the custom colorscale
  marker = list(line = list(width = 1), opacity = 1)  # Adjust line width and opacity
)

# Layout adjustments to remove world borders and center the map
fig <- fig %>% layout(
  geo = list(
    showcoastlines = FALSE,  # Hide coastlines
    showland = FALSE,       # Hide land
    projection = list(type = "mercator"),
    center = list(lon = mean(c(9.708774, 10.5)), lat = mean(c(54.830533, 53.0))),  # Adjust based on your data
    fitbounds = "locations",  # Automatically fit bounds to your data
    showcountries = FALSE    # Hide country borders
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0)  # Remove margins
)

# Show the map
fig


# Create the Plotly map
fig <- plot_ly()

# Loop through each unique party and add traces
unique_parties <- unique(gdf$party)

for (party in unique_parties) {
  # Filter the data for the current party
  party_data <- gdf[gdf$party == party, ]
  
  # Add a trace for the current party
  fig <- fig %>% add_trace(
    type = "choropleth",
    geojson = data,
    locations = party_data$WKR_NR,  # Use the WKR_NR from filtered data
    z = rep(1, nrow(party_data)),  # Dummy z value, since color is fixed
    colorscale = list(0 = c(0, primary_color[party]), 1 = c(1, primary_color[party])),  # Single color scale
    marker = list(line = list(width = 1), opacity = 0.7),  # Adjust line width and opacity
    showscale = FALSE  # Hide the color scale bar
  )
}

# Layout adjustments to remove world borders and center the map
fig <- fig %>% layout(
  geo = list(
    showcoastlines = FALSE,  # Hide coastlines
    showland = FALSE,       # Hide land
    projection = list(type = "mercator"),
    center = list(lon = mean(c(9.708774, 10.5)), lat = mean(c(54.830533, 53.0))),  # Adjust based on your data
    fitbounds = "locations",  # Automatically fit bounds to your data
    showcountries = FALSE    # Hide country borders
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0)  # Remove margins
)

# Show the map
fig


