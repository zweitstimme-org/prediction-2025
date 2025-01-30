message("Erststimme plot.")

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
# wahlkreise_sf <- st_read("data/btw25_geometrie_wahlkreise_shp/btw25_geometrie_wahlkreise_shp.shp")

gdf <- districts %>% filter(winner == 1)

pred_vacant <- readRDS("api/pred_vacant.rds")

gdf <- merge(gdf, pred_vacant %>% dplyr::select(-wkr_name), by = "wkr", all.x = TRUE) %>% 
  # NA abandon_p to 0
  mutate(abandon_p = ifelse(is.na(abandon_p), 0, abandon_p))

# Load necessary libraries
library(sf)           # For handling spatial data
library(rmapshaper)   # For simplifying geometries
library(geojsonsf)

# Read your GeoJSON file
geojson_file <- "data/germany-25-wahlkreise.geojson"
# geojson_text <- readLines(geojson_file)
geo_data <- sf::st_read(geojson_file)

geo_data_simplified <- ms_simplify(geo_data, keep = 0.2, keep_shapes = TRUE)
# geo_data <- rjson::fromJSON(paste(geojson_text, collapse = ""))
# geo_data$features[[5]]$geometry$type

# Simplify all that are not multipolygon
vec <- sapply(geo_data_simplified$geometry, function (x) class(x)[2]) == "POLYGON"
geo_data[vec, ] <- ms_simplify(geo_data[vec, ], keep = 0.2, keep_shapes = TRUE)

# What is needed here to make geojson_text?
geojson_text <- geojsonsf::sf_geojson(geo_data)

# Now, try parsing
library(rjson)
data <- rjson::fromJSON(paste(geojson_text, collapse = ""))
data$features[[66]]

data$features[[66]]$geometry$type




for (k in 1:299) {
  print(k)
  
  if (data$features[[k]]$geometry$type == "MultiPolygon") next
  
  for (i in 1:length(data$features[[k]]$geometry$coordinates)) {
    print(i)
    data$features[[k]]$geometry$coordinates[[i]] <- data$features[[k]]$geometry$coordinates[[i]][length(data$features[[k]]$geometry$coordinates[[i]]):1]

    # vec <- seq_along(data$features[[k]]$geometry$coordinates[[i]][[1]])
    # vec <- unique(c(1, seq(1, length(vec), by = 3), length(vec)))
    # if (length(vec) < 50) data$features[[k]]$geometry$coordinates[[i]] <- NULL else 
    # if(length(vec) > 10) data$features[[k]]$geometry$coordinates[[i]] <- data$features[[k]]$geometry$coordinates[[i]][[1]][vec] %>% list
    
  }
  
}



# Add an ID variable to each feature
for (i in seq_along(data$features)) {
  data$features[[i]]$properties$id <- data$features[[i]]$properties$WKR_NR
  data$features[[i]]$id <- data$features[[i]]$properties$WKR_NR
}


#########
# Plotly
########

# Create the Plotly map
fig <- plot_ly()

# Loop through each unique party and add traces
unique_parties <- unique(gdf$party)


# f <- 67
for (party in unique_parties) {
  # Filter the data for the current party
  party_data <- gdf[gdf$party == party, ]
  # party_data <- gdf[f,]
  
  contestors <- districts %>% filter(wkr %in% party_data$wkr & probability >= 5 & !winner) %>%
    dplyr::select(wkr, partei, value) %>% group_by(wkr) %>% arrange(-value) %>%
    summarise(contestors = str_c(", ", str_c(partei, " (", value %>% round(0), "%)", collapse = ", "))) %>%
    merge(party_data, by = "wkr", all = T) %>% 
    # Mutate contestors from NA to ""
    mutate(contestors = ifelse(is.na(contestors), "", contestors))
  
  new_data <- data
  # new_data$features <- new_data$features[gdf$party == party]
  
  new_data$features <- new_data$features[gdf$party == party]
  # new_data$features <- data$features[f]
  
  # Create custom hover text
  hover_text <- paste0(
    party_data$wkr, "<br>",
    party_data$wkr_name, "<br>",
    party_data$party %>% str_replace_all(c("afd" = "AfD", "cdu" = "CDU/CSU", "bsw" = "BSW", "gru" = "Grüne", "spd" = "SPD", "lin" = "Linke")),
    " (", party_data$value %>% round(0), "%)", contestors$contestors
  )
  
  
  
  colorscale <- list(
    c(0, paste0("rgba(", 
                col2rgb(primary_color[party])[1], ",", 
                col2rgb(primary_color[party])[2], ",", 
                col2rgb(primary_color[party])[3], ", 0.2)")),  # Start with invisible party color
    c(1, primary_color[[party]])  # End with fully opaque party color
  )
  
  # Add a trace for the current party
  fig <- fig %>% add_trace(
    type = "choropleth",
    geojson = new_data,
    locations = party_data$wkr,  # Use the WKR_NR from filtered data
    z = party_data$probability,  # Dummy z value, since color is fixed
    colorscale = colorscale,  # Single color scale
    marker = list(
      line = list(
        width = 1,  # Set the border width
        color = "light gray"  # Set the border color to gray
      )  # Adjust the fill opacity
    ),
    showscale = FALSE,  # Hide the color scale bar
    text = hover_text,  # Pass the custom hover text
    hoverinfo = "text"  # Use only the text for hover information
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
    showframe = FALSE,       # Remove the map frame
    showcountries = FALSE    # Hide country borders
    
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0)  # Remove margins
) %>% 
  plotly::config(displayModeBar = FALSE)

# Show the map
fig

# Save 
saveWidget(fig, "api/map_value.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org")





# Create the Plotly map
fig <- plot_ly()

# Loop through each unique party and add traces
unique_parties <- unique(gdf$party)


# f <- 67
for (party in unique_parties) {
  # Filter the data for the current party
  party_data <- gdf[gdf$party == party, ]
  # party_data <- gdf[f,]
  
  contestors <- districts %>% filter(wkr %in% party_data$wkr & probability >= 5 & !winner) %>%
    dplyr::select(wkr, partei, probability) %>% group_by(wkr) %>% arrange(-probability) %>%
    summarise(contestors = str_c(", ", str_c(partei, " (", probability, "%)", collapse = ", "))) %>%
    merge(party_data, by = "wkr", all = T) %>% 
    # Mutate contestors from NA to ""
    mutate(contestors = ifelse(is.na(contestors), "", contestors))
  
  new_data <- data
  # new_data$features <- new_data$features[gdf$party == party]
  
  new_data$features <- new_data$features[gdf$party == party]
  # new_data$features <- data$features[f]
  
  # Create custom hover text
  hover_text <- paste0(
    party_data$wkr, "<br>",
    party_data$wkr_name, "<br>",
    party_data$party %>% str_replace_all(c("afd" = "AfD", "cdu" = "CDU/CSU", "bsw" = "BSW", "gru" = "Grüne", "spd" = "SPD", "lin" = "Linke")),
    " (", party_data$probability, "%)", contestors$contestors
  )
  


  colorscale <- list(
    c(0, paste0("rgba(", 
                col2rgb(primary_color[party])[1], ",", 
                col2rgb(primary_color[party])[2], ",", 
                col2rgb(primary_color[party])[3], ", 0.2)")),  # Start with invisible party color
    c(1, primary_color[[party]])  # End with fully opaque party color
  )
  
  # Add a trace for the current party
  fig <- fig %>% add_trace(
    type = "choropleth",
    geojson = new_data,
    locations = party_data$wkr,  # Use the WKR_NR from filtered data
    z = party_data$probability,  # Dummy z value, since color is fixed
    colorscale = colorscale,  # Single color scale
    marker = list(
      line = list(
        width = 1,  # Set the border width
        color = "light gray"  # Set the border color to gray
      )  # Adjust the fill opacity
    ),
    showscale = FALSE,  # Hide the color scale bar
    text = hover_text,  # Pass the custom hover text
    hoverinfo = "text"  # Use only the text for hover information
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
    showframe = FALSE,       # Remove the map frame
    showcountries = FALSE    # Hide country borders
    
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0)  # Remove margins
) %>% 
  plotly::config(displayModeBar = FALSE)

# Show the map
fig

# Save 
saveWidget(fig, "api/map_probability.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org")





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
  mutate(label = paste0(party %>% str_replace_all(c("afd" = "AfD", "cdu" = "CDU/CSU", "bsw" = "BSW", "gru" = "Grüne", "spd" = "SPD", "lin" = "Linke")), " (", won_districts, ")")) %>%
  pull(label, name = party)

# # Plot with dynamic labels
# ggplot(gdf) +
#   geom_sf(aes(fill = factor(party), alpha = probability)) +
#   scale_fill_manual(
#     values = primary_color,
#     name = "",
#     labels = party_labels
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   guides(alpha = "none") +
#   coord_sf(expand = FALSE)  # Prevent extra space around the plot
# 
# 
# ggsave(
#   filename = "api/figure_forecast_districts.png",
#   device = "png",
#   dpi = 300,
#   height = 5,  # Adjust to fit your map's aspect ratio
#   width = 5,
#   bg = "white"
# )


# Assuming gdf has 'party', 'geometry' and 'probability' columns, and party is factorized properly
# Ensure gdf$party is a factor and it matches the primary_color keys
gdf$party <- factor(gdf$party)


# Vacant seats probability map



# Create the Plotly map
fig <- plot_ly()

  # Add a trace for the current party
  fig <- fig %>% add_trace(
    type = "choropleth",
    geojson = data,
    locations = gdf$wkr,  # Use the WKR_NR from filtered data
    z = gdf$abandon_p,  # Dummy z value, since color is fixed
    # Greyscale
    colorscale = list(
      c(0, "rgba(0, 0, 0, 0.0)"),  # Start with invisible party color
      c(1, "rgba(0, 0, 0, 0.9)")  # End with fully opaque party color
    ),
    marker = list(
      line = list(
        width = 1,  # Set the border width
        color = "light gray"  # Set the border color to gray
      )
    ),
    showscale = FALSE,  # Hide the color scale bar
    text = paste0(
      gdf$wkr, "<br>",
      gdf$wkr_name, "<br>",
      gdf$abandon_p*100, "%"
    ),  # Pass the custom hover text
    hoverinfo = "text"  # Use only the text for hover information
  )


# Layout adjustments to remove world borders and center the map
fig <- fig %>% layout(
  geo = list(
    showcoastlines = FALSE,  # Hide coastlines
    showland = FALSE,       # Hide land
    projection = list(type = "mercator"),
    center = list(lon = mean(c(9.708774, 10.5)), lat = mean(c(54.830533, 53.0))),  # Adjust based on your data
    fitbounds = "locations",  # Automatically fit bounds to your data
    showframe = FALSE,       # Remove the map frame
    showcountries = FALSE    # Hide country borders
    
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0)  # Remove margins
) %>% 
  plotly::config(displayModeBar = FALSE)

# Show the map
fig

# Save 
saveWidget(fig, "api/map_vacant.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org")
