

### ----------------------------------------------------------
### Create District-Level Election Forecast Visualizations
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Load District Forecast Data --------------------------

# Load district forecast from most recent cutoff date
load("data/erststimme_interaktiv.RData")

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
  "oth" = "white"      # Others Grey
)

### 3. Process Geographic Data -----------------------------

# Set random seed for reproducibility
set.seed(123)

erststimme$party <- erststimme$party %>% str_replace_all(c("CDU/CSU" = "cdu", "AfD" = "afd", "DIE LINKE" = "lin", "GRÜNE" = "gru", "SPD" = "spd", "BSW" = "bsw", "FDP" = "fdp", "Sonstige" = "oth"))

# Filter for winning districts

# erststimme mode_winner_t TRUE wenn winner_percentage am groessten
gdf <- erststimme %>% group_by(wkr) %>% 
  mutate(mode_winner_t = mean == max(mean, na.rm = T),
         wkr = as.numeric(wkr)) %>% 
  filter(mode_winner_t) %>% arrange(wkr)


# gdf <- erststimme %>% 
#   filter(mode_winner_t)

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
  
  new_data <- data
  # new_data$features <- new_data$features[gdf$party == party]
  
  new_data$features <- new_data$features[gdf$party == party]
  # new_data$features <- data$features[f]
  
  # Create custom hover text
  hover_text <- paste0(
    party_data$wkr, "<br>",
    party_data$wkr_name, "<br>",
    party_data$party %>% str_replace_all(c("afd" = "AfD", "cdu" = "CDU/CSU", "bsw" = "BSW", "gru" = "Grüne", "spd" = "SPD", "lin" = "Linke")),
    " (", party_data$mean %>% round(0), "%)"
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
    z = party_data$mean,  # Dummy z value, since color is fixed
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

# Add click event handling
fig <- fig %>% 
  onRender("
    function(el, x) {
      el.on('plotly_click', function(d) {
        var wkr = d.points[0].location;  // Get the WKR number from the clicked district
        
        // Send message to parent window
        window.parent.postMessage(
          {
            wkr: parseInt(wkr)  // Convert to integer and send
          },
          '*'  // Allow any parent domain
        );
      });
    }
  ")

# Save 
saveWidget(fig, "api/map_citizen_mean.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org")


