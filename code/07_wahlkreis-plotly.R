districts <- readRDS("api/forecast_districts.rds")

# Create a new column for the type of the forecast

winners <- districts %>% filter(winner == 1)

library(pacman)
p_load(tidyverse, rio, sf, patchwork, httr, jsonlite, plotly)

districts <- readRDS("api/forecast_districts.rds")

# Create a new column for the type of the forecast

coal_majo <- function(share, share_above_hurdle){
  if(any(share < 0.05)){
    return(FALSE)
  } else {
    return(sum(share)/share_above_hurdle > 0.5)
  }
}

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
ggplot(gdf) +
  geom_sf(aes(fill = factor(party), alpha = probability)) +  # 'party' as the factor for fill
  scale_fill_manual(
    values = primary_color,  # Map party names to custom colors
    name = "Party"  # Label for the legend
  ) +
  theme_void() +
  theme(
    legend.position = "none",  # Show the legend for the party color
    plot.title = element_text(hjust = 0.5)  # Center-align the title
  ) 
  
ggsave(filename = "output/fig/figure_forecast_districts.png", device = "png", dpi = 300,  height = 5*1.7, width = 5, bg = "white")
ggsave(filename = "api/figure_forecast_districts.png", device = "png", dpi = 300,  height = 5*1.7, width = 5, bg = "white")



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

# Check the 'party' column to ensure correct matching
unique(gdf$party)  # Make sure these match the names in primary_color

# Create the ggplot map with custom colors
ggplot_map <- ggplot(gdf) +
  geom_sf(aes(fill = party)) +  # 'party' as the factor for fill
  scale_fill_manual(
    values = primary_color,  # Map party names to custom colors
    name = "Party"  # Label for the legend
  ) +
  theme_void() +
  theme(
    legend.position = "none",  # Hide the legend
    plot.title = element_text(hjust = 0.5)  # Center-align the title
  )

# Convert the ggplot object to plotly for interactivity
plotly_map <- ggplotly(ggplot_map)

# Show the plotly map
plotly_map
