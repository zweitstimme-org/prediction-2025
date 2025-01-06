
message("Creating plotly for probabilities.")

forecast <- readRDS("api/forecast_draws.rds")


plot_forecast <- forecast %>%
  mutate(draw = 1:n()) %>% 
  pivot_longer(cdu:lin, values_to = "posterior_draw", names_to = "party") %>% 
  group_by(draw) %>%
  summarise(share_above_hurdle = sum(posterior_draw[posterior_draw > 0.05]),
            afd_larger = max(posterior_draw) == posterior_draw[party == "afd"],
            spd_larger = max(posterior_draw) == posterior_draw[party == "spd"],
            cdu_larger = max(posterior_draw) == posterior_draw[party == "cdu"],
            
            cduspdgreens = coal_majo(c(posterior_draw[party == "cdu"],posterior_draw[party == "spd"],posterior_draw[party == "gru"]),share_above_hurdle),
            cdugreens = coal_majo(c(posterior_draw[party == "cdu"],posterior_draw[party == "gru"]),share_above_hurdle),
            cduspd = coal_majo(c(posterior_draw[party == "cdu"],posterior_draw[party == "spd"]),share_above_hurdle),
            # cdu = coal_majo(c(posterior_draw[party == "cdu"]),share_above_hurdle),
            
            left_hurdle = posterior_draw[party == "lin"] > 0.05,
            fdp_hurdle = posterior_draw[party == "fdp"] > 0.05,
            bsw_hurdle = posterior_draw[party == "bsw"] > 0.05,
            
            
  ) %>%
  ungroup() %>%
  summarise("\n\nCDU/CSU"  = mean(cdu_larger)*100,
            "\n\nAfD"  = mean(spd_larger)*100,
            "\n\nSPD"  = mean(afd_larger)*100,

            "CDU/CSU + SPD + Grüne" = mean(cduspdgreens)*100,
            "\nCDU/CSU + Grüne" = mean(cdugreens) *100,
            "\nCDU/CSU + SPD" = mean(cduspd) * 100,
            # "CDU/CSU allein" = mean(cdu) * 100,
            
            
            "\n\nLinke" = mean(left_hurdle)*100,
            "\n\nFDP" = mean(fdp_hurdle)*100,
            "\n\nBSW" = mean(bsw_hurdle)*100,
            ) %>% 
  # Round all values to 0 decimal places
  round(0) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "category", values_to = "probability")

plot_forecast$type <- c(
  rep("Stärkste Partei", 3), # winner_* categories
  rep("Mehrheit", 3),    # coalition categories
  rep("5%-Hürde", 3)         # party_hurdle categories
)





# Sort by group (type) and then by probability in descending order
forecast_sorted <- plot_forecast %>%
  arrange(type, (probability))


# # Create the plot for each type separately
# majorities <- plot_ly(
#   data = forecast_sorted %>% filter(type == "Mehrheit"),
#   x = ~probability,
#   y = ~factor(category, levels = forecast_sorted$category), # Maintain sorted order
#   type = 'bar',
#   orientation = 'h',
#   text = ~paste0(probability, '%'),      # Text to display (percentage)
#   textposition = "outside"              # Position the text outside the bars
# ) %>%
#   layout(
#     title = "", # Mehrheiten
#     xaxis = list(title = "%", fixedrange = TRUE),
#     yaxis = list(title = "", fixedrange = TRUE),
#     hovermode = FALSE                    # Disable hover interactions
#   ) %>%
#   config(displayModeBar = FALSE)
# majorities
# 
# 
# winner <- plot_ly(
#   data = forecast_sorted %>% filter(type == "Stärkste Partei"),
#   x = ~probability,
#   y = ~factor(category, levels = forecast_sorted$category), # Maintain sorted order
#   type = 'bar',
#   orientation = 'h',
#   text = ~paste0(probability, '%'),      # Text to display (percentage)
#   textposition = "outside"              # Position the text outside the bars
# ) %>%
#   layout(
#     title = "", # Stärkste Kraft
#     xaxis = list(title = "%", fixedrange = TRUE),
#     yaxis = list(title = "", fixedrange = TRUE),
#     hovermode = FALSE                    # Disable hover interactions
#   ) %>%
#   config(displayModeBar = FALSE)
# winner
# 
# hurdle <- plot_ly(
#   data = forecast_sorted %>% filter(type == "5%-Hürde"),
#   x = ~probability,
#   y = ~factor(category, levels = forecast_sorted$category), # Maintain sorted order
#   type = 'bar',
#   orientation = 'h',
#   text = ~paste0(probability, '%'),      # Text to display (percentage)
#   textposition = "outside"              # Position the text outside the bars
# ) %>%
#   layout(
#     title = "", # 5%-Hürde
#     xaxis = list(title = "%", fixedrange = TRUE),
#     yaxis = list(title = "", fixedrange = TRUE),
#     hovermode = FALSE                    # Disable hover interactions
#   ) %>%
#   config(displayModeBar = FALSE)
# hurdle
# 
# 
# saveWidget(majorities, "api/majorities.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
#   suppressWarnings()
# 
# saveWidget(winner, "api/winner.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
#   suppressWarnings()
# 
# 
# saveWidget(hurdle, "api/hurdle.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
#   suppressWarnings()



library(ggplot2)
library(plotly)
library(dplyr)


# Function to prepare data and create pie charts
create_pie_chart <- function(data, type_filter, output_file) {
  # Filter and prepare data
  pie_data <- data %>%
    filter(type == type_filter) %>%
    mutate(remainder = 100 - probability) %>%
    pivot_longer(
      cols = c(probability, remainder),
      names_to = "slice",
      values_to = "value"
    ) %>%
    mutate(
      category = factor(category, levels = data %>%
                          filter(type == type_filter) %>%
                          arrange(desc(probability)) %>%
                          pull(category)),
      category = category %>% str_replace_all(" \\+ ", "\n")
    )
  
  num_facets <- length(unique(pie_data$category))
  
  
  # Add labels for percentages in the center of the pie charts
  center_labels <- pie_data %>%
    filter(slice == "probability") %>%
    distinct(category, .keep_all = TRUE) %>%
    mutate(label = paste0(round(value, 1), "%"))
  
  # Create the ggplot pie chart
  gg_pies <- pie_data %>%
    ggplot(aes(x = "", y = value, fill = slice)) +
    geom_bar(stat = "identity", width = 1, alpha = 0.85) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("black", "lightgrey")) +
    facet_wrap(~category, nrow = 1) +
    geom_text(
      data = center_labels,
      aes(x = 0.5, y = 45, label = label),
      color = "white",
      size = 5,
      inherit.aes = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold")
    ) +
    labs(fill = "Slice")
  
  # Save the output
  ggsave(filename = output_file, plot = gg_pies, height = num_facets/2, width = num_facets*1.5)
}

# Generate charts for 'Mehrheit', 'Stärkste Partei', and '5%-Hürde'
create_pie_chart(forecast_sorted, "Mehrheit", "api/majorities.png")
create_pie_chart(forecast_sorted, "Stärkste Partei", "api/winner.png")
create_pie_chart(forecast_sorted, "5%-Hürde", "api/hurdle.png")



