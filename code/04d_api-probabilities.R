
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
            cdu = coal_majo(c(posterior_draw[party == "cdu"]),share_above_hurdle),
            
            left_hurdle = posterior_draw[party == "lin"] > 0.05,
            fdp_hurdle = posterior_draw[party == "fdp"] > 0.05,
            bsw_hurdle = posterior_draw[party == "bsw"] > 0.05,
            
            
  ) %>%
  ungroup() %>%
  summarise("Union"  = mean(cdu_larger)*100,
            "AfD"  = mean(spd_larger)*100,
            "SPD"  = mean(afd_larger)*100,

            "Union + SPD + Grüne" = mean(cduspdgreens)*100,
            "Union + Grüne" = mean(cdugreens) *100,
            "Union + SPD" = mean(cduspd) * 100,
            "Union allein" = mean(cdu) * 100,
            
            
            "Linke" = mean(left_hurdle)*100,
            "FDP" = mean(fdp_hurdle)*100,
            "BSW" = mean(bsw_hurdle)*100,
            ) %>% 
  # Round all values to 0 decimal places
  round(0) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "category", values_to = "probability")

plot_forecast$type <- c(
  rep("Stärkste Partei", 3), # winner_* categories
  rep("Mehrheit", 4),    # coalition categories
  rep("5%-Hürde", 3)         # party_hurdle categories
)





# Sort by group (type) and then by probability in descending order
forecast_sorted <- plot_forecast %>%
  arrange(type, (probability))


# Create the plot for each type separately
majorities <- plot_ly(
  data = forecast_sorted %>% filter(type == "Mehrheit"),
  x = ~probability,
  y = ~factor(category, levels = forecast_sorted$category), # Maintain sorted order
  type = 'bar',
  orientation = 'h',
  text = ~paste0(probability, '%'),      # Text to display (percentage)
  textposition = "outside"              # Position the text outside the bars
) %>%
  layout(
    title = "", # Mehrheiten
    xaxis = list(title = "%", fixedrange = TRUE),
    yaxis = list(title = "", fixedrange = TRUE),
    hovermode = FALSE                    # Disable hover interactions
  ) %>%
  config(displayModeBar = FALSE)
majorities

winner <- plot_ly(
  data = forecast_sorted %>% filter(type == "Stärkste Partei"),
  x = ~probability,
  y = ~factor(category, levels = forecast_sorted$category), # Maintain sorted order
  type = 'bar',
  orientation = 'h',
  text = ~paste0(probability, '%'),      # Text to display (percentage)
  textposition = "outside"              # Position the text outside the bars
) %>%
  layout(
    title = "", # Stärkste Kraft
    xaxis = list(title = "%", fixedrange = TRUE),
    yaxis = list(title = "", fixedrange = TRUE),
    hovermode = FALSE                    # Disable hover interactions
  ) %>%
  config(displayModeBar = FALSE)
winner

hurdle <- plot_ly(
  data = forecast_sorted %>% filter(type == "5%-Hürde"),
  x = ~probability,
  y = ~factor(category, levels = forecast_sorted$category), # Maintain sorted order
  type = 'bar',
  orientation = 'h',
  text = ~paste0(probability, '%'),      # Text to display (percentage)
  textposition = "outside"              # Position the text outside the bars
) %>%
  layout(
    title = "", # 5%-Hürde
    xaxis = list(title = "%", fixedrange = TRUE),
    yaxis = list(title = "", fixedrange = TRUE),
    hovermode = FALSE                    # Disable hover interactions
  ) %>%
  config(displayModeBar = FALSE)
hurdle


saveWidget(majorities, "api/majorities.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()

saveWidget(winner, "api/winner.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()


saveWidget(hurdle, "api/hurdle.html", selfcontained = TRUE, title = "Wahlprognose von zweitstimme.org") %>% 
  suppressWarnings()




