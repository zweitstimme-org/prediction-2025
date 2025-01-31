
message("Static plots.")

df_forecast <- readRDS("api/forecast_api.rds")

df_forecast %>% mutate(value_label = round(value, 1),
                       value_label = ifelse(value_label %% 1 == 0, str_c(value_label, ".0"), value_label),
                       value_label = str_replace_all(value_label, "\\.", ",")) %>% 
  ggplot(aes(x = reorder(name, -value), y = value, color = name, fill = name)) +
  geom_hline(yintercept = 5, linetype = "dotted", linewidth = .5, color = "black") +
  geom_linerange(aes(ymin = low95, ymax = high95), linewidth = 10, alpha = 0.3, position=position_dodge(width=.5)) + # , col = party_colors[all_fcst$party_name]) +
  geom_linerange(aes(ymin = low, ymax = high), linewidth = 10, alpha = 0.7, position=position_dodge(width=.5)) + # , col = party_colors[all_fcst$party_name]) +
  geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
  geom_point(size = 2, fill = "white", shape = 21) +
  scale_color_manual(values = df_forecast$color, breaks = df_forecast$name) +
  scale_fill_manual(values = df_forecast$color, breaks = df_forecast$name) +
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
  labs(
    x = NULL,
    y = "%"
  ) +
  geom_text(aes(label = value_label, y = value), hjust = 2, size = 3, color = "black") 

ggsave(filename = "api/figure_forecast.pdf", height = 6, width = 6*1.5)
ggsave(filename = "api/figure_forecast.png", device = "png", dpi = 300,  height = 5, width = 5*1.5, bg = "white")


message("Interactive plots.")


font_family <- "'Segoe UI', -apple-system, sans-serif"

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



# # Plot
# pdf("output/fig/figure_forecast.pdf", width = 12, height = 8)
# par(mar=c(5,5,0,0)+.1)
# 
# x_val <- 1:7
# plot(x = x_val,
#      y = df_forecast$value, col = "white", 
#      type = "n", bty = "n", 
#      ylim = c(0,45), xlim = c(0,7.5),
#      xlab = "",
#      ylab = "",
#      yaxt = "n",
#      xaxt = "n",
#      cex.axis = 1.2)
# abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
# abline(h = c(0), lty = "solid", col = "lightgrey")
# abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
# 
# #segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low95, y1 = df_forecast$high95, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.6), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low, y1 = df_forecast$high, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.99), lend = 1)
# 
# points(x = x_val,
#        y = df_forecast$value, col = "white", lwd = 2)
# text(y = df_forecast$value, x = x_val-0.35, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
# #text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = x_val+0.35, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
# axis(1, at = x_val, labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
# axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
# mtext("Vote Share (%)", side=2, line=3, cex=1.2)
# 
# 
# 
# dev.off()
# 
# png("output/fig/figure_forecast.png", width = 1000, height = 600)
# par(mar=c(5,5,0,0)+.1)
# x_val <- 1:7
# plot(x = x_val,
#      y = df_forecast$value, col = "white", 
#      type = "n", bty = "n", 
#      ylim = c(0,45), xlim = c(0,7.5),
#      xlab = "",
#      ylab = "",
#      yaxt = "n",
#      xaxt = "n",
#      cex.axis = 1.2)
# abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
# abline(h = c(0), lty = "solid", col = "lightgrey")
# abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
# 
# #segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low95, y1 = df_forecast$high95, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.6), lend = 1)
# segments(x0 = x_val, y0 = df_forecast$low, y1 = df_forecast$high, 
#          lwd = 25, col = adjustcolor(df_forecast$color, alpha = 0.99), lend = 1)
# 
# points(x = x_val,
#        y = df_forecast$value, col = "white", lwd = 2)
# text(y = df_forecast$value, x = x_val-0.35, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
# #text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = x_val+0.35, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
# axis(1, at = x_val, labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
# axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
# mtext("Vote Share (%)", side=2, line=3, cex=1.2)
# dev.off()
