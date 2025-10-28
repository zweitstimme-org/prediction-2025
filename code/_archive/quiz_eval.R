# Calculate winners
has_valid_results <- function(results) {
  !is.null(results) && 
    all(!is.na(c(results$cdu, results$afd, results$spd, results$gru, 
                 results$lin, results$bsw, results$fdp, results$oth, 
                 results$turnout)))
}

library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)

# Create output directory
dir.create("quiz", showWarnings = FALSE)

# Read and process submissions first
quiz_dir <- "api/quiz"
files <- list.files(quiz_dir, pattern = "*.json", full.names = TRUE)

process_submission <- function(file) {
  data <- jsonlite::fromJSON(file)
  base <- list(
    name = data$name,
    affiliation = data$affiliation,
    timestamp = ymd_hms(data$timestamp),
    turnout = data$turnout,
    projection_time = sprintf("%02d:%02d:%02d", 
                              data$firstProjection$hours,
                              data$firstProjection$minutes,
                              data$firstProjection$seconds)
  )
  c(base, as.list(data$parties))
}

quiz_data <- bind_rows(lapply(files, process_submission))

# Calculate projection seconds for later use
quiz_data <- quiz_data %>%
  mutate(projection_seconds = as.numeric(hms(projection_time)))

# Plot 0: Overview of participants by team
p0 <- ggplot(quiz_data, aes(x = affiliation, fill = affiliation)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Teilnehmer nach Team",
       x = "Team",
       y = "Anzahl") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold")) +
  scale_x_discrete(labels = c("Hertie School", "Uni Mannheim", "Uni Witten/Herdecke"))

# Add participant list
participant_list <- quiz_data %>%
  arrange(affiliation, name) %>%
  group_by(affiliation) %>%
  summarise(participants = paste(name, collapse = ", ")) %>%
  mutate(affiliation = case_when(
    affiliation == "hertie" ~ "Hertie School",
    affiliation == "mannheim" ~ "Uni Mannheim",
    affiliation == "witten" ~ "Uni Witten/Herdecke"
  ))

write.csv(participant_list, "quiz/participants.csv", row.names = FALSE)
ggsave("quiz/participants.png", p0, width = 12, height = 6)

# Now ask for results
cat("Do you have the actual election results? (yes/no): ")
has_results <- tolower(readline())

actual_results <- NULL
actual_time <- NULL

if (has_results == "yes") {
  # Get time first
  cat("\nEnter the actual time of first projection (HH:MM:SS): ")
  actual_time <- readline()
  
  # Get turnout
  cat("\nEnter the actual turnout (%): ")
  actual_turnout <- as.numeric(readline())
  
  # Get party results
  cat("\nEnter the actual results for each party:\n")
  actual_results <- list(
    cdu = as.numeric(readline("CDU/CSU (%): ")),
    afd = as.numeric(readline("AfD (%): ")),
    spd = as.numeric(readline("SPD (%): ")),
    gru = as.numeric(readline("Grüne (%): ")),
    lin = as.numeric(readline("Linke (%): ")),
    bsw = as.numeric(readline("BSW (%): ")),
    fdp = as.numeric(readline("FDP (%): ")),
    oth = as.numeric(readline("Sonstige (%): ")),
    turnout = actual_turnout
  )
  
  
  
  if (has_valid_results(actual_results)) {
    # 1. Zweitstimmen winner (MSE)
    actual_parties <- c(actual_results$cdu, actual_results$afd, actual_results$spd, 
                        actual_results$gru, actual_results$lin, actual_results$bsw, 
                        actual_results$fdp, actual_results$oth)
    
    mse_scores <- quiz_data %>%
      rowwise() %>%
      mutate(mse = mean((c(cdu, afd, spd, gru, lin, bsw, fdp, oth) - actual_parties)^2)) %>%
      ungroup()
    
    zweitstimmen_winner <- mse_scores %>%
      filter(mse == min(mse)) %>%
      sample_n(1) %>%  # Random selection if tied
      pull(name)
    
    # 2. Time winner (closest to actual)
    actual_seconds <- as.numeric(hms(actual_time))
    time_diff <- quiz_data %>%
      mutate(diff = abs(projection_seconds - actual_seconds))
    
    time_winner <- time_diff %>%
      filter(diff == min(diff)) %>%
      sample_n(1) %>%  # Random selection if tied
      pull(name)
    
    # 3. Turnout winner (closest)
    turnout_diff <- quiz_data %>%
      mutate(diff = abs(turnout - actual_results$turnout))
    
    turnout_winner <- turnout_diff %>%
      filter(diff == min(diff)) %>%
      sample_n(1) %>%  # Random selection if tied
      pull(name)
    
    # Save winners to file
    winners_df <- data.frame(
      category = c("Zweitstimmen", "Zeit", "Wahlbeteiligung"),
      winner = c(zweitstimmen_winner, time_winner, turnout_winner),
      team = c(
        quiz_data$affiliation[quiz_data$name == zweitstimmen_winner][1],
        quiz_data$affiliation[quiz_data$name == time_winner][1],
        quiz_data$affiliation[quiz_data$name == turnout_winner][1]
      ),
      score = c(
        sprintf("MSE: %.2f", min(mse_scores$mse)),
        sprintf("Abweichung: %d Sekunden", min(time_diff$diff)),
        sprintf("Abweichung: %.1f%%", min(turnout_diff$diff))
      )
    )
    
    # Create winners visualization
    p_winners <- ggplot(winners_df, aes(x = category, y = 1, fill = team)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%s\n(%s)\n%s", winner, team, score)), 
                size = 4, vjust = 0.5) +
      theme_minimal() +
      labs(title = "Gewinner in allen Kategorien",
           x = NULL, y = NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size = 16, face = "bold"))
    
    ggsave("quiz/winners_overview.png", p_winners, width = 12, height = 6)
    
    write.csv(winners_df, "quiz/winners.csv", row.names = FALSE)
    
    # Print winners
    cat("\nGewinner:\n")
    cat("\nZweitstimmen (kleinster MSE):", zweitstimmen_winner, 
        "(Team", quiz_data$affiliation[quiz_data$name == zweitstimmen_winner], ")")
    cat("\nZeit (nächste Schätzung):", time_winner,
        "(Team", quiz_data$affiliation[quiz_data$name == time_winner], ")")
    cat("\nWahlbeteiligung (nächste Schätzung):", turnout_winner,
        "(Team", quiz_data$affiliation[quiz_data$name == turnout_winner], ")")
  }
}

# Party predictions visualization
party_data <- quiz_data %>%
  select(name, cdu, afd, gru, spd, lin, bsw, oth, fdp) %>%
  pivot_longer(-name, names_to = "party", values_to = "prediction") %>%
  mutate(party = factor(party, levels = c("cdu", "afd", "spd", "gru", "lin", "bsw", "fdp", "oth")))

# Get current forecast
forecast <- jsonlite::fromJSON("http://polsci.uni-wh.de:8073/forecast")

# Calculate Sonstige as remainder
main_parties_value <- sum(forecast$value[forecast$name != "Sonstige"])
main_parties_low <- sum(forecast$low[forecast$name != "Sonstige"])
main_parties_high <- sum(forecast$high[forecast$name != "Sonstige"])

# Create reference values dataframe
reference_values <- data.frame(
  party = c("cdu", "afd", "spd", "gru", "lin", "bsw", "fdp", "oth"),
  forecast = c(
    forecast$value[forecast$name == "CDU/CSU"],
    forecast$value[forecast$name == "AfD"],
    forecast$value[forecast$name == "SPD"],
    forecast$value[forecast$name == "Grüne"],
    forecast$value[forecast$name == "Linke"],
    forecast$value[forecast$name == "BSW"],
    forecast$value[forecast$name == "FDP"],
    100 - main_parties_value  # Remainder for Sonstige
  ),
  low = c(
    forecast$low[forecast$name == "CDU/CSU"],
    forecast$low[forecast$name == "AfD"],
    forecast$low[forecast$name == "SPD"],
    forecast$low[forecast$name == "Grüne"],
    forecast$low[forecast$name == "Linke"],
    forecast$low[forecast$name == "BSW"],
    forecast$low[forecast$name == "FDP"],
    0  # Lower bound for Sonstige is 0
  ),
  high = c(
    forecast$high[forecast$name == "CDU/CSU"],
    forecast$high[forecast$name == "AfD"],
    forecast$high[forecast$name == "SPD"],
    forecast$high[forecast$name == "Grüne"],
    forecast$high[forecast$name == "Linke"],
    forecast$high[forecast$name == "BSW"],
    forecast$high[forecast$name == "FDP"],
    100 - main_parties_low  # Upper bound for Sonstige
  )
)

# Reference turnout (UPDATE AFTER ELECTION)
reference_turnout <- 76.6  # Past turnout from 2021

# Calculate median time for plot 3
median_time <- median(quiz_data$projection_seconds)
median_time_str <- format(as.POSIXct(median_time, origin = "1970-01-01"), "%H:%M:%S")

# Plot 1: Party predictions with means and actual results if available
p1 <- ggplot() +
  geom_boxplot(data = party_data,
               aes(x = party, y = prediction),
               fill = "grey90", alpha = 0.5,
               outlier.shape = NA)

if (has_valid_results(actual_results)) {
  # Add actual results data
  actual_data <- data.frame(
    party = c("cdu", "afd", "spd", "gru", "lin", "bsw", "fdp", "oth"),
    value = c(actual_results$cdu, actual_results$afd, actual_results$spd,
              actual_results$gru, actual_results$lin, actual_results$bsw,
              actual_results$fdp, actual_results$oth)
  )
  
  p1 <- p1 +
    geom_point(data = actual_data,
               aes(x = party, y = value),
               color = "blue", size = 5, shape = 18) +
    geom_text(data = actual_data,
              aes(x = party, y = value,
                  label = sprintf("%.1f%%", value)),
              color = "blue",
              vjust = -0.8,
              size = 3.5,
              fontface = "bold") +
    # Highlight winner's predictions
    geom_point(data = party_data %>% filter(name == zweitstimmen_winner),
               aes(x = party, y = prediction),
               color = "gold", size = 4, shape = 21,
               position = position_jitter(width = 0.2, height = 0))
}

p1 <- p1 +
  geom_linerange(data = reference_values,
                 aes(x = party, ymin = low, ymax = high),
                 color = "darkred", size = 2.5, alpha = 0.2) +
  geom_point(data = reference_values,
             aes(x = party, y = forecast),
             color = "darkred", size = 5, shape = 18) +
  geom_text(data = reference_values,
            aes(x = party, y = forecast, 
                label = sprintf("%.1f%%", forecast)),
            color = "darkred",
            vjust = -0.8,
            size = 3.5,
            fontface = "bold") +
  geom_point(data = party_data, 
             aes(x = party, y = prediction, color = name),
             position = position_jitter(width = 0.2, height = 0), 
             size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Zweitstimmen-Prognosen",
       subtitle = if (has_valid_results(actual_results)) {
         sprintf("Boxplots zeigen Verteilung der Tipps\nRote Rauten: Prognose mit Unsicherheit, Blaue Rauten: Tatsächliche Ergebnisse\nGewinner (gold): %s (Team %s)", 
                 zweitstimmen_winner, quiz_data$affiliation[quiz_data$name == zweitstimmen_winner])
       } else {
         "Boxplots zeigen Verteilung der Tipps\nRote Rauten und Linien zeigen aktuelle Prognose mit Unsicherheit"
       },
       x = "Partei", 
       y = "Prognose (%)",
       color = "Teilnehmer/in") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_x_discrete(labels = c("CDU/CSU", "AfD", "SPD", "Grüne", "Linke", "BSW", "FDP", "Sonstige"))
ggsave("quiz/party_predictions.png", width = 12, height = 8)

# Update existing plots to use team colors
team_colors <- c("hertie" = "#1f77b4", "mannheim" = "#ff7f0e", "witten" = "#2ca02c")

# Update party predictions plot
p1 <- p1 +
  aes(color = affiliation) +
  scale_color_manual(values = team_colors,
                     labels = c("Hertie School", "Uni Mannheim", "Uni Witten/Herdecke"))

# Plot 2: Turnout predictions with actual result if available
p2 <- ggplot(quiz_data) +
  geom_col(aes(x = reorder(name, turnout), y = turnout, fill = affiliation)) +
  geom_hline(yintercept = mean(quiz_data$turnout), linetype = "dashed", color = "black")

if (has_valid_results(actual_results)) {
  p2 <- p2 +
    geom_hline(yintercept = actual_results$turnout,
               color = "blue", size = 1) +
    annotate("text", x = 1, y = actual_results$turnout,
             label = sprintf("Tatsächlich: %.1f%%", actual_results$turnout),
             vjust = -0.5, color = "blue") +
    # Highlight winner
    geom_col(data = quiz_data %>% filter(name == turnout_winner),
             aes(x = reorder(name, turnout), y = turnout),
             fill = "gold", alpha = 0.8)
} else {
  p2 <- p2 +
    geom_hline(yintercept = reference_turnout, color = "darkred", size = 1) +
    annotate("text", x = 1, y = reference_turnout,
             label = "2021: 76.6%",
             vjust = -0.5, color = "darkred")
}

p2 <- p2 +
  theme_minimal() +
  labs(title = "Wahlbeteiligung-Prognosen",
       subtitle = if (has_valid_results(actual_results)) {
         sprintf("Gestrichelte Linie zeigt Durchschnitt\nGewinner (gold): %s (Team %s)", 
                 turnout_winner, quiz_data$affiliation[quiz_data$name == turnout_winner])
       } else {
         "Gestrichelte Linie zeigt Durchschnitt"
       },
       x = "Teilnehmer/in", 
       y = "Wahlbeteiligung (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold"))
ggsave("quiz/turnout_predictions.png", width = 12, height = 8)

# Update turnout plot
p2 <- p2 +
  aes(fill = affiliation) +
  scale_fill_manual(values = team_colors,
                    labels = c("Hertie School", "Uni Mannheim", "Uni Witten/Herdecke"))


quiz_data$projection_seconds %>% class

# Plot 3: Time predictions with actual time if available
p3 <- ggplot(quiz_data) +
  geom_point(aes(x = reorder(name, projection_seconds), 
                 y = projection_time, color = affiliation),
             size = 4) 

if (!is.null(actual_time) && actual_time != "") {
  p3 <- p3 +
    geom_hline(yintercept = actual_time,
               color = "blue", size = 1) +
    annotate("text", x = 1, y = actual_time,
             label = sprintf("Tatsächlich: %s", actual_time),
             vjust = -0.5, color = "blue") +
    # Highlight winner
    geom_point(data = quiz_data %>% filter(name == time_winner),
               aes(x = reorder(name, projection_seconds), y = projection_time),
               color = "gold", size = 6)
}

p3 <- p3 +
  geom_hline(yintercept = median_time_str,  # Use the formatted median time string
             linetype = "dashed", color = "black") +
  annotate("text", x = 1, y = median_time_str,
           label = if (!is.null(actual_time) && actual_time != "") {
             sprintf("Gestrichelte Linie zeigt Median\nGewinner (gold): %s (Team %s)", 
                     time_winner, quiz_data$affiliation[quiz_data$name == time_winner])
           } else {
             "Gestrichelte Linie zeigt Median"
           },
           vjust = -0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Prognostizierte Zeit der ersten Hochrechnung",
       subtitle = if (!is.null(actual_time) && actual_time != "") {
         sprintf("Gestrichelte Linie zeigt Median\nGewinner (gold): %s (Team %s)", 
                 time_winner, quiz_data$affiliation[quiz_data$name == time_winner])
       } else {
         "Gestrichelte Linie zeigt Median"
       },
       y = "Teilnehmer/in", 
       x = "Uhrzeit",
       color = "Teilnehmer/in") +
  theme(axis.text.y = element_text(hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("quiz/projection_times.png", width = 12, height = 8)

# Update time plot
p3 <- p3 +
  aes(color = affiliation) +
  scale_color_manual(values = team_colors,
                     labels = c("Hertie School", "Uni Mannheim", "Uni Witten/Herdecke"))

# Save data silently
write.csv(quiz_data, "quiz/all_predictions.csv", row.names = FALSE)

