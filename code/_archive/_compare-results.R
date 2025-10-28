
# Erststimme Model
prediction_data_districts <- readRDS("../prediction-2025/output/prediction_data_districts.rds")

prediction_data_districts <- prediction_data_districts %>% 
  dplyr::select(c(wkr, party, winner, probability, value, low, high, value_l1, zs_value, zs_value_l1, zs_low, zs_high))

compare_data <- prediction_data_districts # merge(voteshares_WKR_means, prediction_data_districts, by = c("wkr", "party"), all = TRUE)

compare_data <- compare_data %>% 
  dplyr::select(wkr, party, value, value_l1, zs_value, zs_value_l1)


# Yougov MrP Model
yougov_mrp <- read.xlsx("../prediction-2025/data/WKR-Shares-Projections-Publication-2025-02-19.xlsx")

yougov_mrp <- yougov_mrp %>%
  pivot_longer(
    cols = c(UnionShare:SonstigeHi),  # All party-related columns
    names_to = c("party", ".value"),
    names_pattern = "([A-Za-z]+)(Share|Lo|Hi)"
  ) %>%
  rename(
    wkr = WK_ID,
    value = Share,  # Main vote share
    low = Lo,  # Lower bound
    high = Hi  # Upper bound
  ) %>%
  mutate(
    party = recode(party, "Union" = "cdu", "SPD" = "spd", "AfD" = "afd", "Green" = "gru", 
                   "FDP" = "fdp", "Linke" = "lin", "BSW" = "bsw", "FW" = "fw", "Sonstige" = "oth"),
    Winner25 = str_replace_all(Winner25, c("Union" = "cdu", "SPD" = "spd", "AfD" = "afd", "Green" = "gru", 
                                           "FDP" = "fdp", "Linke" = "lin", "BSW" = "bsw", "FW" = "fw", "Sonstige" = "oth")),
    Winner21 = str_replace_all(Winner21, c("Union" = "cdu", "SPD" = "spd", "AfD" = "afd", "Green" = "gru", 
                                           "FDP" = "fdp", "Linke" = "lin", "BSW" = "bsw", "FW" = "fw", "Sonstige" = "oth")),
    Second25 = str_replace_all(Second25, c("Union" = "cdu", "SPD" = "spd", "AfD" = "afd", "Green" = "gru", 
                                           "FDP" = "fdp", "Linke" = "lin", "BSW" = "bsw", "FW" = "fw", "Sonstige" = "oth")),
    YouGovCall = str_replace_all(YouGovCall, c("Union" = "cdu", "SPD" = "spd", "AfD" = "afd", "Green" = "gru", 
                                               "FDP" = "fdp", "Linke" = "lin", "BSW" = "bsw", "FW" = "fw", "Sonstige" = "oth")),
    ChangeFrom21 = str_replace_all(ChangeFrom21, c("Union" = "cdu", "SPD" = "spd", "AfD" = "afd", "Green" = "gru", 
                                                   "FDP" = "fdp", "Linke" = "lin", "BSW" = "bsw", "FW" = "fw", "Sonstige" = "oth")),
    Winner25 = Winner25 == party,
    Winner21 = Winner21 == party,
    Second25 = Second25 == party
  ) %>%
  # Aggregate the 'fw' and 'oth' categories
  mutate(party = recode(party, "fw" = "oth", "oth" = "oth")) %>%
  group_by(wkr, party) %>%
  # Aggregate the values of 'fw' and 'oth' together
  summarise(
    Winner25 = as.numeric(unique(Winner25)),
    value_yougov = sum(value, na.rm = TRUE), 
    low = sum(low, na.rm = TRUE), 
    high = sum(high, na.rm = TRUE),
    .groups = "drop"  # Drops the grouping after summarisation
  ) %>%
  ungroup() %>%
  dplyr::select(wkr, party, value_yougov, winner_yougov = Winner25)

# Check result
head(yougov_mrp)

compare_data <- merge(compare_data, yougov_mrp, by = c("wkr", "party"), all = TRUE)
compare_data %>% names

compare_data <- dplyr::select(compare_data, c(wkr, party, value_yougov, value_old = value, value_l1, zs_value, zs_value_l1, winner_yougov))

compare_data <- compare_data %>%
  # Group by wkr to calculate max values for each group
  group_by(wkr) %>%
  mutate(
    # winner_mrp = if_else(value_mrp == max(value_mrp, na.rm = TRUE), 1, 0),
    # winner_yougov = if_else(value_yougov == max(value_yougov, na.rm = TRUE), 1, 0),
    winner_old = if_else(value_old == max(value_old, na.rm = TRUE), 1, 0)
  ) %>%
  ungroup() %>%
  dplyr::select(wkr, party, value_yougov, value_old, value_l1, zs_value, zs_value_l1, 
         winner_yougov, winner_old)




# election.de
load("data/2025-02-14-election-de.RData")
election_de_results_df <- election_de_results_df %>% dplyr::select(wkr = wkr_nr, party, prob_election_de = prob)
election_de_results_df$party <- election_de_results_df$party %>% str_replace_all(c("CDU" = "cdu", "GRÜNE" = "gru", "SPD" = "spd", "AfD" = "afd", "DIE LINKE" = "lin", "CSU" = "cdu"))

election_de_results_df <- election_de_results_df %>% group_by(wkr) %>% 
  mutate(winner_election_de = (prob_election_de == max(prob_election_de)) %>% as.numeric)

compare_data <- merge(compare_data, election_de_results_df, by = c("wkr", "party"), all = T)


# Drop if value_old NA
compare_data <- compare_data %>% filter(!is.na(value_old))

# Make election_de_results_df$election_de_winner FALSE if NA
compare_data$winner_election_de[is.na(compare_data$winner_election_de)] <- 0

compare_data$prob_election_de[is.na(compare_data$prob_election_de)] <- 0


compare_data <- compare_data %>% 
  group_by(wkr) %>% 
  mutate(winner_l1 = as.numeric(value_l1 == max(value_l1)))

compare_data %>% 
  ungroup() %>% 
  dplyr::select(wkr, party, starts_with("value")) %>% 
  mutate(
    error_yougov_v_l1 = (value_yougov - value_l1)^2,
    error_old_v_l1 = (value_old - value_l1)^2
  ) %>% 
  summarise(
    rmse_yougov_v_l1 = sqrt(mean(error_yougov_v_l1)),
    rmse_old_v_l1 = sqrt(mean(error_old_v_l1))
  )


compare_data %>% 
  ungroup() %>% 
  dplyr::select(wkr, party, starts_with("winner")) %>% names


accuracy_data <- compare_data %>% 
  ungroup() %>% 
  mutate(
    correct_old = (winner_old == winner_l1),
    correct_election_de = (winner_election_de == winner_l1)
  ) %>% 
  summarise(
    share_correct_old = mean(correct_old, na.rm = TRUE),
    share_correct_election_de = mean(correct_election_de, na.rm = TRUE),
    share_correct_yougov = mean(winner_yougov == winner_l1, na.rm = TRUE)
  )

print(accuracy_data)

accuracy_data %>% 
  pivot_longer(cols = everything(), names_to = "model", values_to = "share") %>% 
  ggplot(aes(x = model, y = share, fill = model)) +
  geom_bar(stat = "identity") +
  labs(title = "Prediction Accuracy of Winner Models",
       y = "Share Correct",
       x = "Model") +
  coord_flip() +
  theme_minimal()
