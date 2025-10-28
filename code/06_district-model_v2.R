### ----------------------------------------------------------
### District-Level Election Forecast Model
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Load and Prepare Data --------------------------------

# Configuration

# Load latest forecast
forecast_files <- list.files("/mnt/forecasts/prediction-2025/forecast", full.names = TRUE) %>% 
  str_subset("forecast_draws_")

latest_date <- max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))
forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == latest_date]
forecast <- readRDS(forecast_files)

# Adjust party order
party_order <- c("cdu", "spd", "lin", "gru", "fdp", "afd", "bsw", "oth")
forecast <- forecast[1:nsim, match(party_order, colnames(forecast))]
colnames(forecast) <- c("CDU/CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")

### 2. Load and Process Candidate Data ----------------------

# Load historical candidate data
btw_candidates_1983_2025 <- read.csv("/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2025_full.csv", stringsAsFactors = FALSE)
# filter(btw_candidates_1983_2025, !(has_candidate_2025 | is.na(has_candidate_2025))) %>% View
btw_candidates_1983_2025 <- filter(btw_candidates_1983_2025, has_candidate_2025 | is.na(has_candidate_2025))
nrow(btw_candidates_1983_2025)

btw_candidates_1983_2025 %>% filter(election == 2025 & party == "bsw")

btw_candidates_1983_2025$no_cand_l1 <- as.numeric(btw_candidates_1983_2025$res_l1_E == 0)

btw_candidates_1983_2025$party %>% unique

# btw_candidates_1983_2025 %>% filter(party == "bsw") %>% View
# Process party names
# btw_candidates_1983_2025$partei[btw_candidates_1983_2025$partei == "CSU"] <- "CDU/CSU"
# btw_candidates_1983_2025$partei[btw_candidates_1983_2025$partei == "CDU"] <- "CDU/CSU"

# Load historical election results
res21 <- c(24.2, 25.7, 2.45*2, 14.7, 11.4, 10.4, 2.45*2, 8.7) / 100
res17 <- c(32.9, 20.5, 4.6, 8.9, 10.7, 12.6, 4.6, 5.9) / 100
res13 <- c(41.5, 25.7, 4.3, 8.4, 4.8, 4.7, 4.3, 6.3) / 100
btw_bund_res <- rbind(res21, res17, res13)
colnames(btw_bund_res) <- c("CDU/CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")
rownames(btw_bund_res) <- c("2021", "2017", "2013")

### 3. Process Candidate Data for 2025 ----------------------



### 4. Prepare Training and Test Data -----------------------

election <- 2025
election_l1 <- election - 4

# Split data
train <- btw_candidates_1983_2025 %>%
  filter(
    (election < 2025 & election != 1990) |
    (election == 1990 & east != 1),
    partei != "AND"
  )

test <- filter(btw_candidates_1983_2025, election == 2025)
test$weight <- 1

### 5. Train Linear Model ----------------------------------

# Define formula
# model_formula <- "resp_E ~ resp_Z + res_l1_E + incumbent_party + no_cand_l1"
# model_formula <- "resp_E ~ resp_Z + res_l1_E + incumbent + no_cand_l1 + female + east + akad + propPlatz + no_cand_l1 + alsoList + formercand" # AD no_cand
model_formula <- "resp_E ~ ncand + propPlatz + alsoList + resp_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1"

# Fit model
reg <- lm(model_formula, data = train)

summary(reg)

# Generate model uncertainty simulations
mu_nsim <- 25
S <- MASS::mvrnorm(n = mu_nsim, coef(reg), vcov(reg))

### 6. Process Test Data -----------------------------------

# Prepare test data frames
rf_df <- model.frame(as.formula(model_formula), data = train)
rf_test <- model.frame(as.formula(model_formula), data = test)

# Standardize column names
standard_cols <- c("ncand", "propPlatz", "alsoList", "resp_Z", "res_l1_E", "formercand", "east", "female", "incumbent", "akad", "incumbent_in_wkr", "no_cand_l1")
colnames(rf_df) <- colnames(rf_test) <- standard_cols

# colnames(rf_df) <-
#   colnames(rf_test) <-
#   c(
#     "resp_E", #  direct vote
#     "ncand", # number of candidates 
#     "propPlatz", # list spot divided by number state party list spots
#     "alsoList", # also on list
#     "resp_Z", # list vote (from Zweitstimme Model)
#     "res_l1_E", # lagged direct vote
#     "formercand", # former candidate
#     "east", # east germany
#     "female", # female
#     "incumbent", # incumbent
#     "akad", # phd title
#     "incumbent_in_wkr", # incumebent running in this district
#     "no_cand_l1" # no candidate in last election
#   )

### 7. Generate Predictions --------------------------------

# Calculate swings
res_el <- btw_bund_res[paste0(election_l1),]
sim.swing <- -sweep(-forecast, 2, -res_el)
# sim.prop.swing_old <- t(apply(sim.swing, 1, function(x) x / res_el))
sim.prop.swing <- t(sapply(1:nrow(forecast), function(draw)  (forecast[draw,] - res_el)/res_el))

# Initialize prediction matrix
zs_pred <- matrix(0, nrow = nrow(test), ncol = nrow(sim.swing))

# Generate predictions for each party
for (i in 1:nrow(test)) {
  if (test[i, "partei"] %in% colnames(sim.prop.swing)) {
    zs_pred[i,] <- test[i, "res_l1_Z"] + 
      sim.prop.swing[, colnames(sim.prop.swing) %in% test[i, "partei"]] * 
      test[i, "res_l1_Z"] * test[i, "weight"]
  } else if (test[i, "partei"] == "CSU") {
    zs_pred[i,] <- test[i, "res_l1_Z"] + 
      sim.prop.swing[, "CDU"] * test[i, "res_l1_Z"] * test[i, "weight"]
  } else {
    zs_pred[i,] <- test[i, "res_l1_Z"] + 
      sim.prop.swing[, "And"] * test[i, "res_l1_Z"] * test[i, "weight"]
  }
}

### 8. Generate Final Predictions --------------------------

# Initialize results storage
res_list <- vector("list", length = nsim)
district_reg_predictions <- matrix(0, nrow = nrow(test), ncol = nsim)

# standard_cols <- c("ncand", "propPlatz", "alsoList", "resp_Z", "res_l1_E", "formercand", "east", "female", "incumbent", "akad", "incumbent_in_wkr", "no_cand_l1")

# Generate predictions for each simulation
for (zsim in 1:nsim) {
  # Prepare test data
  test_sim <- data.frame(
    resp_E = test$resp_E,
    ncand = test$ncand,
    propPlatz = test$propPlatz,
    alsoList = test$alsoList,
    resp_Z = zs_pred[, zsim],
    res_l1_E = test$res_l1_E,
    formercand = test$formercand,
    east = test$east,
    female = test$female,
    incumbent = test$incumbent,
    akad = test$akad,
    incumbent_in_wkr = test$incumbent_in_wkr,
    no_cand_l1 = test$no_cand_l1
  )
  
  # Generate predictions
  test_x <- as.matrix(test_sim[, 2:ncol(test_sim)])
  # show var classes of test_sim
  # sapply(test_sim, class)
  
  # make all classes of test_x numeric
  test_x <- apply(test_x, 2, as.numeric)
  
  reg_test_mat <- cbind(1, test_x)
  reg_preds <- reg_test_mat %*% t(S) + 
    matrix(rnorm(nrow(reg_test_mat) * mu_nsim, 0, sd(residuals(reg))),
           nrow = nrow(reg_test_mat),
           ncol = mu_nsim)
  
  # Process predictions
  reg_preds[reg_preds < 0] <- 0
  
  # Normalize by district
  for (j in 1:299) {
    reg_preds[test$wkr == j,] <- sweep(
      reg_preds[test$wkr == j, ], 
      2, 
      colSums(reg_preds[test$wkr == j,]), 
      "/"
    )
  }
  
  district_reg_predictions[, zsim] <- rowMeans(reg_preds)
  cat(zsim, "of", nsim, "\n")
}

### 9. Process Final Results -------------------------------

# Calculate confidence intervals and means
test$low <- round(apply(district_reg_predictions, 1, quantile, 0.025) * 100, 1)
test$high <- round(apply(district_reg_predictions, 1, quantile, 0.975) * 100, 1)
test$value <- round(rowMeans(district_reg_predictions)*100, 1) # round(apply(district_reg_predictions, 1, quantile, .5) * 100, 1) # round(rowMeans(district_reg_predictions) * 100, 1)
# test$zs_pred <- rowMeans(zs_pred)

# Calculate winner probabilities
test$winner <- FALSE
test$probability <- NA

for (district in unique(test$wkr)) {
  district_preds <- district_reg_predictions[test$wkr == district, ]
  winners <- apply(district_preds, 2, which.max)
  winner_freq <- table(winners)
  most_freq_winner <- as.numeric(names(winner_freq)[which.max(winner_freq)])
  
  probabilities <- numeric(nrow(district_preds))
  for (i in 1:nrow(district_preds)) {
    probabilities[i] <- sum(winners == i) / length(winners)
  }
  
  test$winner[test$wkr == district] <- (1:nrow(district_preds) == most_freq_winner)
  test$probability[test$wkr == district] <- round(probabilities * 100, 0)
}

### 10. Format and Save Results ---------------------------

# Create standardized party names
test$party <- case_when(
  test$partei == "CDU/CSU" ~ "cdu",
  test$partei == "SPD" ~ "spd",
  test$partei == "LINKE" ~ "lin",
  test$partei == "GRUENE" ~ "gru",
  test$partei == "FDP" ~ "fdp",
  test$partei == "AFD" ~ "afd",
  test$partei == "BSW" ~ "bsw",
  TRUE ~ "oth"
)

# Format party names for display
test$partei[test$partei == "AND"] <- "And."
test$partei[test$partei == "AFD"] <- "AfD"
test$partei[test$partei == "GRUENE"] <- "Grüne"
test$partei[test$partei == "LINKE"] <- "Linke"

# Save final results
prediction_data_districts <- test %>% 
  dplyr::select(wkr, wkr_name, land, party, partei, winner, probability, 
         value, low, high, value_l1 = res_l1_E, 
         zs_value_l1 = res_l1_Z, incumbent_party, 
         zs_valid_l1 = valid_Z_l1, valid_l1 = valid_E_l1)

prediction_data_districts$zs_value_l1 <- round(prediction_data_districts$zs_value_l1*100, 1)
prediction_data_districts$value_l1 <- round(prediction_data_districts$value_l1*100, 1)

# wkr_zs_l1 <- c()
for (i in 1:299) {
  
  wkr_draws <- zs_pred[prediction_data_districts$wkr == i, ]
  wkr_sum <- colSums(wkr_draws)
  
  # Divide each column of wkr_draw by the corresponding wkr_sum
  zs_pred[prediction_data_districts$wkr == i, ] <- sweep(wkr_draws, 2, wkr_sum, "/")
  
  # wkr_zs_l1[i] <- prediction_data_districts[prediction_data_districts$wkr == i, "zs_value_l1"] %>% sum
  
}

# prediction_data_districts[prediction_data_districts$wkr == i, "party"]

# Add party vote var
prediction_data_districts$zs_value <- round(rowMeans(zs_pred)*100,1) # round(apply(zs_pred, 1, quantile, .5)*100, 1) # round(rowMeans(zs_pred)*100, 1)

# Add 83% confidence intervals as zs_low and zs_high
prediction_data_districts$zs_low <- round(apply(zs_pred, 1, quantile, 1/12)*100, 1)
prediction_data_districts$zs_high <- round(apply(zs_pred, 1, quantile, 11/12)*100, 1)


(prediction_data_districts %>% group_by(wkr) %>%
  summarise(
    zs_value = sum(zs_value)
  ))$zs_value %>% mean

# Save
saveRDS(prediction_data_districts, "output/prediction_data_districts.rds")

saveRDS(district_reg_predictions, "/mnt/forecasts/prediction-2025/temp/district_reg_predictions.rds")

# prediction_data_districts_new <- prediction_data_districts
# 
# prediction_data_districts <- readRDS("output/prediction_data_districts.rds")
# filter(prediction_data_districts, party %in% c("bsw", "lin"))$party %>% unique
# filter(prediction_data_districts_new, party %in% c("lin"))$winner %>% table
# data.frame(party = filter(prediction_data_districts, party %in% c("bsw", "lin"))$party, 
#            old = filter(prediction_data_districts, party %in% c("bsw", "lin"))$value, 
#            new = filter(prediction_data_districts_new, party %in% c("lin"))$value) %>% 
#   ggplot() +
#   geom_point(aes(x = old, y = new, color = party)) +
#   # add diagonal
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   labs(title = "Comparison of Linke and BSW Predictions",
#        x = "Old Predictions",
#        y = "New Predictions") + 
#   # make axis equal
#   coord_fixed()

