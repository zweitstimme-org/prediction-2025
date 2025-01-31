message("Probabilities.")

####################
# Calculating seats
####################

# Get candidate data
load("data/btw_candidates_1983-2025.RData")

# Get data from district forecast
test <- readRDS("data/test.RDS")
res_pred <- readRDS("/mnt/forecasts/prediction-2025/temp/res_pred.RDS")

# Get the latest forecast file
(forecast_files <- list.files("/mnt/forecasts/prediction-2025/forecast", full.names = T) %>% str_subset("forecast_draws_"))
(forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))])

draws <- readRDS(forecast_files)

# Split CDU and CSU CSU: 7571313 CDU: 28743222
(csu_factor <- 7571313/(7571313+28743222))
(cdu_factor <- 28743222/(7571313+28743222))


# Split cdu column by factor to make cdu and csu
draws <- cbind(draws, csu = round(draws[, "cdu"]*csu_factor, 4))
draws[, "cdu"] <- round(draws[, "cdu"]*cdu_factor, 4)


# Get shares per party and state, then proportional swing, then multiply with list votes
votes_est <- btw_candidates_1983_2025 %>% filter(election %in% c(2021)) %>% 
  mutate(resp_Z = resp_Z %>% str_replace(",", ".") %>% as.numeric) %>%
  ungroup() %>% arrange(election) %>% 
  mutate(party = substr(partei, 1, 3) %>% tolower(),
         party = case_when(party == "and" ~ "oth",
                           T ~ party)) %>% 
  dplyr::select(land, wkr, party, valid_Z, resp_Z) %>% 
  filter(!is.na(valid_Z))

# Split lin voteshare between lin and bsw
lin_est <- votes_est %>% filter(party == "lin") %>% 
  mutate(resp_Z = resp_Z/2)
bsw_est <- lin_est %>% mutate(party = "bsw")
votes_est <- bind_rows(filter(votes_est %>% filter(party != "lin")), lin_est, bsw_est)
votes_est <- as.data.frame(votes_est)

# Past election results at federal level for proportional swing
res_el <- c(cdu = 0.1890, spd = 0.2574, afd = 0.1034, fdp = 0.1146, lin = 0.0489/2, gru = 0.1475, csu = 0.0517, bsw = 0.0489/2, oth = 0.0875)

# Reorder like draws
res_el <- res_el[colnames(draws)]

# Get district forecast
# forecast_districts <- readRDS("forecast_districts_2025-01-02.rds")



# Divisorverfahren Sainte-Laguë/Schepers


# 1. Determine parties with more than 5%, or at least 3 in three districts the majority, or national minority (unlikely)

# 2. Divide 630 seats according to the number of list votes to parties (subtract successful individual district candidates, unlikely)

# 3. Divide allocate seats to party lists at state level (need Zweitstimme result at state level?)


pred_probabilities <- data.frame()

for (j in 1:10000) {
  print(j)
  
  forecast_seed <- j # sample(1:500, 1)
  
  # Find parties to take into account
  
  # Draw from the district forecast
  district_draw <- cbind(pred = res_pred[, forecast_seed], test)  %>% 
    group_by(wkr) %>% filter(pred == max(pred))
  district_draw$party[district_draw$party == "cdu" & district_draw$land == "BY"] <- "csu"
  
  # Get parties which win at least 4 districts
  (grundmandat <- table(district_draw$party) %>% as.data.frame %>% filter(Freq >= 3) %>% pull(Var1) %>% as.character())
  
  # Get parties with at least 5% (or 3 districts? Take CDU?)
  # forecast_seed <- sample(1:ncol(draws), 1)
  
  full_draw <- draws[forecast_seed, ]
  draw <- full_draw[(full_draw >= 0.05) | colnames(draws) %in% grundmandat]
  
  # Remove oth
  draw <- draw[names(draw) != "oth"]
  
  # Make percentages sum to 1
  draw <- draw/sum(draw)
  
  # Now determine majorities
  pred_probabilities <- bind_rows(pred_probabilities, draw)
  
}

# Set all NA cells in all columns to 0
pred_probabilities[is.na(pred_probabilities)] <- 0


# saveRDS(pred_probabilities, "output/forecasts/pred_probabilities.rds")
data.frame(
  hurdle_lin = mean(pred_probabilities$lin > 0),
  hurdle_bsw = mean(pred_probabilities$bsw > 0),
  hurdle_fdp = mean(pred_probabilities$fdp > 0),
  hurdle_spd = mean(pred_probabilities$spd > 0),
  hurdle_cdu = mean(pred_probabilities$cdu > 0),
  hurdle_csu = mean(pred_probabilities$csu > 0),
  hurdle_gru = mean(pred_probabilities$gru > 0),
  hurdle_afd = mean(pred_probabilities$afd > 0),
  maj_cdu_csu_gru = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$gru) > 0.5),
  maj_cdu_csu_spd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$spd) > 0.5),
  maj_cdu_csu_gru_spd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$gru + pred_probabilities$spd) > 0.5),
  maj_cdu_csu_afd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$afd) > 0.5),
  prob_lin_largest = mean(pred_probabilities$lin > apply(pred_probabilities[, -which(names(pred_probabilities) == "lin")], 1, max)),
  prob_bsw_largest = mean(pred_probabilities$bsw > apply(pred_probabilities[, -which(names(pred_probabilities) == "bsw")], 1, max)),
  prob_fdp_largest = mean(pred_probabilities$fdp > apply(pred_probabilities[, -which(names(pred_probabilities) == "fdp")], 1, max)),
  prob_spd_largest = mean(pred_probabilities$spd > apply(pred_probabilities[, -which(names(pred_probabilities) == "spd")], 1, max)),
  prob_cdu_largest = mean(pred_probabilities$cdu > apply(pred_probabilities[, -which(names(pred_probabilities) == "cdu")], 1, max)),
  prob_csu_largest = mean(pred_probabilities$csu > apply(pred_probabilities[, -which(names(pred_probabilities) == "csu")], 1, max)),
  prob_gru_largest = mean(pred_probabilities$gru > apply(pred_probabilities[, -which(names(pred_probabilities) == "gru")], 1, max)),
  prob_afd_largest = mean(pred_probabilities$afd > apply(pred_probabilities[, -which(names(pred_probabilities) == "afd")], 1, max))
) %>% 
  saveRDS("api/pred_probabilities.rds")

  
  # data.frame(maj_cdu_csu_gru = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$gru) > .5),
  #            maj_cdu_csu_spd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$spd) > .5),
  #            maj_cdu_csu_gru_spd = mean((pred_probabilities$cdu + pred_probabilities$csu + pred_probabilities$gru + pred_probabilities$spd) > .5)) # %>% 
  # 