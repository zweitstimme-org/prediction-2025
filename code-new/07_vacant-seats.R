### ----------------------------------------------------------
### Calculate Vacant Seats in German Parliament
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

message("Calculating vacant seats...")

### 1. Configuration and Data Loading ------------------------

nsim <- 10000  # Number of simulations

# Load required data
btw_candidates_1983_2025 <- read.csv2("data/btw_candidates_1983-2025.csv", stringsAsFactors = FALSE)
prediction_data_districts <- readRDS("output/prediction_data_districts.rds")
district_reg_predictions <- readRDS("output/district_reg_predictions.rds")

# Get votes from last election
land_Z <- btw_candidates_1983_2025 %>% 
  filter(incumbent_party == 1 & election %in% c(2021)) %>% 
  group_by(election, land) %>% 
  summarise(valid_Z_l1 = sum(valid_Z_l1), 
            valid_Z = sum(valid_Z)) %>% 
  ungroup() %>% 
  arrange(election)

### 2. Load Latest Forecast --------------------------------

# Get most recent forecast file
forecast_files <- list.files("output", full.names = TRUE) %>% 
  str_subset("forecast_draws_")
latest_date <- max(ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")))
forecast_files <- forecast_files[ymd(str_extract(forecast_files, ".{10}(?=\\.rds)")) == latest_date]
draws <- readRDS(forecast_files)

### 3. Process CDU/CSU Split -------------------------------

# Calculate CDU/CSU split factors based on 2021 results
csu_votes <- 7571313
cdu_votes <- 28743222
csu_factor <- csu_votes / (csu_votes + cdu_votes)
cdu_factor <- cdu_votes / (csu_votes + cdu_votes)

# Split CDU column into CDU and CSU
draws <- cbind(draws, csu = round(draws[, "cdu"] * csu_factor, 4))
draws[, "cdu"] <- round(draws[, "cdu"] * cdu_factor, 4)

### 4. Process Vote Estimates -----------------------------

# Get shares per party and state
votes_est <- btw_candidates_1983_2025 %>% 
  filter(election %in% c(2021)) %>% 
  mutate(
    resp_Z = resp_Z %>% str_replace(",", ".") %>% as.numeric,
    party = substr(partei, 1, 3) %>% tolower(),
    party = if_else(party == "and", "oth", party)
  ) %>% 
  dplyr::select(land, wkr, party, valid_Z, resp_Z) %>% 
  filter(!is.na(valid_Z))

# Split Left party votes between Linke and BSW
lin_est <- votes_est %>% 
  filter(party == "lin") %>% 
  mutate(resp_Z = resp_Z/2)
bsw_est <- lin_est %>% 
  mutate(party = "bsw")

votes_est <- bind_rows(
  filter(votes_est, party != "lin"),
  lin_est,
  bsw_est
) %>% 
  as.data.frame()

### 5. Calculate Proportional Swing -----------------------

# Past election results at federal level
res_el <- c(
  cdu = 0.1890, spd = 0.2574, afd = 0.1034, 
  fdp = 0.1146, lin = 0.0489/2, gru = 0.1475, 
  csu = 0.0517, bsw = 0.0489/2, oth = 0.0875
)

# Reorder like draws
res_el <- res_el[colnames(draws)]

# Calculate swing
sim.swing <- -sweep(-draws, 2, -res_el)
sim.prop.swing <- t(apply(sim.swing, 1, function(x) x / res_el))

### 6. Calculate Predicted Votes --------------------------

# Initialize prediction matrix
zs_pred <- matrix(0, nrow = nrow(votes_est), ncol = nrow(sim.swing))

# Calculate predictions using vectorized operations
zs_pred <- t(apply(votes_est, 1, function(row) {
  party <- row["party"]
  resp_Z <- as.numeric(row["resp_Z"])
  if (party %in% colnames(sim.prop.swing)) {
    return(resp_Z + sim.prop.swing[, party] * resp_Z)
  } else {
    return(rep(0, ncol(sim.prop.swing)))
  }
}))

### 7. Process Vacant Seats ------------------------------

vacant_seats <- data.frame()

for (j in 1:nsim) {
  message(sprintf("Processing simulation %d of %d", j, nsim))
  
  # Get district forecast for this iteration
  district_draw <- cbind(
    pred = district_reg_predictions[, j], 
    prediction_data_districts
  ) %>%
    mutate(
      party = if_else(party == "cdu" & land == "BY", "csu", party),
      draw_winner = pred == max(pred),
      .by = wkr
    )
  
  # Get parties with at least 3 direct mandates
  grundmandat <- district_draw %>%
    filter(draw_winner) %>%
    dplyr::count(party) %>%
    filter(n >= 3) %>%
    pull(party)
  
  # Calculate votes for this iteration
  votes_est$zs_pred <- zs_pred[, j]
  votes_est$pred_Z_abs <- round(votes_est$valid_Z * votes_est$zs_pred, 0)
  
  # Get qualifying parties (>5% or grundmandat)
  full_draw <- draws[j, ]
  draw <- full_draw[full_draw >= 0.05 | names(full_draw) %in% grundmandat]
  draw <- draw[names(draw) != "oth"]
  draw <- draw/sum(draw)
  
  # Calculate seats
  listvote_party <- round(draw * sum(land_Z$valid_Z), 0)
  divisor <- sum(land_Z$valid_Z)/630
  seats <- round(listvote_party/divisor, 0)
  seats_sum <- sum(seats)
  
  # Adjust divisor until total seats = 630
  while(seats_sum != 630) {
    if (seats_sum > 630) {
      divisor_cand <- c(listvote_party / (seats - .5), 
                       listvote_party / (seats - 1.5))
    } else {
      divisor_cand <- c(listvote_party / (seats + .5), 
                       listvote_party / (seats + 1.5))
    }
    
    divisor_cand <- divisor_cand[divisor_cand > 0]
    if (seats_sum > 630) {
      divisor <- mean(sort(divisor_cand)[1:2])
    } else {
      divisor <- mean(sort(divisor_cand, decreasing = TRUE)[1:2])
    }
    
    seats <- round(listvote_party/divisor, 0)
    seats_sum <- sum(seats)
  }
  
  # Calculate seats by state
  seats_states <- data.frame()
  
  for (seats_party in names(seats)) {
    party_est <- votes_est %>%
      group_by(land, party) %>%
      summarise(
        pred_Z_abs = sum(pred_Z_abs),
        .groups = "drop"
      ) %>%
      filter(party == seats_party) %>%
      mutate(
        this_divisor = pred_Z_abs / seats[seats_party],
        seats = pred_Z_abs / this_divisor,
        seats_round = round(seats, 0)
      )
    
    # Adjust state seats until they match total party seats
    counter <- 0
    while(sum(party_est$seats_round) != seats[seats_party] && counter < 10) {
      if(sum(party_est$seats_round) > seats[seats_party]) {
        divisor_cand <- c(party_est$pred_Z_abs / (party_est$seats_round - .5),
                         party_est$pred_Z_abs / (party_est$seats_round - 1.5))
      } else {
        divisor_cand <- c(party_est$pred_Z_abs / (party_est$seats_round + .5),
                         party_est$pred_Z_abs / (party_est$seats_round + 1.5))
      }
      
      divisor_cand <- unique(divisor_cand[divisor_cand > 0])
      if(sum(party_est$seats_round) > seats[seats_party]) {
        this_divisor <- mean(sort(divisor_cand)[1:2])
      } else {
        this_divisor <- mean(sort(divisor_cand, decreasing = TRUE)[1:2])
      }
      
      party_est$seats_round <- round(party_est$pred_Z_abs/this_divisor, 0)
      counter <- counter + 1
    }
    
    party_est$zs_draw <- draw[seats_party]
    party_est$divisor <- this_divisor
    seats_states <- bind_rows(seats_states, party_est)
  }
  
  # Combine with district winners
  seats_states <- district_draw %>%
    group_by(land, party) %>%
    arrange(desc(draw_winner), desc(pred)) %>%
    mutate(rank = row_number()) %>%
    mutate(rank = if_else(!draw_winner, NA_integer_, rank)) %>%
    merge(seats_states, by = c("land", "party"), all.x = TRUE) %>%
    mutate(
      seats_round = coalesce(seats_round, 0),
      abandoned = draw_winner & rank > seats_round
    ) %>%
    arrange(party, land, rank) %>%
    dplyr::select(land, party, wkr, wkr_name, pred, rank, seats_round,
           abandoned, zs_draw, divisor, draw_winner) %>%
    mutate(iteration = j)
  
  vacant_seats <- bind_rows(vacant_seats, seats_states)
}

### 8. Save Results -------------------------------------

saveRDS(vacant_seats, "output/vacant_seats.rds")


## Add to API

# test <- merge(test, pred_abandoned  %>% filter(abandoned) %>%  group_by(wkr, party) %>% mutate(n = n()/max(iteration)) %>%
#                 summarise(abandon_p_party = mean(n) %>% round(2)),
#               by = c("wkr", "party"), all.x = T) %>% 
#   dplyr::mutate(abandon_p_party = case_when(is.na(abandon_p_party) ~ 0,
#                                             T ~ abandon_p_party)) 
# saveRDS(test, file = "api/forecast_districts.rds")
# 
# 
# 
# pred_abandoned %>%  group_by(land, wkr, wkr_name) %>% mutate(n = n()/max(iteration)) %>%
#   summarise(abandon_p = mean(abandoned) %>% round(2)) %>% arrange(-abandon_p) %>% 
#   saveRDS(file = "api/pred_vacant.rds")

