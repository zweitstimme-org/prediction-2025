message("Vacant seats.")

####################
# Calculating seats
####################
nsim <- 10000
# Get candidate data
load("data/btw_candidates_1983-2025.RData")

# Get data from district forecast
test <- readRDS("data/test.RDS")
res_pred <- readRDS("/mnt/forecasts/prediction-2025/temp/res_pred.RDS")

# Divide Linke and BSW
# 
# test %>% filter(party == "lin") %>% mutate(party = "bsw",
#                                            res_l1_Z = res_l1_Z/2,
#                                            res_l1_E = res_l1_E/2,

# Get votes at last election as estimate for this election
land_Z <- btw_candidates_1983_2025 %>% filter(incumbent_party == 1 & election %in% c(2021)) %>% 
  group_by(election, land) %>% summarise(valid_Z_l1 = sum(valid_Z_l1), valid_Z = sum(valid_Z)) %>% 
  ungroup() %>% arrange(election)

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


# Proportional swing
sim.swing <- -sweep(-draws, 2, -res_el)
sim.prop.swing <- t(apply(sim.swing, 1, function(x)
  x / res_el))

zs_pred <- matrix(0, nrow = nrow(votes_est), ncol = nrow(sim.swing))

# for (i in 1:nrow(votes_est)) if (votes_est[i, "party"] %in% colnames(sim.prop.swing)) zs_pred[i, ] <- votes_est[i, "resp_Z"] + sim.prop.swing[, colnames(sim.prop.swing) %in% votes_est[i, "party"]] * votes_est[i, "resp_Z"]
# Proportional swing
zs_pred <- t(apply(votes_est, 1, function(row) {
  party <- row["party"]
  resp_Z <- as.numeric(row["resp_Z"])
  if (party %in% colnames(sim.prop.swing)) {
    return(resp_Z + sim.prop.swing[, party] * resp_Z)
  } else {
    return(rep(0, ncol(sim.prop.swing)))
  }
}))

pred_abandoned <- data.frame()

for (j in 1:nsim) {
  print(j)
  
  forecast_seed <- j # sample(1:500, 1)
  
  # Find parties to take into account
  
  # Draw from the district forecast
  district_draw <- cbind(pred = res_pred[, forecast_seed], test)  %>% 
    group_by(wkr) %>% filter(pred == max(pred))
  district_draw$party[district_draw$party == "cdu" & district_draw$land == "BY"] <- "csu"
  

  
  # Rows are rows from votes_est, cols are per draw
  votes_est$zs_pred <- zs_pred[, forecast_seed]
  votes_est$pred_Z_abs <- round(votes_est$valid_Z * votes_est$zs_pred, 0) # For now take only second column
  
  # Get parties which win at least 4 districts
  (grundmandat <- table(district_draw$party) %>% as.data.frame %>% filter(Freq >= 3) %>% pull(Var1) %>% as.character())
  
  # Get parties with at least 5% (or 3 districts? Take CDU?)

  full_draw <- draws[forecast_seed, ]
  draw <- full_draw[(full_draw >= 0.05) | colnames(draws) %in% grundmandat]
  
  # Remove oth
  draw <- draw[names(draw) != "oth"]
  
  # Make percentages sum to 1
  draw <- draw/sum(draw)
  
  # Estimated list vote per party
  (listvote_party <- round(draw*sum(land_Z$valid_Z), 0))
  
  # Starting divisor
  (divisor <- sum(land_Z$valid_Z)/630) # 73717.5
  # (divisor <- sum(votes_est$pred_Z_abs)/630) # 90453))
  
  # Estimated seats per party
  (seats <- round(listvote_party/divisor, 0))
  
  (seats_sum <- round(seats %>% sum))
  
  # If sum is > 630 -> increase divisor, if sum is < 630 -> reduce divisor
  
  
  while(seats_sum != 630) {
    
    if (seats_sum > 630) {
      
      (divisor_cand <- c(listvote_party / (seats - .5), listvote_party / (seats - 1.5)))
      
      # Only positive divisors
      divisor_cand <- divisor_cand[divisor_cand > 0]
      
      # Choose new divisor between smallest and second smallest candidate
      (divisor_min <- sort(divisor_cand)[1])
      (divisor_max <- sort(divisor_cand)[2])
      
      
    } else {
      
      (divisor_cand <- c(listvote_party / (seats + .5), listvote_party / (seats + 1.5)))
      
      # Only positive divisors
      divisor_cand <- divisor_cand[divisor_cand > 0]
      
      # Choose new divisor between smallest and second smallest candidate
      (divisor_min <- sort(divisor_cand, decreasing = T)[1])
      (divisor_max <- sort(divisor_cand, decreasing = T)[2])
      
    }
    
    # New divisor
    (divisor <- (divisor_min + divisor_max)/2)
    (seats <- round(listvote_party/divisor, 0))
    (seats_sum <- round(seats %>% sum))
    
  }
  
  # divisor
  # seats
  # sum(seats)
  
  ##############################################
  # List votes per party and state (proportional swing?)
  ##############################################
  
  mytest <- votes_est %>% group_by(party) %>% dplyr::select(party, pred_Z_abs) %>%
    summarise(pred_Z_abs = sum(pred_Z_abs)) %>% pivot_wider(names_from = "party", values_from = "pred_Z_abs") %>% as.matrix
  
  
  # For each party in
  seats_allocated <- data.frame()
  
  seats_states <- data.frame()
  
  for (seats_party in names(seats)) {
    
    print(seats_party)
    
    if(seats_party == "fdp") next
    
    # Number of votes
    mytest[colnames(mytest) == seats_party]
    
    # Number of seats
    seats[seats_party]
    
    # Divisor
    (this_divisor <- mytest[colnames(mytest) == seats_party] / seats[seats_party])
    
    party_est <- votes_est %>% group_by(land, party) %>% 
      summarise(pred_Z_abs = sum(pred_Z_abs)) %>% filter(!is.na(pred_Z_abs)) %>% 
      filter(party == seats_party) %>% 
      mutate(seats = pred_Z_abs / this_divisor,
             seats_round = round(pred_Z_abs / this_divisor, 0)) %>% suppressMessages()
    
    sum(party_est$seats_round)
    
    while(sum(party_est$seats_round) != seats[seats_party]) {
      if(sum(party_est$seats_round) > seats[seats_party]) {
        
        (divisor_cand <- c(party_est$pred_Z_abs / (party_est$seats_round - .5), party_est$pred_Z_abs / (party_est$seats_round - 1.5)) %>% unique)
        
        # Only positive divisors
        (divisor_cand <- divisor_cand[divisor_cand > 0])
        
        # Choose new divisor between smallest and second smallest candidate
        (divisor_min <- sort(divisor_cand)[1])
        (divisor_max <- sort(divisor_cand)[2])
        
        
      } else {
        
        (divisor_cand <- c(party_est$pred_Z_abs / (party_est$seats_round + .5), party_est$pred_Z_abs / (party_est$seats_round + 1.5)) %>% unique)
        
        # Only positive divisors
        divisor_cand <- divisor_cand[divisor_cand > 0]
        
        # Choose new divisor between smallest and second smallest candidate
        (divisor_min <- sort(divisor_cand, decreasing = T)[1])
        (divisor_max <- sort(divisor_cand, decreasing = T)[2])

      }
      
      # New divisor
      (this_divisor <- (divisor_min + divisor_max)/2)
      (party_est$seats_round <- round(party_est$pred_Z_abs/this_divisor, 0))
    }
    
    # this_divisor
    sum(party_est$seats_round)
    
    seats_allocated <- bind_rows(seats_allocated, data.frame(party = seats_party, divisor = this_divisor, seats = sum(party_est$seats_round)))
    
    seats_states <- bind_rows(seats_states, party_est)
    
  }
  
  
  seats_states <- district_draw %>% # filter(winner == 1) %>% 
    group_by(land, party) %>%
    # Sort decreasing by value
    arrange(desc(value)) %>% 
    # Create a rank var
    mutate(rank = row_number()) %>% 
    merge(., seats_states, by = c("land", "party"), all.x = T) %>%
    mutate(seats_round = case_when(is.na(seats_round) ~ 0,
                                   T ~ seats_round)) %>% 
    mutate(abandoned = rank > seats_round) %>% 
    arrange(party, land, rank) %>%
    # filter(abandoned)  %>% 
    dplyr::select(land, party, wkr, wkr_name, pred, rank, seats_round, abandoned)
  
  seats_states$iteration <- j
  
  
  pred_abandoned <- bind_rows(pred_abandoned, seats_states)
  
  # if("lin" %in% filter(district_draw, winner == 1)$party) break
  
}

# (table(pred_abandoned$wkr) %>% sort(decreasing = T))/max(pred_abandoned$iteration)

# pred_abandoned %>% filter(abandoned) %>% View

# pred_abandoned %>% filter(abandoned) %>%  group_by(land, wkr, wkr_name, party) %>% mutate(n = n()/max(iteration)) %>%
#   summarise(abandon_p = mean(n),
#             value_mean = mean(pred)) %>% arrange(-abandon_p) %>% View

# Merge to test and make NA values 0, to add abandon_p
test <- merge(test, pred_abandoned  %>% filter(abandoned) %>%  group_by(wkr, party) %>% mutate(n = n()/max(iteration)) %>%
                summarise(abandon_p_party = mean(n) %>% round(2)),
              by = c("wkr", "party"), all.x = T) %>% 
  dplyr::mutate(abandon_p_party = case_when(is.na(abandon_p_party) ~ 0,
                         T ~ abandon_p_party)) 
saveRDS(test, file = "api/forecast_districts.rds")



pred_abandoned %>%  group_by(land, wkr, wkr_name) %>% mutate(n = n()/max(iteration)) %>%
  summarise(abandon_p = mean(abandoned) %>% round(2)) %>% arrange(-abandon_p) %>% 
  saveRDS(file = "api/pred_vacant.rds")

# This can be merged with a ranked list of candidate results to determine which districts will not have a representative



