### ----------------------------------------------------------
### Calculate Vacant Seats in German Parliament
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

message("Calculating vacant seats...")

### 1. Configuration and Data Loading ------------------------

# Load required data
btw_candidates_1983_2025 <- read.csv("/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2025_full.csv", stringsAsFactors = FALSE)
prediction_data_districts <- readRDS("output/prediction_data_districts.rds")
district_reg_predictions <- readRDS("/mnt/forecasts/prediction-2025/temp/district_reg_predictions.rds")

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
    party = if_else(party == "and", "oth", party),
    party = if_else(party == "cdu" & land == "BY", "csu", party),
  ) %>% 
  dplyr::select(land, wkr, party, valid_Z, resp_Z) %>% 
  filter(!is.na(valid_Z))

# votes_est$party[votes_est$land == "BY" & votes_est$party == "cdu"] <- "csu"


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
  fdp = 0.1146, lin = 0.0489, gru = 0.1475, 
  csu = 0.0517, bsw = 0.0489, oth = 0.0875
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
  print(j)
  
  # Find parties to take into account
  
  # Draw from the district forecast
  district_draw <- cbind(pred = district_reg_predictions[, j], prediction_data_districts)
  district_draw$party[district_draw$party == "cdu" & district_draw$land == "BY"] <- "csu"
  district_draw <- district_draw %>% group_by(wkr) %>% mutate(draw_winner = (pred == max(pred)))
  
  # Get parties which win at least 4 districts
  (grundmandat <- table((district_draw %>% filter(draw_winner))$party) %>% as.data.frame %>% filter(Freq >= 3) %>% pull(Var1) %>% as.character())
  
  
  # Rows are rows from votes_est, cols are per draw
  votes_est$zs_pred <- zs_pred[, j]
  votes_est$pred_Z_abs <- round(votes_est$valid_Z * votes_est$zs_pred, 0) # For now take only second column
  
  # Get parties which win at least 4 districts
  # Get parties which win at least 4 districts
  
  # Get parties with at least 5% (or 3 districts? Take CDU?)
  
  full_draw <- draws[j, ]
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
  
  
  ##############################################
  # List votes per party and state (proportional swing?)
  ##############################################
  
  prediction_subset <- votes_est %>% group_by(party) %>% dplyr::select(party, pred_Z_abs) %>%
    summarise(pred_Z_abs = sum(pred_Z_abs)) %>% pivot_wider(names_from = "party", values_from = "pred_Z_abs") %>% as.matrix
  
  
  # For each party in
  
  seats_states <- data.frame()
  
  for (seats_party in names(seats)) {
    
    print(seats_party)
    
    # if(seats_party == "fdp") next
    
    # Number of votes
    prediction_subset[colnames(prediction_subset) == seats_party]
    
    # Number of seats
    seats[seats_party]
    
    # Divisor
    (this_divisor <- prediction_subset[colnames(prediction_subset) == seats_party] / seats[seats_party])
    
    party_est <- votes_est %>% group_by(land, party) %>% 
      summarise(pred_Z_abs = sum(pred_Z_abs)) %>% filter(!is.na(pred_Z_abs)) %>% 
      filter(party == seats_party) %>% 
      mutate(seats = pred_Z_abs / this_divisor,
             seats_round = round(pred_Z_abs / this_divisor, 0)) %>% suppressMessages()
    
    sum(party_est$seats_round)
    
    counter <- 0 # Stop if there is no solution
    
    while(sum(party_est$seats_round) != seats[seats_party] & counter < 10) {
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
      
      counter <- counter + 1
    }
    
    party_est$zs_draw <- draw[seats_party]
    party_est$divisor <- this_divisor
    
    party_est$seats[party_est$party == "csu"]
    
    # this_divisor
    sum(party_est$seats_round)
    
    seats_states <- bind_rows(seats_states, party_est)
    
  }
  
  # seats_allocated
  
  seats_states <- district_draw %>% # filter(winner == 1) %>% 
    group_by(land, party) %>%
    # Sort decreasing by value
    arrange(desc(draw_winner), desc(value)) %>% 
    # Create a rank var
    mutate(rank = row_number()) %>% 
    mutate(rank = case_when(!draw_winner ~ NA,
                            T ~ rank)) %>% 
    merge(., seats_states, by = c("land", "party"), all.x = T) %>%
    mutate(seats_round = case_when(is.na(seats_round) ~ 0,
                                   T ~ seats_round)) %>% 
    mutate(abandoned = (draw_winner) & (rank > seats_round)) %>% 
    arrange(party, land, rank) %>%
    dplyr::select(land, party, wkr, wkr_name, pred, rank, seats_round, abandoned, zs_draw, divisor, draw_winner)
  
  seats_states$iteration <- j
  
  
  vacant_seats <- bind_rows(vacant_seats, seats_states)
  
}




### 8. Save Results -------------------------------------

saveRDS(vacant_seats, "output/vacant_seats.rds")
# vacant_seats <- readRDS("output/vacant_seats.rds")

pred_vacant <- vacant_seats %>%  filter(draw_winner) %>% group_by(land, wkr, wkr_name) %>% mutate(n = n()/max(iteration)) %>%
  summarise(abandon_p = mean(abandoned) %>% round(2)) %>% arrange(-abandon_p)

saveRDS(pred_vacant, "output/pred_vacant.rds")

## Add to API

# Add party specific vacancy probability
prediction_data_districts <- merge(prediction_data_districts, vacant_seats  %>% filter(abandoned) %>%  group_by(wkr, party) %>% mutate(n = n()/max(iteration)) %>%
                summarise(abandon_p_party = mean(n) %>% round(2)),
              by = c("wkr", "party"), all.x = T) %>%
  dplyr::mutate(abandon_p_party = round(abandon_p_party*100, 0)) %>% 
  dplyr::mutate(abandon_p_party = case_when(is.na(abandon_p_party) ~ 0,
                                            T ~ abandon_p_party))

# Add postal codes
plz <- read_delim("data/PLZ_WK_Zweitstimme.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
plz <- dplyr::select(plz, c(wkr = wahlkreis_nr, plz)) %>% mutate(wkr = as.numeric(wkr))
  
# Group by wkr and concatenate with ", "
plz <- plz %>% group_by(wkr) %>% summarise(plz = paste(plz, collapse = ", ")) %>% as.data.frame

# Merge by wkr
prediction_data_districts <- merge(prediction_data_districts, plz, by = "wkr", all.x = T)


# Add candidate names
# prediction_data_districts <- "output/prediction_data_districts.rds" %>% readRDS()

btw25_bewerb_utf8 <- read_delim("/mnt/forecasts/prediction-2025/temp/btw25_bewerb_utf8.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                skip = 8)
btw25_bewerb_utf8$GruppennameKurz %>% unique

cand_names <- btw25_bewerb_utf8
cand_names$Vornamen <- cand_names$Rufname

# If Title is not NA add at beginning of Vornamen
cand_names$Vornamen <- case_when(!is.na(cand_names$Titel) ~ str_c(cand_names$Titel, " ", cand_names$Vornamen),
                                 T ~ cand_names$Vornamen)

# If Namenszusatz is not NA add at beginning of Nachname
cand_names$Nachname <- case_when(!is.na(cand_names$Namenszusatz) ~ str_c(cand_names$Namenszusatz, " ", cand_names$Nachname),
                                 T ~ cand_names$Nachname)


cand_names <- filter(cand_names, Gebietsart == "Wahlkreis") %>% 
  dplyr::select(c(wkr = Gebietsnummer, GruppennameKurz, Nachname, Vornamen)) # VorpGewaehlt

# Only keep SPD, CDU, CSU, GRÜNE, FDP, Die Linke, AfD, BSW
cand_names <- cand_names %>% 
  filter(GruppennameKurz %in% c("SPD", "CDU", "CSU", "GRÜNE", "GRÜNE/B 90", "FDP", "Die Linke", "AfD", "BSW"))

# Add party var where SPD = spd, CSU = cdu, CDU = cdu .. 
cand_names$party <- case_when(cand_names$GruppennameKurz == "SPD" ~ "spd",
                              cand_names$GruppennameKurz == "CDU" ~ "cdu",
                              cand_names$GruppennameKurz == "CSU" ~ "cdu",
                              cand_names$GruppennameKurz == "GRÜNE" ~ "gru",
                              cand_names$GruppennameKurz == "GRÜNE/B 90" ~ "gru",
                              cand_names$GruppennameKurz == "FDP" ~ "fdp",
                              cand_names$GruppennameKurz == "Die Linke" ~ "lin",
                              cand_names$GruppennameKurz == "AfD" ~ "afd",
                              cand_names$GruppennameKurz == "BSW" ~ "bsw")
cand_names$party %>% table

# Merge by wkr and party
prediction_data_districts <- merge(prediction_data_districts, cand_names, by = c("wkr", "party"), all.x = T)


# Save to API
saveRDS(prediction_data_districts, file = "api/prediction_data_districts.rds")
saveRDS(pred_vacant, "api/pred_vacant.rds")


