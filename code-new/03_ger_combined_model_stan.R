### ----------------------------------------------------------
### Election Polling Forecasting - Combined Model
### Implementation of Backward Random Walk for Multi-party Set-ups
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Setup and Data Loading ----------------------------------

# Load model and initial values
model_file <- "model_code/combined_model_simple.stan"
structural_inits <- readRDS("output/2025_structural_inits_simple.rds")
initlist <- replicate(nChains, structural_inits, simplify = FALSE)

# Process poll data: Handle zeros and calculate others
wahlrecht_polls <- wahlrecht_polls %>% 
  # Replace NA values with 2
  mutate(across(c(lin, bsw, fdp), ~ifelse(is.na(.), 2, .))) %>%
  # Replace 0 values with 2
  mutate(across(c(lin, bsw, fdp), ~ifelse(. == 0, 2, .))) %>%
  # Calculate others as remainder
  mutate(oth = 100 - cdu - spd - gru - afd - lin - bsw - fdp)

### 2. Process Poll Data for Dynamic Model ---------------------

# Filter polls up to cutoff date
wahlrecht_polls <- filter(wahlrecht_polls, date <= cutoff)

# Select polls within the desired time window
polls <- wahlrecht_polls %>%
  filter(
    date > (election_date - days_in_model) & date <= cutoff,
    !apply(is.na(.), 1, any)
  )

# Add time indices
all_dates <- seq.Date((election_date - days_in_model), election_date, by = "1 day")
polls$t <- match(polls$date, all_dates)
polls$iid <- as.numeric(factor(polls$institute))

### 3. Prepare Data for Stan Model ----------------------------

# Load and process structural data
pre_train_data_25 <- readRDS("output/pre_train_data_25.rds") %>% 
  # Adjust Left party vote share split
  mutate(
    voteshare_l1 = case_when(
      election == 21 & party %in% c("lin", "bsw") ~ 4.87/2,
      TRUE ~ voteshare_l1
    )
  ) %>%
  # Calculate log ratios
  mutate(
    log_voteshare_l1 = log_ratio(voteshare_l1/100),
    log_polls_200_230 = log_ratio(polls_200_230/100)
  )

# Define model variables
predictors <- c("log_voteshare_l1", "chancellor_party", "log_polls_200_230")
dependent <- "voteshare"
party_names <- c("spd", "cdu", "gru", "fdp", "afd", "lin", "bsw", "oth")
nParties <- length(party_names)

# Prepare matrices for Stan
election_res <- as.matrix(pre_train_data_25[, dependent])
election_pred <- as.matrix(pre_train_data_25[, predictors]) / 100
ii_obs <- which(complete.cases(election_res))
ii_mis <- which(!complete.cases(election_res))

# Prepare predictors for upcoming election
election_pred_E <- pre_train_data_25 %>%
  filter(election == 21) %>%
  dplyr::select(all_of(predictors)) %>%
  as.matrix() / 100
rownames(election_pred_E) <- pre_train_data_25$party[pre_train_data_25$election == 21]
election_pred_E <- election_pred_E[party_names, ]

# Process poll data
Y <- round(as.matrix(polls[, party_names] / 100) * polls$sample_size)

### 4. Create Stan Data Object -------------------------------

forstan <- list(
  # Dynamic Model parameters
  y = Y,
  nParties = nParties,
  nPeriods = length(all_dates),
  nPolls = nrow(Y),
  iid = polls$iid,
  nInst = max(polls$iid),
  date = polls$t,
  
  # Fundamental Model parameters
  LA = length(unique(pre_train_data_25$election)),
  L = length(unique(pre_train_data_25$election)) + 1,
  N = length(election_res),
  Nobs = sum(complete.cases(election_res)),
  Nmis = sum(!complete.cases(election_res)),
  v_obs = election_res[ii_obs] / 100,
  v = election_res / 100,
  x = election_pred,
  K = ncol(election_pred),
  election = pre_train_data_25$election,
  xE = election_pred_E,
  b_prior = c(structural_inits$b),
  b0_prior = structural_inits$b0,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = as.vector(table(pre_train_data_25$election))
)

### 5. Estimate Model and Generate Forecasts ----------------

# Estimate model
cat("\nEstimating Model for Election", upcoming_election, 
    "with a cutoff of", as.character(cutoff), "\n")

results <- stan(
  file = model_file,
  data = forstan,
  iter = nIter,
  chains = nChains,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

# Save results
saveRDS(results, 
        file = paste0("output/res_brw_", upcoming_election, "_", 
                     as.Date(cutoff+1), ".rds"))

### 6. Process and Save Forecasts --------------------------

# Extract results
res <- as.matrix(results)

# Process forecast results
draws_forecast_levels <- list()
levels <- array(NA, dim = c(nIter / 2 * nChains, nParties, (days_in_model+1)))

for (t in 1:(days_in_model+1)) {
  sel_levels_temp <- paste0("alpha[", t, ",", 1:nParties, "]")
  levels[, , t] <- res[, sel_levels_temp]
}

# Store results
draws_forecast_levels[["levels"]] <- levels
colnames(draws_forecast_levels[["levels"]]) <- party_names
draws_forecast_levels[["forecast"]] <- res[, paste0("forecast[", 1:nParties, "]")]
colnames(draws_forecast_levels[["forecast"]]) <- party_names
draws_forecast_levels[["party_names"]] <- party_names
draws_forecast_levels[["polls"]] <- polls

# Save forecast draws
forecast <- draws_forecast_levels[["forecast"]]
colnames(forecast) <- draws_forecast_levels[["party_names"]]
saveRDS(forecast, str_c("output/forecast_draws_", as.Date(cutoff + 1), ".rds"))

# Display results
round(apply(forecast, 2, mean) * 100, 1)  # Mean forecast
round(t(apply(forecast, 2, quantile, c(1/12, 11/12))) * 100, 1)  # Credible intervals

