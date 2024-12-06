### ----------------------------------------------------------
### Election Polling Forecasting
### Implementation of Backward Random Walk for Multi-party Set-ups
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------



# Load required packages and functions
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions

### ----------------------------------------------------------
### 1. Set-Up and Pre-train Structural Model
### ----------------------------------------------------------


# Model and initial values
message("Loading stan code.")
model_file <- "model_code/combined_model_simple.stan"

message("Loading structural inits.")
structural_inits <- readRDS("data/2025_structural_inits_simple.rds")
initlist <- replicate(nChains, structural_inits, simplify = FALSE)


# Prepare poll data for model: Avoid zeros, and change to 2, and calculate others
wahlrecht_polls <- wahlrecht_polls %>% 
  mutate(lin = ifelse(!is.na(lin), lin, 2),
         bsw = ifelse(!is.na(bsw), bsw, 2),
         fdp = ifelse(!is.na(fdp), fdp, 2)) %>%
  mutate(lin = ifelse(lin == 0, 2, lin),
         bsw = ifelse(bsw == 0, 2, bsw),
         fdp = ifelse(fdp == 0, 2, fdp)) %>%
  # Make var oth which is 100 minus these vars cdu + spd + gru + lin + afd + fdp, but sometimes they are NA
  mutate(oth = 100 - cdu - spd - gru - afd - lin - bsw - fdp)



### ----------------------------------------------------------
### 2. Process Poll Data for Dynamic Model
### ----------------------------------------------------------


# run_again <- T # Remove this line to run the model only when their is a new poll

# If the latest poll is from yesterday, run the script again

  
  # Filter Polls 
  wahlrecht_polls <- filter(wahlrecht_polls, date <= cutoff)
  
  # Select polls within the desired time win
  polls <- wahlrecht_polls %>%
    filter(
      date > (election_date - days_in_model) & date <= cutoff,
      !apply(is.na(.), 1, any)
    )
  
  # Add necessary indices
  all_dates <- seq.Date((election_date - days_in_model), election_date, by = "1 day")
  polls$t <- match(polls$date, seq.Date((election_date-days_in_model), election_date, 1) )
  polls$iid <- as.numeric(factor(polls$institute))
  
  ### ----------------------------------------------------------
  ### 3. Prepare Data for Stan Model
  ### ----------------------------------------------------------
  
  # Load structural data
  data_structural <- readRDS("data/pre_train_data_25.rds") %>% 
    mutate(voteshare_l1 = case_when(election == 21 & party %in% c("lin", "bsw")  ~ 4.8700000/2, TRUE ~   voteshare_l1 )) %>%
    mutate(log_voteshare_l1 = log((ifelse(voteshare_l1 == 0, 0.01, voteshare_l1)/100) / (1-ifelse(voteshare_l1 == 0, 0.01, voteshare_l1)/100)),
             # log(ifelse(voteshare_l1==0,voteshare_l1+0.01,voteshare_l1)), 
           log_polls_200_230 = log((polls_200_230/100) / (1-polls_200_230/100))
             # log(ifelse(polls_200_230==0,polls_200_230+0.01,polls_200_230))
           ) 
  
  
  
  # Define predictors and dependent variable
  predictors <- c("log_voteshare_l1", "chancellor_party", "log_polls_200_230")
  dependent <- "voteshare"
  
  # Prepare matrices for Stan
  election_res <- as.matrix(data_structural[, dependent])
  election_pred <- as.matrix(data_structural[, predictors]) / 100
  party_names <- c("spd", "cdu", "gru", "fdp", "afd", "lin", "bsw", "oth") # data_structural$party[is.na(election_res)]
  nParties <- length(party_names)
  
  # Observed and missing election indices
  ii_obs <- which(complete.cases(election_res))
  ii_mis <- which(!complete.cases(election_res))
  
  # Predictors for the upcoming election
  election_pred_E <- data_structural %>%
    filter(election == 21) %>%
    select(all_of(predictors)) %>%
    as.matrix() / 100
  rownames(election_pred_E) <- data_structural$party[data_structural$election == 21]
  election_pred_E <- election_pred_E[party_names, ]
  
  # Prepare poll data
  Y <- round(as.matrix(polls[, party_names] / 100) * polls$sample_size)
  
  # Prepare Stan data list
  forstan <- list(
    # Dynamic Model
    y = Y,
    nParties = nParties,
    nPeriods = length(all_dates),
    nPolls = nrow(Y),
    iid = polls$iid,
    nInst = max(polls$iid),
    date = polls$t,
    
    # Fundamental Model
    LA = length(unique(data_structural$election)),
    L = length(unique(data_structural$election)) + 1,
    N = length(election_res),
    Nobs = sum(complete.cases(election_res)),
    Nmis = sum(!complete.cases(election_res)),
    v_obs = election_res[ii_obs] / 100,
    v = election_res / 100,
    x = election_pred,
    K = ncol(election_pred),
    election = data_structural$election,
    xE = election_pred_E,
    b_prior = c(structural_inits$b),
    b0_prior = structural_inits$b0,
    ii_obs = ii_obs,
    ii_mis = ii_mis,
    s = as.vector(table(data_structural$election))
  )
  
  ### ----------------------------------------------------------
  ### 4. Estimate Model and Generate Forecasts
  ### ----------------------------------------------------------
  
  # Estimate model
  cat("\nEstimating Model for Election", upcoming_election, "with a cutoff of", as.character(cutoff), "\n")
  # results <- stan(
  #   file = model_file,
  #   data = forstan,
  #   iter = nIter,
  #   chains = nChains,
  #   control = list(adapt_delta = 0.99, max_treedepth = 15)
  # )
  
  message("Saving the draws.")
  saveRDS(results, file = paste0("/mnt/forecasts/prediction-2025/draws/res_brw_", upcoming_election, "_", cutoff, "_", Sys.Date(), ".rds"))
  
  # Extract results
  res <- as.matrix(results)
  
  # Process forecast results
  draws_forecast_levels <- list()
  levels <- array(NA, dim = c(nIter / 2 * nChains, nParties, (days_in_model+1)))
  
  for (t in 1:(days_in_model+1)) {
    sel_levels_temp <- paste0("alpha[", t, ",", 1:nParties, "]")
    levels[, , t] <- res[, sel_levels_temp]
  }
  
  draws_forecast_levels[["levels"]] <- levels
  colnames(draws_forecast_levels[["levels"]]) <- party_names
  draws_forecast_levels[["forecast"]] <- res[, paste0("forecast[", 1:nParties, "]")]
  colnames(draws_forecast_levels[["forecast"]]) <- party_names
  draws_forecast_levels[["party_names"]] <- party_names
  draws_forecast_levels[["polls"]] <- polls
  
  boxplot(draws_forecast_levels$forecast)
  # saveRDS(draws_forecast_levels, str_c("/mnt/forecasts/prediction-2025/zweitstimme/zweitstimme_output_", Sys.Date(),".rds"))
  
  # Generate summary statistics
  forecast <- draws_forecast_levels[["forecast"]]
  colnames(forecast) <- draws_forecast_levels[["party_names"]]
  
  # Output these draws to the API
  saveRDS(forecast, str_c("/mnt/forecasts/prediction-2025/forecast/forecast_draws_", Sys.Date(),".rds"))
  
  
  round(apply(forecast, 2, mean) * 100, 1)  # Mean forecast
  round(t(apply(forecast, 2, quantile, c(1 / 12, 11 / 12))) * 100, 1)  # Credible intervals
  

