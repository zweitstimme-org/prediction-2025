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

# Specifications
upcoming_election <- 2025
cutoff <- Sys.Date()
election_date <- as.Date("2025-02-23")

# Sampler Parameters
nIter <- 4000
nChains <- 5 

# Model and initial values
model_file <- "model_code/combined_model_simple.stan"
structural_inits <- readRDS("data/structural_inits/2025_structural_inits_simple.RDS")
initlist <- replicate(nChains, structural_inits, simplify = FALSE)

### ----------------------------------------------------------
### 2. Process Poll Data for Dynamic Model
### ----------------------------------------------------------

# Load and process polls
down <- get_surveys()

wahlrecht_polls <- down %>%
  unnest(surveys) %>%
  unnest(survey) %>%
  select(institut = pollster, date, party, poll_share = percent, sample_size = respondents) %>%
  mutate(
    date = ymd(date),
    party = case_when(
      party == "greens" ~ "gru",
      party == "left" ~ "lin",
      party == "others" ~ "oth",
      TRUE ~ party
    )
  ) %>%
  pivot_wider(names_from = party, values_from = poll_share) %>%
  mutate(
    oth = 100 - cdu - spd - gru - afd,
    oth = ifelse(!is.na(lin), oth - lin, oth),
    oth = ifelse(!is.na(bsw), oth - bsw, oth),
    oth = ifelse(!is.na(fdp), oth - fdp, oth)
  ) %>%
  mutate(across(c(lin, bsw, fdp), ~ replace_na(., 0))) # Replace NA values with 0 for specific columns

# Save processed polls
save(wahlrecht_polls, file = "data/wahlrecht_polls.RData")


# Filter Polls 
wahlrecht_polls <- filter(wahlrecht_polls, date <= cutoff)

# Select polls within the desired time window
polls <- wahlrecht_polls %>%
  filter(
    date > (election_date - 365) & date <= cutoff,
    !apply(is.na(.), 1, any)
  )

# Add necessary indices
all_dates <- seq.Date((election_date - 365), election_date, by = "1 day")
polls$t <- match(polls$date, seq.Date((election_date-365), election_date, 1) )
polls$iid <- as.numeric(factor(polls$institut))

### ----------------------------------------------------------
### 3. Prepare Data for Stan Model
### ----------------------------------------------------------

# Load structural data
data_structural <- readRDS("data/structural/pre_train_data_25.RDS") %>% 
  mutate(voteshare_l1 = case_when(election == 21 & party %in% c("lin", "bsw")  ~ 4.8700000/2, TRUE ~   voteshare_l1 )) %>%
  mutate(log_voteshare_l1 = log(ifelse(voteshare_l1==0,voteshare_l1+0.01,voteshare_l1)), 
         log_polls_200_230 = log(ifelse(polls_200_230==0,polls_200_230+0.01,polls_200_230)))

# Define predictors and dependent variable
predictors <- c("log_voteshare_l1", "chancellor_party", "log_polls_200_230")
dependent <- "voteshare"

# Prepare matrices for Stan
election_res <- as.matrix(data_structural[, dependent])
election_pred <- as.matrix(data_structural[, predictors]) / 100
party_names <- data_structural$party[is.na(election_res)]
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
results <- stan(
  file = model_file,
  data = forstan,
  iter = nIter,
  chains = nChains,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

saveRDS(results, file = paste0("output/draws/combined_model/res_brw_", upcoming_election, "_", cutoff, ".RDS"))

# Extract results
res <- as.matrix(results)

# Process forecast results
draws_forecast_levels <- list()
levels <- array(NA, dim = c(nIter / 2 * nChains, nParties, 366))

for (t in 1:366) {
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
saveRDS(draws_forecast_levels, "output/zweitstimme_output.RDS")

# Generate summary statistics
forecast <- draws_forecast_levels[["forecast"]]
colnames(forecast) <- draws_forecast_levels[["party_names"]]

round(apply(forecast, 2, mean) * 100, 1)  # Mean forecast
round(t(apply(forecast, 2, quantile, c(1 / 12, 11 / 12))) * 100, 1)  # Credible intervals
