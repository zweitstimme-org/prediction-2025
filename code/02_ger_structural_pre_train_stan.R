### ----------------------------------------------------------
### Election Polling Forecasting Script - Structural Model
### Authors: Lukas Stoetzer & Cornelius Erfort 
### ----------------------------------------------------------

#' Pre-train Structural Model
#' This process is performed once per election. The structural pre-train is stored
#' for updating the combined model.

### 1. Setup and Data Loading ------------------------------------

# Stan model configuration
model_file <- "model_code/structural_pre_train_simple.stan"  # Simplified Dirichlet model
data_structural <- readRDS("output/pre_train_data_25.rds")

# Transform vote shares for new parties (BSW/Left party split)
data_structural <- data_structural %>%
  mutate(
    voteshare_l1 = case_when(
      election == 21 & party %in% c("lin", "bsw") ~ 4.87 / 2,
      TRUE ~ voteshare_l1
    ),
    # Calculate log ratios for relevant variables
    lr_voteshare = log_ratio(voteshare),
    lr_voteshare_l1 = log_ratio(voteshare_l1),
    lr_polls_200_230 = log_ratio(polls_200_230)
  )

### 2. Prepare Stan Model Data ----------------------------------

# Define model variables
predictors <- c("lr_voteshare_l1", "chancellor_party", "lr_polls_200_230")
dependent <- "voteshare"

# Create matrices for model
election_res <- as.matrix(data_structural[, dependent])
election_pred <- as.matrix(data_structural[, predictors])
rownames(election_pred) <- NULL
election_pred[, c(1, 3)] <- election_pred[, c(1, 3)] / 100

# Prepare indices and metadata for Stan
party_names <- data_structural$party[is.na(election_res)]
nParties <- length(party_names)
nParties_vec <- as.vector(table(data_structural$election))
ii_obs <- which(complete.cases(election_res / 100))
ii_mis <- which(!complete.cases(election_res / 100))

# Create Stan data object
forstan <- list(
  LA = length(unique(data_structural$election)),
  L = length(unique(data_structural$election)) + 1,
  N = length(election_res),
  Nobs = sum(complete.cases(election_res / 100)),
  Nmis = sum(!complete.cases(election_res / 100)),
  y_obs = c(election_res / 100)[ii_obs],
  x = election_pred,
  K = ncol(election_pred),
  election = data_structural$election,
  nParties = nParties,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = nParties_vec
)

### 3. Estimate Model and Save Results --------------------------

# Run Stan model
results <- stan(
  file = model_file,
  data = forstan,
  iter = nIter,
  chains = nChains
)

# Extract and process forecasts
res <- as.matrix(results)
structural_forecast <- res[, grepl("y_mis\\[", colnames(res))]
colnames(structural_forecast) <- data_structural$party[!complete.cases(data_structural$voteshare)]

# Save initial values for priors
jags_summary_df <- jags_summary(res)
b_mean <- jags_summary_df %>%
  filter(str_detect(var, "^b\\[")) %>%
  pull(mean) %>%
  as.matrix(ncol = 3)

b_0_mean <- jags_summary_df %>%
  filter(str_detect(var, "b0")) %>%
  pull(mean)

structural_inits <- list(b = b_mean, b0 = b_0_mean)
saveRDS(structural_inits,
        file = paste0("output/", upcoming_election, "_structural_inits_simple.rds"))
