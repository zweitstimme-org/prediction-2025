### ----------------------------------------------------------
### Election Polling Forecasting Script
### Strutural Model
### Authors: Lukas Stoetzer & Cornelius Erfort 
### ----------------------------------------------------------

# Load necessary packages and functions
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions

### ----------------------------------------------------------
### 0. Pre-train Structural Model
### ----------------------------------------------------------

# This process is performed once per election. The structural pre-train is stored
# for updating the combined model.

# Election specifications
upcoming_election <- 2025  # Next election year
election_date <- as.Date("2025-02-23")  # Election date

# Sampler parameters
nIter <- 2000
nChains <- 10

# Stan model file
model_file <- "model_code/structural_pre_train_simple.stan"

# Model is a simplified Dirichlet model (without dynamic component on the parameters).

### ----------------------------------------------------------
### 1. Load Structural Model Data
### ----------------------------------------------------------


# We should create a script to prepare this RDS File for the 25 election
# Load structural data
#data_structural25 <- readRDS("data/pre_train_data_25.rds")
#data_structural21 <- readRDS("data/pre_train_data_21.rds")
# 
#data_structural21[1:10,]
#data_structural25[1:10,]

data_structural <- readRDS("data/pre_train_data_25.rds")


# Define a function to calculate the log ratio
log_ratio <- function(x) {
  x <- x / 100                      # Scale the value by dividing by 100
  x <- ifelse(x == 0, x + 0.01, x)  # Add 0.02 if the value is 0
  log(x/(1-x))                            # Calculate the logarithm
}

# Apply the function to your data
data_structural <- data_structural %>%
  mutate(
    voteshare_l1 = case_when(
      election == 21 & party %in% c("lin", "bsw") ~ 4.8700000 / 2,
      TRUE ~ voteshare_l1
    ),
    lr_voteshare = log_ratio(voteshare),     # Apply log_ratio to voteshare_l1
    lr_voteshare_l1 = log_ratio(voteshare_l1),     # Apply log_ratio to voteshare_l1
    lr_polls_200_230 = log_ratio(polls_200_230)   # Apply log_ratio to polls_200_230
  )


# For comparsion
m <- lm(voteshare ~ voteshare_l1 + chancellor_party + polls_200_230, data_structural)
summary(m)
predict(m, newdata = filter(data_structural, election == 21))

m_lr <- lm(lr_voteshare ~ lr_voteshare_l1 + chancellor_party + lr_polls_200_230, data_structural)
pred_lr <- predict(m_lr, newdata = filter(data_structural, election == 21))
summary(m_lr)


### ----------------------------------------------------------
### 2. Prepare Data for Stan Model
### ----------------------------------------------------------

# Define predictors and dependent variable
# We need to work with the log transformed voteshares if we want a proportional relationship
predictors <- c("lr_voteshare_l1", "chancellor_party", "lr_polls_200_230")
dependent <- "voteshare"

# Create matrices for predictors and dependent variable
election_res <- as.matrix(data_structural[, dependent])
election_pred <- as.matrix(data_structural[, predictors])
rownames(election_pred) <- NULL
election_pred[, c(1, 3)] <- election_pred[, c(1, 3)] / 100

# Stan data preparation
party_names <- data_structural$party[is.na(election_res)]
nParties <- length(party_names)
nParties_vec <- as.vector(table(data_structural$election))
ii_obs <- which(complete.cases(election_res / 100))
ii_mis <- which(!complete.cases(election_res / 100))

# Put into Stan Object
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


### ----------------------------------------------------------
### 3. Estimate Model with Stan
### ----------------------------------------------------------

results <- stan(
  file = model_file,
  data = forstan,
  iter = nIter,
  chains = nChains,
  #control = list(adapt_delta = 0.99, max_treedepth = 15)
)

#saveRDS(results, 
#        file = paste0("output/draws/", upcoming_election, "_structural_pre_train_stan_simple.RDS"))

### ----------------------------------------------------------
### 4. Generate Forecasts and Save Results
### ----------------------------------------------------------

# Extract forecasts
res <- as.matrix(results)
structural_forecast <- res[, grepl("y_mis\\[", colnames(res))]
colnames(structural_forecast) <- data_structural$party[!complete.cases(data_structural$voteshare)]

saveRDS(structural_forecast, 
        file = paste0("output/forecasts/", upcoming_election, "_structural_forecast_simple.RDS"))

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
saveRDS(structural_inits, file = paste0("data/", upcoming_election, "_structural_inits_simple.RDS"))
