### ----------------------------------------------------------
### Election polling forecasting 
### implementation of backward random walk for multi-party set-ups
### 
### Lukas Stoetzer & Marcel Neunhoeffer
### 
### 1. Set-up and Estimate Structural Model
###
### ----------------------------------------------------------

source("code/R/auxiliary/packages.r")  # Load required packages
source("code/R/auxiliary/functions.r") # Load additional functions


### 0. Pre-train Structural Model

# This has only to be done once per election. The structural pre-train will be 
# stored and accessed for the updates of the combined model.

# Specifications

upcoming_election <- 2021 # What's the next election?

# Parameters for Sampler
nIter <- 1000
nChains <- 6


model_file <- "code/model_code/structural_pre_train.stan"




### Data for Structural Model

# Load Data
data_structural <- readRDS("data/ger/Structural/pre_train_data_21.RDS")


# Predictors
predictors <- c("voteshare_l1", "chancellor_party", "polls_200_230")
dependent<- "voteshare"

# Election results in a n x 1 matrix
election_res <- as.matrix(data_structural[, dependent])

# Predictor for past elections in a n x k matrix
election_pred <- as.matrix(data_structural[, predictors])
election_pred[,c(1, 3)] <- election_pred[,c(1, 3)] / 100

party_names <- data_structural$party[is.na(election_res)]
nParties <- length(party_names) # Number of parties in upcoming election

nParties_vec <- as.vector(table(data_structural$election)) # Number of parties in all elections

ii_obs <- which(complete.cases(c(election_res / 100))) # Indicator for observed elections for stan
ii_mis <- which(!complete.cases(c(election_res / 100))) # Indicator for missing elections

# Data in list for stan
forstan <- list(
  
  # Fundamental Model
  LA = length(unique(data_structural$election)),
  L = length(unique(data_structural$election)) + 1,# Number of elections
  N = length(election_res), #Number of observations
  Nobs = sum(complete.cases(c(election_res / 100))),
  Nmis = sum(!complete.cases(c(election_res / 100))),
  y_obs = c(election_res / 100)[ii_obs],  # Dependent variable
  x = election_pred, # Predictors past elections
  K = ncol(election_pred),   # Number of predictors
  election = data_structural$election,
  nParties = nParties,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = nParties_vec
)


results <- stan(file = model_file, data = forstan,
            iter = nIter, chains = nChains, thin = 1, control = list(adapt_delta = 0.99, max_treedepth = 15))


saveRDS(results, file = paste0("output/ger/draws/structural_pre_train/", 2021, "_structural_pre_train_stan.RDS"))

res <- as.matrix(results)


jags_matrix <- as.matrix(res)


structural_forecast <- jags_matrix[,grepl("y_mis\\[",colnames(jags_matrix))]
colnames(structural_forecast) <- data_structural$party[!complete.cases(data_structural$voteshare)]

saveRDS(structural_forecast, file = paste0("output/ger/forecasts/structural/", 2021, "_structural_forecast.RDS"))

jags_summary_df <- jags_summary(jags_matrix)

b_mean <- filter(jags_summary_df, str_detect(var, "^b\\[")) %>% magrittr::extract2("mean")

b_mean <- matrix(b_mean, ncol = 3)


b_0_mean <- filter(jags_summary_df, str_detect(var, "^b0\\[")) %>% magrittr::extract2("mean")


structural_inits <- list(b = b_mean, b0 = b_0_mean)

saveRDS(structural_inits, paste0("data/ger/structural_inits/", 2021,"_structural_inits.RDS"))

