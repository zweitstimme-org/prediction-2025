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

# Specifications

upcoming_election <- 2025 # What's the next election?

# Parameters for Sampler
nIter <- 3000
nChains <- 6


model_file <- "code/model_code/combined_model.stan"

structural_inits <- readRDS("data/ger/structural_inits/2021_structural_inits.RDS")
initlist <- replicate(nChains, structural_inits, simplify=FALSE)


#### Poll Data for Dynamic Model

# wahlrecht_polls <- get_wahlrecht_polls()





#### Data for Structural Model

# Load Data
# Load Data
data_structural <- readRDS("data/ger/Structural/pre_train_data_21.RDS")

dat <- data_structural

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

# Predictors for upcoming election
election_pred_E <- dat[dat$election==20,c(predictors)]
rownames(election_pred_E) <- dat[dat$election==20,"party"]
election_pred_E <- election_pred_E[party_names,]
election_pred_E[,c(1, 3)] <- election_pred_E[,c(1, 3)] / 100


cutoff <- Sys.Date()
election_date <- as.Date("2025-02-23")

wahlrecht_polls <- filter(wahlrecht_polls, date <= cutoff)

sel <- (wahlrecht_polls$date > (election_date-365)) & wahlrecht_polls$date <= cutoff & apply(wahlrecht_polls, 1, function(x) !any(is.na(x)))

polls <- wahlrecht_polls[sel,]


all_dates <- seq.Date((election_date-365), election_date, 1) 
polls$t <- match(wahlrecht_polls$date[sel], seq.Date((election_date-365), election_date, 1) )
polls$iid <- as.numeric(factor(polls$institute))
  

  # Prepare Data for Jags
  Y <- round(as.matrix(polls[,party_names] / 100) * 
               polls$sample_size) # Grep existing Parties and transform share to number
 
  
  
  
forstan <- list(
  
  # Dynamic Model
  y = Y,
  nParties = nParties,
  nPeriods =  length(all_dates), 
  nPolls = nrow(Y),
  iid = polls$iid,
  nInst = max(polls$iid),
  date = polls$t,
  
  # Fundamental Model
  LA = length(unique(dat$election)),
  L = length(unique(dat$election)) + 1,
  N = length(election_res), #Number of observations
  Nobs = sum(complete.cases(c(election_res / 100))),
  Nmis = sum(!complete.cases(c(election_res / 100))),
  v_obs = c(election_res / 100)[ii_obs],  # Dependent variable
  v = c(election_res / 100),  # Dependent variable
  x = election_pred, # Predictors past elections
  K = ncol(election_pred),   # Number of predictors
  election = dat$election,
  xE = as.matrix(election_pred_E), # Predictors for upcoming election
  b_prior = structural_inits$b, b0_prior = structural_inits$b0,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = as.vector(table(dat$election))
  
)

cat("\n Estimating Model for Election", 2021, "with a cutoff of ", cutoff, "\n") 

results <- stan(file = model_file, data = forstan,
                iter = nIter, chains = nChains, thin = 1, control = list(adapt_delta = 0.99, max_treedepth = 12) )


# saveRDS(results, file = paste0("output/ger/draws/combined_model/res_brw_", 2021,"_",cutoff,".RDS"))

res <- as.matrix(results)


draws_forecast_levels <- list() # Output Object

# Grep Levels, put in array and attach to list
levels <- array(NA,c(nIter / 2 * nChains, nParties, 366))

for(t in 1:366){
  sel_levels_temp <- paste0("alpha[", t, ",", 1:nParties, "]")
  levels[, , t] <- as.matrix(res[, sel_levels_temp])
}

draws_forecast_levels[["levels"]] <- levels


#Grep forcast and attach to list
sel_forecast <- paste0("forecast[", 1:nParties, "]")
draws_forecast_levels[["forecast"]] <- as.matrix(res[, sel_forecast])

# Attach partynames to list
draws_forecast_levels[["party_names"]] <- party_names

# Attach Polls used for estimation
draws_forecast_levels[["polls"]] <- polls

# saveRDS(draws_forecast_levels, 
#         file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS")
# )


df <- draws_forecast_levels


forecast <- df$forecast

colnames(forecast) <- df$party_names

adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), df$party_names)


forecast <- forecast[, adjustOrder]
#as.matrix(forecast)

names(attr(forecast, "dimnames")) <- NULL


#df$levels
# Mean Forecast
round(apply(forecast, 2, mean)*100, 1)

# 5/6 Credible Intervals
round(t(apply(forecast, 2, quantile, c(1/12, 11/12)))*100, 1)


party_names <- c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth")

adjustOrder <- match(party_names, df$party_names)


levels <- df$levels[,adjustOrder,]


lev_list <- lapply(seq(dim(levels)[3]), function(x) levels[ , , x])

names(lev_list) <- seq.Date(from = as.Date("2021-09-26")-365, to = as.Date("2021-09-26"), by = 1)

lev_list <- lapply(lev_list, function(x){ colnames(x) <- party_names
x})

date_df <- lapply(lev_list, function(x) rbind(mean=colMeans(x), apply(x, 2, quantile, c(1/12, 11/12))))




zweitstimme_output <- list(forecast = data.frame(forecast), poll_aggregator = lapply(lev_list, function(x) data.frame(x)), polls = data.frame(df$polls), timestamp = Sys.time())
saveRDS(zweitstimme_output, "~/zweitstimme/zweitstimme_output.RDS")
