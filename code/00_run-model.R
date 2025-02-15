setwd("/home/cerfort/prediction-2025")


start_time <- Sys.time()

# Load required packages and functions
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions


# Specifications
upcoming_election <- 2025
cutoff <-  Sys.Date() # Date of the last poll
election_date <- as.Date("2025-02-23")
past_election_date <- as.Date("2021-09-26")
days_in_model <- 365*2

nsim <- 1000  # Number of simulations


# Sampler Parameters
nIter <- 1000
nChains <- 20


# Get new polls
message("Checking for new polls.")

new_wahlrecht_polls <- get_wahlrecht_polls() %>% 
  # Remove GMS, because BSW is not working through the package
  filter(institute != "gms")

# Get date of last run
(last_run <- max(list.files("/mnt/forecasts/prediction-2025/draws") %>% str_subset("res_brw_2025_") %>% str_extract(".{10}(?=\\.rds)") %>% ymd))

# Run again?
run_again <- F

# Get polls of last run, if file doesn't exist, run again anyway
if(file.exists(str_c("output/polls/wahlrecht_polls_", last_run,".RData"))) {
  load(str_c("output/polls/wahlrecht_polls_", last_run,".RData")) 
  # Is there a new poll?
  run_again <- (max(new_wahlrecht_polls$date, na.rm = T) > max(wahlrecht_polls$date, na.rm = T))
} else run_again <- T

# If for some reason, this is NA, try running again anyway
if(is.na(run_again)) run_again <- T

# Save the new polls
wahlrecht_polls <- new_wahlrecht_polls
save(wahlrecht_polls, file = str_c("output/polls/wahlrecht_polls_", Sys.Date(),".RData"))


run_again <- T

if(run_again) {
  
  message("Running all code.")

  # Only run once
  # source("code/01_prepare-data.R")
  # source("code/02_ger_structural_pre_train_stan.R")
  
  # Run the model file
  source("code/03_ger_combined_model_stan.R")
  
  # Run the data and plots
  source("code/04_party-vote-data.R")
  source("code/05_party-vote-figures.R")
  source("code/06_district-model_v2.R")
  source("code/07_vacant-seats.R")
  source("code/08_district-figures.R")
  source("code/09_probabilities.R")
  source("code/10_forecast-trend.R")
  
  # Add timestamp to API
  saveRDS(Sys.time(), file = "api/last_updated.rds")
  
}

# Calculate time needed
end_time <- Sys.time()
message("Total time needed in hours:")
difftime(end_time, start_time, unit = "hours") %>% round(1) %>% message

