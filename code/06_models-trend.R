
setwd("/home/cerfort/prediction-2025")

# Load required packages and functions
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions

# Run once

# draws_files <- list.files("/mnt/forecasts/prediction-2025/draws", full.names = T) %>% 
#   str_subset("res_brw_2025_") # %>% str_extract(".{10}(?=\\.rds)") %>% ymd
# # Dates (not cutoff)
# draws_files %>% str_extract(".{10}(?=\\.rds)") %>% ymd




# Make object cutoff that is the weekly dates before 2024-12-04, going back for 10 weeks
cutoffs <- seq.Date(as.Date("2024-11-17"), by = "-1 week", length.out = 7) %>% ymd

# Specifications
upcoming_election <- 2025
election_date <- as.Date("2025-02-23")
past_election_date <- as.Date("2021-09-26")
days_in_model <- 365*2

# Sampler Parameters
nIter <- 1000
nChains <- 20

# Load polls
load(str_c("output/polls/wahlrecht_polls_2024-12-16.RData"))


for (cutoff in cutoffs) {
  start_time <- Sys.time()
  message("Running model iteration.")
  message(cutoff)
  # Run the model file
  source("code/03_ger_combined_model_stan.R")

  # Calculate time needed
  end_time <- Sys.time()
  message("Total time needed in hours:")
  difftime(end_time, start_time, unit = "hours") %>% round(1) %>% message
}

# # Define the function to run for each cutoff
# run_model <- function(cutoff, worker_id) {
#   log_file <- paste0("/home/cerfort/prediction-2025/logs/model_trend_worker_", worker_id, ".log")
#   
#   log_message <- function(msg) {
#     cat(paste0(Sys.time(), " - ", msg, "\n"), file = log_file, append = TRUE)
#   }
#   
#   start_time <- Sys.time()
#   log_message(paste("Running model iteration for cutoff:", cutoff))
#   
#   # Assign the cutoff to a global variable if needed
#   # assign("cutoff", cutoff, envir = .GlobalEnv)
#   
#   # Run the model file
#   source("code/03_ger_combined_model_stan.R", local = T)
#   
#   # Calculate time needed
#   end_time <- Sys.time()
#   elapsed_time <- round(difftime(end_time, start_time, units = "hours"), 1)
#   log_message(paste("Total time needed in hours:", elapsed_time))
#   
#   return(list(cutoff = cutoff, time = elapsed_time))
# }
# 
# 
# 
# # Set up parallel cluster
# n_cores <- detectCores() - 1  # Use one less core than available
# cl <- makeCluster(n_cores)
# 
# # Export necessary variables, libraries, and functions to the cluster
# # Add any global variables or libraries needed by the model
# clusterExport(cl, c(
#   "cutoffs", 
#   "upcoming_election", "election_date", "past_election_date", 
#   "days_in_model", "nIter", "nChains", 
#   "run_model", "wahlrecht_polls"  # The main function
# ))
# clusterEvalQ(cl, {
#   library(dplyr)  # Add any libraries used in the script
#   library(stringr)
#   library(lubridate)
#   setwd("/home/cerfort/prediction-2025")  # Set working directory on workers
#   source("auxiliary/packages.r")  # Load packages
#   source("auxiliary/functions.r") # Load functions
# })
# 
# # Pass the worker ID to each worker
# results <- parLapply(cl, seq_along(cutoffs), function(worker_id) {
#   run_model(cutoffs[worker_id], worker_id)
# })
# 
# # Run the model in parallel
# results <- parLapply(cl, cutoffs, run_model)
# 
# 
# # Stop the cluster
# stopCluster(cl)

# View results
# print(results)