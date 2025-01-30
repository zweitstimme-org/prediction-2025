### ----------------------------------------------------------
### Generate Descriptive Statistics and Tables
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------

### 1. Load Required Data ---------------------------------

# Load district forecasts
forecast_districts <- readRDS(str_c("output/prediction_data_districts.rds"))

### 2. Generate District Forecast Table -------------------

# Table B.3: Predicted Candidate-vote shares (Erststimme)
forecast_districts %>% 
  # Select relevant columns and filter parties
  dplyr::select(wkr, wkr_name, party, value) %>% 
  filter(party != "oth") %>% 
  # Pivot to wide format
  pivot_wider(names_from = party, values_from = value) %>% 
  # Truncate long district names
  mutate(
    str_len_wkr_name = str_length(wkr_name),
    wkr_name = ifelse(str_len_wkr_name > 10, 
                     str_c(substr(wkr_name, 1, 10), "..."), 
                     wkr_name)
  ) %>% 
  dplyr::select(-str_len_wkr_name) %>%
  # Generate LaTeX table
  stargazer(
    summary = F, 
    type = "latex", 
    out = "output/tables/forecast_districts.tex", 
    rownames = F
  )

### 3. Generate Vacant Seats Table ------------------------

# Load vacant seat probabilities
vacant_seats <- readRDS("output/vacant_seats.rds")

# Table C.4: Predicted vacant districts by party
vacant_seats %>% 
  filter(abandoned) %>%  
  # Calculate probabilities by district
  group_by(land, wkr, wkr_name, party) %>% 
  mutate(n = n()/max(iteration)) %>%
  summarise(
    abandon_p = mean(n) %>% round(2),
    value_mean = mean(pred) %>% round(2)
  ) %>% 
  # Truncate long district names
  mutate(
    str_len_wkr_name = str_length(wkr_name),
    wkr_name = ifelse(str_len_wkr_name > 10, 
                     str_c(substr(wkr_name, 1, 10), "..."), 
                     wkr_name)
  ) %>% 
  dplyr::select(-str_len_wkr_name) %>%
  # Filter and sort results
  arrange(-abandon_p) %>% 
  filter(abandon_p > 0.05) %>%
  # Generate LaTeX table
  stargazer(
    summary = F, 
    type = "latex", 
    out = "output/tables/vacant_districts.tex", 
    rownames = F
  )

### 4. Process Federal Poll Accuracy ----------------------

# Load and filter federal polls
federal_polls <- read_csv("data/germany-federal-polls.csv") %>%
  # Filter relevant parties
  filter(party %in% c("cdu", "spd", "afd", "gru", "lin", "bsw", "fdp")) %>%
  # Keep only polls in the week before election
  filter(date >= electiondate - 7 & date < electiondate) %>%
  # Calculate difference from actual results
  mutate(diff = poll_share - vote_share)

# Calculate accuracy metrics by pollster
federal_polls_pollster <- federal_polls %>%
  group_by(electiondate, auftraggeber) %>%
  summarise(
    mse = mean((poll_share - vote_share)^2, na.rm = T),
    rmse = sqrt(mean((poll_share - vote_share)^2, na.rm = T))
  )

# Calculate overall accuracy metrics
federal_polls <- federal_polls %>%
  group_by(electiondate) %>%
  summarise(
    mse = mean((poll_share - vote_share)^2, na.rm = T),
    rmse = sqrt(mean((poll_share - vote_share)^2, na.rm = T))
  )

### 5. Generate Federal RMSE Plot ------------------------

# Calculate mean RMSE
mean_rmse <- mean(federal_polls$rmse, na.rm = TRUE)

# Define election years
election_years <- seq(1998, 2021, by = 4)

# Figure D.1: RMSE between last week poll mean and election results
ggplot(federal_polls, aes(x = electiondate, y = rmse)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = mean_rmse, linetype = "dashed", color = "grey") +
  scale_x_date(
    breaks = as.Date(paste0(election_years, "-09-01")), 
    labels = as.character(election_years)
  ) +
  labs(
    title = "RMSE over time for federal elections",
    x = "Election date",
    y = "RMSE"
  ) +
  theme_minimal()

# Save plot
dir.create("plots", showWarnings = FALSE)
ggsave("output/plots/federal-rmse.pdf")

### 6. Process State Poll Accuracy -----------------------

# Load state data
state_polls <- read_csv("data/germany-state-polls.csv")
state_election_results <- read_csv("data/germany-state-elections.csv")

# Add next election dates to polls
state_polls <- state_polls %>%
  mutate(poll_date = as.Date(date))

# Find next election date for each poll
next_election_dates <- vector("list", length = nrow(state_polls))
for (i in 1:nrow(state_polls)) {
  next_election_dates[[i]] <- state_election_results %>%
    filter(
      land == state_polls$land[i], 
      electiondate > state_polls$poll_date[i]
    ) %>%
    summarise(next_election = min(electiondate)) %>%
    pull(next_election)
}

# Process state polls
state_polls <- state_polls %>%
  # Add election dates
  mutate(electiondate = as.Date(unlist(next_election_dates))) %>%
  # Merge with election results
  left_join(state_election_results, 
            by = c("land", "party", "electiondate")) %>%
  # Filter relevant parties and timeframe
  filter(
    party %in% c("cdu", "spd", "afd", "gru", "lin", "bsw", "fdp"),
    poll_date >= electiondate - 7 & poll_date < electiondate
  ) %>%
  # Calculate accuracy metrics
  group_by(electiondate) %>%
  summarise(
    mse = mean((poll_share - vote_share)^2, na.rm = T),
    rmse = sqrt(mean((poll_share - vote_share)^2, na.rm = T))
  )

### 7. Generate State RMSE Plot -------------------------

# Calculate mean RMSE
mean_rmse <- mean(state_polls$rmse, na.rm = TRUE)

# Figure D.2: RMSE between last week poll mean and election results
ggplot(state_polls, aes(x = electiondate, y = rmse)) +
  geom_hline(yintercept = mean_rmse, linetype = "dashed", color = "grey") +
  geom_line() +
  geom_point() +
  labs(
    title = "RMSE over time for state elections",
    x = "Election date",
    y = "RMSE"
  ) +
  theme_minimal()

# Save plot
ggsave("output/plots/state-rmse.pdf")
