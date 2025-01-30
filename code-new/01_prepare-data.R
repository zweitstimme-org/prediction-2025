### ----------------------------------------------------------
### Election Polling Forecasting Script - Structural Model
### Authors: Lukas Stoetzer & Cornelius Erfort 
### ----------------------------------------------------------


### Load and prepare data ------------------------------------------

# Load existing data
load(str_c("output/wahlrecht_polls_", cutoff+1,".RData"))  # Poll data
pre_train_data_25 <- readRDS("data/pre_train_data_21.rds")    # Structural data

# Import hand-coded data for 2021 and 2025 elections
pre_train_data_25_2021 <- read.xlsx("output/pre_train_data_21.xlsx") %>% 
  filter(election == 20)
pre_train_data_25_2025 <- read.xlsx("output/pre_train_data_21.xlsx") %>% 
  filter(election == 21)

# Remove existing 2021 data from main dataset
pre_train_data_25 <- filter(pre_train_data_25, election != 20)

### Calculate polling averages ------------------------------------

# Calculate average poll shares (200-230 days before election)
polls_fund <- wahlrecht_polls %>% 
  pivot_longer(cols = c("cdu", "spd", "gru", "fdp", "lin", "afd", "bsw"), names_to = "party", values_to = "poll_share") %>% 
  filter(date >= election_date - 230 & date <= election_date - 200) %>% 
  group_by(party) %>% 
  summarise(poll_share = mean(poll_share, na.rm = TRUE))

### Update and combine datasets ----------------------------------

# Update 2025 data with calculated poll shares
pre_train_data_25_2025 <- pre_train_data_25_2025 %>% 
  left_join(polls_fund, by = "party") %>% 
  mutate(poll_share = ifelse(is.na(poll_share), 0, poll_share)) %>% 
  dplyr::select(-c(polls_200_230)) %>% 
  rename(polls_200_230 = poll_share)

# Combine all datasets
pre_train_data_25 <- bind_rows(pre_train_data_25, pre_train_data_25_2021) %>%
  bind_rows(pre_train_data_25_2025)

# Set BSW party's previous vote share to 0 (new party)
pre_train_data_25$voteshare_l1[pre_train_data_25$party == "bsw"] <- 0

# Sort final dataset
pre_train_data_25 <- pre_train_data_25 %>% arrange(election, party)

# Save results
saveRDS(pre_train_data_25, "output/pre_train_data_25.rds")
