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

upcoming_election <- 2025 # What's the next election?
election_date <- as.Date("2025-02-23")

# Parameters for Sampler
nIter <- 1000
nChains <- 6


model_file <- "code/model_code/structural_pre_train.stan"

# Get Poll Data
# federal_polls <- read_csv("data/federal-polls.csv")
# wahlrecht_polls <- federal_polls %>% rename(institute = auftraggeber) %>% select(date, party, poll_share, institute) %>% 
#   filter(!(party %in% c("pir", "fw", "npd", "bsw"))) %>%
#   # pivot_wider values from poll_share, names from party, if duplicate dates, make two rows
#   
#   # If lin is NA, set to lag
#   mutate(lin = ifelse(is.na(lin), lag(lin), lin)) %>%
#   # If afd is NA, set to lag
#   mutate(afd = ifelse(is.na(afd), lag(afd), afd)) %>%
#   mutate(oth = 100 - cdu - spd - afd - gru - lin - fdp) %>% 
#   # Add fake sample size
#   mutate(sample_size = 1000)

down <- get_surveys()

# sample_size
wahlrecht_polls <- down %>% 
  unnest(surveys) %>% 
  unnest(survey) %>% 
  select(institut = pollster, date, party, poll_share = percent, sample_size = respondents) %>%
  mutate(date = ymd(date),
         party = case_when(party == "greens" ~ "gru",
                           party == "left" ~ "lin",
                           party == "others" ~ "oth",
                           TRUE ~ party)) %>%     
  pivot_wider(names_from = party, values_from = poll_share) %>%
  pivot_longer(cols = cdu:lin, names_to = "party", values_to = "poll_share") %>% 
  rename(institute = institut) %>% 
  pivot_wider(names_from = party, values_from = poll_share, values_fn = mean) %>%
  # Make var oth which is 100 minus these vars cdu + spd + gru + lin + afd + fdp, but sometimes they are NA
  mutate(oth = 100 - cdu - spd - gru - afd)

# If lin is not NA, subtract form oth
wahlrecht_polls <- wahlrecht_polls %>% 
  mutate(oth = ifelse(!is.na(lin), oth - lin, oth),
         oth = ifelse(!is.na(bsw), oth - bsw, oth),
         oth = ifelse(!is.na(fdp), oth - fdp, oth))
  
save(wahlrecht_polls, file = "data/wahlrecht_polls.RData")



### Data for Structural Model

# Load Data
data_structural <- readRDS("data/ger/Structural/pre_train_data_21.RDS")

data_structural_2021 <- filter(data_structural, election == 20)
# write.xlsx(data_structural_2021, "data/ger/Structural/pre_train_data_21.xlsx")
btw_2021_kerg2 <- read_csv("data/btw_2021_kerg2.csv")

btw_2021_kerg2 %>% filter(Gruppenart == "Partei" & Stimme == 2 & Gebietsart == "Bund") %>% select(Gruppenname, Prozent) %>% 
  # Rename Gruppenname CSU to CDU
  mutate(Gruppenname = ifelse(Gruppenname == "CSU", "CDU", Gruppenname),
         Prozent = Prozent %>% str_replace("\\,", ".") %>% as.numeric) %>%
  # Aggreate by Gruppenname
  group_by(Gruppenname) %>% summarise(Prozent = sum(Prozent) %>% round(2)) %>% 
  # Order by Prozent decreasing
  arrange(desc(Prozent)) %>% View

# Read xlsx back in
data_structural_2021 <- read.xlsx("data/ger/Structural/pre_train_data_21.xlsx")

# Add back to df
data_structural <- filter(data_structural, election != 20)
data_structural <- bind_rows(data_structural, data_structural_2021)


# Get the average for each party (columns cdu, spd, gru, lin, afd, fdp, bsw) from 200 to 230 days before election_date
polls_fund <- wahlrecht_polls %>% pivot_longer(cols = cdu:lin, names_to = "party", values_to = "poll_share") %>% 
  filter(date >= election_date - 230 & date <= election_date - 200) %>% 
  group_by(party) %>% summarise(poll_share = mean(poll_share, na.rm = TRUE))

# Replace the NA values in the data_structural with these values
data_structural <- data_structural %>% left_join(polls_fund, by = "party") %>% 
  mutate(poll_share = ifelse(is.na(poll_share), 0, poll_share)) %>% 
  select(-c(polls_200_230)) %>% 
  rename(polls_200_230 = poll_share)

saveRDS(data_structural, "data/ger/Structural/pre_train_data_25.RDS")
data_structural <- readRDS("data/ger/Structural/pre_train_data_25.RDS")

# Set past vote share of BSW to 0
data_structural$voteshare_l1[data_structural$party == "bsw"] <- 0


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


saveRDS(results, file = paste0("data/ger/draws/structural_pre_train/", upcoming_election, "_structural_pre_train_stan.RDS"))

res <- as.matrix(results)


jags_matrix <- as.matrix(res)


structural_forecast <- jags_matrix[,grepl("y_mis\\[",colnames(jags_matrix))]
colnames(structural_forecast) <- data_structural$party[!complete.cases(data_structural$voteshare)]

saveRDS(structural_forecast, file = paste0("data/ger/forecasts/Structural/", upcoming_election, "_structural_forecast.RDS"))

jags_summary_df <- jags_summary(jags_matrix)

b_mean <- filter(jags_summary_df, str_detect(var, "^b\\[")) %>% magrittr::extract2("mean")

b_mean <- matrix(b_mean, ncol = 3)


b_0_mean <- filter(jags_summary_df, str_detect(var, "^b0\\[")) %>% magrittr::extract2("mean")


structural_inits <- list(b = b_mean, b0 = b_0_mean)

saveRDS(structural_inits, paste0("data/ger/structural_inits/", upcoming_election,"_structural_inits.RDS"))

