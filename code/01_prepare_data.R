### ----------------------------------------------------------
### Election Polling Forecasting Script
### Structural Model
### Authors: Lukas Stoetzer & Cornelius Erfort 
### ----------------------------------------------------------

# Load necessary packages and functions
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions

### Data for Structural Model

# Get polls
wahlrecht_polls <- get_wahlrecht_polls()
save(wahlrecht_polls, file = str_c("output/polls/wahlrecht_polls_", Sys.Date(),".RData"))


# Load structural data from 2021
data_structural <- readRDS("data/pre_train_data_21.rds")

# Export data structural for hand coding
# data_structural_2021 <- filter(data_structural, election == 20)
# write.xlsx(data_structural_2021, "data/ger/Structural/pre_train_data_21.xlsx")

# Read xlsx back in
data_structural_2021 <- read.xlsx("data/pre_train_data_21.xlsx")

# 2021 election results for hand coding
# btw_2021_kerg2 <- read_csv("data/btw_2021_kerg2.csv")
# 
# btw_2021_kerg2 %>% filter(Gruppenart == "Partei" & Stimme == 2 & Gebietsart == "Bund") %>% select(Gruppenname, Prozent) %>% 
#   # Rename Gruppenname CSU to CDU
#   mutate(Gruppenname = ifelse(Gruppenname == "CSU", "CDU", Gruppenname),
#          Prozent = Prozent %>% str_replace("\\,", ".") %>% as.numeric) %>%
#   # Aggreate by Gruppenname
#   group_by(Gruppenname) %>% summarise(Prozent = sum(Prozent) %>% round(2)) %>% 
#   # Order by Prozent decreasing
#   arrange(desc(Prozent)) %>% View


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

# Set past vote share of BSW to 0
data_structural$voteshare_l1[data_structural$party == "bsw"] <- 0

data_structural <- data_structural %>% arrange(election, party)

saveRDS(data_structural, "data/pre_train_data_25.rds")
