library(stringdist)

# Load historical candidate data
btw_candidates_1983_2025 <- read.csv2("/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2025.csv", stringsAsFactors = FALSE)

# Process party names
btw_candidates_1983_2025$partei[btw_candidates_1983_2025$partei == "CSU"] <- "CDU/CSU"
btw_candidates_1983_2025$partei[btw_candidates_1983_2025$partei == "CDU"] <- "CDU/CSU"

btw_candidates_1983_2025$party <- case_when(
  btw_candidates_1983_2025$partei == "CDU/CSU" ~ "cdu",
  btw_candidates_1983_2025$partei == "SPD" ~ "spd",
  btw_candidates_1983_2025$partei == "LINKE" ~ "lin",
  btw_candidates_1983_2025$partei == "GRUENE" ~ "gru",
  btw_candidates_1983_2025$partei == "FDP" ~ "fdp",
  btw_candidates_1983_2025$partei == "AFD" ~ "afd",
  btw_candidates_1983_2025$partei == "BSW" ~ "bsw",
  TRUE ~ "oth"
)
btw_candidates_1983_2025$party %>% table


# Load candidate data for 2021
kandidierende_2021 <- read_csv("/mnt/forecasts/prediction-2025/temp/kandidierende-2021.csv")

# Create required variables
kandidierende_2021 <- kandidierende_2021 %>%
  mutate(direct_list_incumbent = !is.na(VorpGewaehlt),
         wkr = Gebietsnummer,
         party = case_when(
           Gruppenname == "SPD" ~ "spd",
           Gruppenname == "CDU" ~ "cdu",
           Gruppenname == "CSU" ~ "cdu",
           Gruppenname == "GRÜNE" ~ "gru",
           Gruppenname == "GRÜNE/B 90" ~ "gru",
           Gruppenname == "FDP" ~ "fdp",
           Gruppenname == "DIE LINKE" ~ "lin",
           Gruppenname == "AfD" ~ "afd",
           Gruppenname == "BSW" ~ "bsw",
           TRUE ~ "oth"
         ))

# Get max of Listenplatz within Staat
kandidierende_2021 <- kandidierende_2021 %>% 
  group_by(GebietLandAbk, party) %>% 
  mutate(max_listenplatz = max(Listenplatz, na.rm = T),
         propPlatz = VerknListenplatz/max_listenplatz) %>% 
  ungroup()

# For party gru and GebietLandAbk SL set propPlatz to 0
kandidierende_2021 <- kandidierende_2021 %>% 
  mutate(propPlatz = ifelse(party == "gru" & GebietLandAbk == "SL", 0, propPlatz))

kandidierende_2021 <- filter(kandidierende_2021, Gebietsart == "Wahlkreis") 

# Compute ncand for each wkr
kandidierende_2021 <- kandidierende_2021 %>%
  group_by(wkr) %>%
  mutate(ncand = n())

# Compute female share for "oth" per wkr
oth_summary <- kandidierende_2021 %>%
  filter(party == "oth") %>%
  group_by(wkr) %>%
  summarise(female = mean(Geschlecht == "w", na.rm = TRUE),
            ncand = first(ncand)) %>%  # Preserve ncand
  mutate(party = "oth",
         propPlatz = 0, akad = 0, direct_list_incumbent = F, formercand = 0)

# Remove "oth" from original data
kandidierende_2021 <- kandidierende_2021 %>%
  filter(party != "oth")


# Only keep SPD, CDU, CSU, GRÜNE, FDP, Die Linke, AfD, BSW
# kandidierende_2021 <- kandidierende_2021 %>% 
#   filter(Gruppenname %in% c("SPD", "CDU", "CSU", "GRÜNE", "GRÜNE/B 90", "FDP", "DIE LINKE", "AfD", "BSW"))


kandidierende_2021$akad <- as.numeric(!is.na(kandidierende_2021$Titel))

kandidierende_2021$female <- as.numeric(kandidierende_2021$Geschlecht == "w")


## formercand
names2017 <- filter(btw_candidates_1983_2025, election == 2017 & party != "oth") %>%
  select(wkr, party, name) %>% 
  mutate(name17 = str_extract(name, ".*(?=\\,)")) %>% select(-c(name))

kandidierende_2021$name21 <- tolower(kandidierende_2021$Nachname) %>% str_replace_all(c("ü" = "ue", "ö" = "oe", "ä" = "ae", "ß" = "ss"))

kandidierende_2021 <- left_join(kandidierende_2021, names2017, by = c("wkr", "party"))

kandidierende_2021$stringdist <- stringdist(kandidierende_2021$name21, kandidierende_2021$name17, method = "jaccard", q = 2)

kandidierende_2021$formercand <- as.numeric(kandidierende_2021$stringdist < 0.5)
kandidierende_2021$formercand[is.na(kandidierende_2021$stringdist)] <- 0

kandidierende_2021$propPlatz[is.na(kandidierende_2021$propPlatz)] <- 0

# Add back the aggregated "oth" observations
kandidierende_2021 <- bind_rows(kandidierende_2021, oth_summary)

# Keep only wkr party direct_list_incumbent akad
kandidierende_2021 <- kandidierende_2021 %>% 
  dplyr::select(c(wkr, party, direct_list_incumbent, akad, female, propPlatz, ncand, formercand, name21))

kandidierende_2021$election <- 2021

kandidierende_2021$east <- as.numeric(kandidierende_2021$wkr %in% c(12:17, 56:65, 66:74, 151:166, 189:196, 75, 76, 83, 84, 85, 86))


# Add data for 2025 candidates
kandidierende_2025 <- read_delim("/mnt/forecasts/prediction-2025/temp/btw25_bewerb_utf8.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                 skip = 8)

kandidierende_2025 %>% jsonlite::toJSON() %>% 
# Write to home
writeLines(con = "~/btw25_bewerb_utf8.json")

# Create required variables
kandidierende_2025 <- kandidierende_2025 %>%
  mutate(direct_list_incumbent = !is.na(VorpGewaehlt),
         wkr = Gebietsnummer,
         party = case_when(
           GruppennameKurz == "SPD" ~ "spd",
           GruppennameKurz == "CDU" ~ "cdu",
           GruppennameKurz == "CSU" ~ "cdu",
           GruppennameKurz == "GRÜNE" ~ "gru",
           GruppennameKurz == "GRÜNE/B 90" ~ "gru",
           GruppennameKurz == "FDP" ~ "fdp",
           GruppennameKurz == "Die Linke" ~ "lin",
           GruppennameKurz == "AfD" ~ "afd",
           GruppennameKurz == "BSW" ~ "bsw",
           TRUE ~ "oth"
         ))

kandidierende_2025$party %>% table

# Get max of Listenplatz within Staat
kandidierende_2025 <- kandidierende_2025 %>% 
  group_by(Staat, party) %>% 
  mutate(max_listenplatz = max(Listenplatz, na.rm = T),
         propPlatz = VerknListenplatz/max_listenplatz) %>% 
  ungroup()


kandidierende_2025 <- filter(kandidierende_2025, Gebietsart == "Wahlkreis") # %>% 
# dplyr::select(c(wkr = Gebietsnummer, GruppennameKurz, Nachname, Vornamen)) # VorpGewaehlt

kandidierende_2025 <- kandidierende_2025 %>% 
  group_by(wkr) %>% 
  mutate(ncand = n()) 

# Compute female share for "oth" per wkr
oth_summary <- kandidierende_2025 %>%
  filter(party == "oth") %>%
  group_by(wkr) %>%
  summarise(female = mean(Geschlecht == "w", na.rm = TRUE),
            ncand = first(ncand)) %>%  # Preserve ncand
  mutate(party = "oth",
         propPlatz = 0, akad = 0, direct_list_incumbent = F, formercand = 0)

# Remove "oth" from original data
kandidierende_2025 <- kandidierende_2025 %>%
  filter(party != "oth")

# Only keep SPD, CDU, CSU, GRÜNE, FDP, Die Linke, AfD, BSW
# kandidierende_2025 <- kandidierende_2025 %>% 
#   filter(GruppennameKurz %in% c("SPD", "CDU", "CSU", "GRÜNE", "GRÜNE/B 90", "FDP", "Die Linke", "AfD", "BSW"))

kandidierende_2025$akad <- as.numeric(!is.na(kandidierende_2025$Titel))

kandidierende_2025$direct_list_incumbent <- !is.na(kandidierende_2025$VorpGewaehlt)

kandidierende_2025$wkr <- kandidierende_2025$Gebietsnummer

kandidierende_2025$female <- as.numeric(kandidierende_2025$Geschlecht == "w")



## formercand
names2021 <- filter(kandidierende_2021, party != "oth") %>%
  select(wkr, party, name21)

kandidierende_2025$name25 <- tolower(kandidierende_2025$Nachname) %>% str_replace_all(c("ü" = "ue", "ö" = "oe", "ä" = "ae", "ß" = "ss"))

kandidierende_2025 <- left_join(kandidierende_2025, names2021, by = c("wkr", "party"))

kandidierende_2025$stringdist <- stringdist(kandidierende_2025$name25, kandidierende_2025$name21, method = "jaccard", q = 2)

kandidierende_2025$formercand <- as.numeric(kandidierende_2025$stringdist < 0.5)
kandidierende_2025$formercand[is.na(kandidierende_2025$stringdist)] <- 0

# Add back the aggregated "oth" observations
kandidierende_2025 <- bind_rows(kandidierende_2025, oth_summary)

# Keep only wkr party direct_list_incumbent akad
kandidierende_2025 <- kandidierende_2025 %>% 
  dplyr::select(c(wkr, party, direct_list_incumbent, akad, female, propPlatz, ncand, formercand))

kandidierende_2025$election <- 2025

kandidierende_2025$east <- as.numeric(kandidierende_2025$wkr %in% c(12:17, 56:65, 66:73,  150:165, 188:195, 74, 75, 82, 83, 84, 85))

# Get number of candidates per wkr

kandidierende_2025$propPlatz[is.na(kandidierende_2025$propPlatz)] <- 0



# Merge
kandidierende <- rbind(kandidierende_2021 %>% select(-c(name21)), kandidierende_2025)
kandidierende$propPlatz[is.na(kandidierende$propPlatz)] <- 0
kandidierende$alsoList <- 0
kandidierende$alsoList[kandidierende$propPlatz == 0] <- 1


# Merge kandidierende to btw_candidates_1983_2025 by election wkr party, fill existing columns when NA
test <- btw_candidates_1983_2025 %>% 
  left_join(kandidierende, by = c("election", "wkr", "party")) %>% 
  mutate(akad = ifelse(is.na(akad.x), akad.y, akad.x),
         female = ifelse(is.na(female.x), female.y, female.x),
         propPlatz = ifelse(is.na(propPlatz.x), propPlatz.y, propPlatz.x),
         alsoList = ifelse(is.na(alsoList.x), alsoList.y, alsoList.x),
         formercand = ifelse(is.na(formercand.x), formercand.y, formercand.x),
         ncand = ifelse(is.na(ncand.x), ncand.y, ncand.x),  # Adding ncand
         east = ifelse(is.na(east.x), east.y, east.x)  # Adding east
  ) %>% 
  select(-akad.x, -akad.y, -female.x, -female.y, 
         -propPlatz.x, -propPlatz.y, -alsoList.x, -alsoList.y, 
         -formercand.x, -formercand.y, -ncand.x, -ncand.y,  # Dropping ncand.x, ncand.y
         -east.x, -east.y)  # Dropping east.x, east.y

# When election is 2021 or 2025 and party is oth, set akad to 0 and direct_list_incumbent to FALSE
test <- test %>% 
  mutate(akad = ifelse(election %in% c(2021, 2025) & party == "oth", 0, akad),
         propPlatz = ifelse(election %in% c(2021, 2025) & party == "oth", 0, propPlatz),
         alsoList = ifelse(election %in% c(2021, 2025) & party == "oth", 1, alsoList),
         female = ifelse(election %in% c(2021, 2025) & party == "oth", 0, female),
         formercand = ifelse(election %in% c(2021, 2025) & party == "oth", 0, formercand),
         direct_list_incumbent = ifelse(election %in% c(2021, 2025) & party == "oth", FALSE, direct_list_incumbent))


test <- test %>% 
  mutate(incumbent = case_when(
    election %in% c(2021, 2025) & direct_list_incumbent & incumbent_party == 1 ~ 1,
    election %in% c(2021, 2025) ~ 0,  # For 2021 and 2025 elections where the condition is not met, set 0
    TRUE ~ incumbent  # For all other elections, keep the original 'incumbent' value
  ))



## Add variable incumbent_in_wkr
test <- test %>% 
  group_by(wkr, election) %>% 
  mutate(incumbent_in_wkr = sum(incumbent, na.rm = T)) %>% 
  ungroup()


test$incumbent_in_wkr %>% table # 12 obs with two incumbets
# test %>% filter(incumbent_in_wkr == 2) %>% View

test$incumbent_in_wkr[test$incumbent_in_wkr == 2] <- 1

# Drop if ncand is NA
test <- test %>% filter(!is.na(ncand))

write.csv(test, file = "/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2025_new.csv")
