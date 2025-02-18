# Source all the libraries needed for this analysis.
# Automatically installs if the package isn't installed.

# source("scripts/01-load-packages.R")

# Also source some helpful functions

# source("scripts/zz-functions.R")

library(stringdist)

btw_candidates_1983_2017 <- read_delim("/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2017.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

btw_candidates_1983_2017 <- btw_candidates_1983_2017 %>% 
  group_by(wkr, election) %>% 
  mutate(incumbent_party = (res_l1_E == max(res_l1_E)) %>% 
           as.numeric) %>% ungroup()

btw_candidates_1983_2017$res_l1_E <- str_replace(btw_candidates_1983_2017$res_l1_E, "\\,", ".") %>% as.numeric()
btw_candidates_1983_2017$res_l1_Z <- str_replace(btw_candidates_1983_2017$res_l1_Z, "\\,", ".") %>% as.numeric()
btw_candidates_1983_2017$resp_E <- str_replace(btw_candidates_1983_2017$resp_E, "\\,", ".") %>% as.numeric()
btw_candidates_1983_2017$resp_Z <- str_replace(btw_candidates_1983_2017$resp_Z, "\\,", ".") %>% as.numeric()
btw_candidates_1983_2017$propPlatz <- str_replace(btw_candidates_1983_2017$propPlatz, "\\,", ".") %>% as.numeric()


btwkr25_umrechnung_btw21 <- read_delim("data/btwkr25_umrechnung_btw21.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                       skip = 4)


# Add the first letter of the first row to the column names if the value is not NA
names(btwkr25_umrechnung_btw21)[!is.na(btwkr25_umrechnung_btw21[1,])] <- paste0(names(btwkr25_umrechnung_btw21)[!is.na(btwkr25_umrechnung_btw21[1,])], "_", substr(btwkr25_umrechnung_btw21[1,!is.na(btwkr25_umrechnung_btw21[1,])[1,]], 1, 1))
btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21[-1,]

names(btwkr25_umrechnung_btw21) <- names(btwkr25_umrechnung_btw21) %>% str_replace_all("\\.{3}", "_") %>% tolower()



btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% rename(wkr = "wkr-nr.", wkr_name = wahlkreisname)


# Pivot longer from col 14
btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% pivot_longer(cols = 14:ncol(btwkr25_umrechnung_btw21), names_to = "partei", values_to = "votes") # %>% 

btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21[, -c(4:11)]

btwkr25_umrechnung_btw21 <- filter(btwkr25_umrechnung_btw21, wkr < 300)


# Pivot wider col partei, by endings _e and _z
btwkr25_umrechnung_btw21$stimme <- str_extract(btwkr25_umrechnung_btw21$partei, "[ez]$")
btwkr25_umrechnung_btw21$partei <- str_remove(btwkr25_umrechnung_btw21$partei, "_[ez]$")
btwkr25_umrechnung_btw21$partei <- str_remove(btwkr25_umrechnung_btw21$partei, "_[0-9]{2}$")

# Pivot wider by stimme
btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% pivot_wider(names_from = stimme, values_from = votes)

# Change all partei to "And" which are not cdu, csu, spd, afd, die linke, fdp, grüne
btwkr25_umrechnung_btw21$partei <- btwkr25_umrechnung_btw21$partei %>%
  str_replace_all("^(?!cdu|csu|spd|afd|die linke|fdp|grüne).*", "And")

# Filter all obs which are partei csu but not land BY




# aggregate gültige_e gültig_z votes by wkr land wahlkreisname partei stimme
btwkr25_umrechnung_btw21 <- aggregate(cbind(e, z) ~ gültige_e + gültig_z + wkr + land + wkr_name + partei, btwkr25_umrechnung_btw21, FUN = function (x) sum(as.numeric(x), na.rm = TRUE))

btwkr25_umrechnung_btw21$gültige_e <- as.numeric(btwkr25_umrechnung_btw21$gültige_e)
btwkr25_umrechnung_btw21$gültig_z <- as.numeric(btwkr25_umrechnung_btw21$gültig_z)

btwkr25_umrechnung_btw21$res_l1_E <- (btwkr25_umrechnung_btw21$e / btwkr25_umrechnung_btw21$gültige_e) # %>% str_replace("\\.", ",")

btwkr25_umrechnung_btw21$res_l1_Z <- (btwkr25_umrechnung_btw21$z / btwkr25_umrechnung_btw21$gültig_z)  # %>% str_replace("\\.", ",")

btwkr25_umrechnung_btw21$resp_E <- NA

btwkr25_umrechnung_btw21$election <- 2025

# winner, second, party_incumbent

# Determine which obs won (had the most votes e)
btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% group_by(wkr) %>% mutate(incumbent_party = (e == max(e)) %>% as.numeric) %>% ungroup()

# btw_candidates_1983_2017$resp_E <- btw_candidates_1983_2017$resp_E %>% str_replace("\\,", ".") %>% as.numeric
# btw_candidates_1983_2017$res_l1_E <- btw_candidates_1983_2017$res_l1_E %>% str_replace("\\,", ".") %>% as.numeric


# btw_candidates_1983_2017 <- btw_candidates_1983_2017 %>% group_by(wkr, election) %>% mutate(winner_test = (resp_E) == max(resp_E)) %>% ungroup()

# table(btw_candidates_1983_2017$winner == btw_candidates_1983_2017$winner_test)

# btw_candidates_1983_2017$winner_mis <- (btw_candidates_1983_2017$winner == btw_candidates_1983_2017$winner_test)

# Determine second place
# btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% group_by(wkr) %>% mutate(second = (e == sort(e, decreasing = TRUE)[2]) %>% as.numeric) %>% ungroup()

# Rename gültige_e to valid_E and gültig_z to valid_Z
btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% rename(valid_E_l1 = gültige_e, valid_Z_l1 = gültig_z)

# Remove if land == BY and party == cdu
btwkr25_umrechnung_btw21 <- btwkr25_umrechnung_btw21 %>% filter(!(land == "BY" & partei == "cdu"))

# btwkr25_umrechnung_btw21 <- dplyr::select(btwkr25_umrechnung_btw21, -c(gültige_e, gültig_z, e, z))




##################################
# 2017 results in 2021 boundaries
##################################

btwkr21_umrechnung_btw17 <- read_delim("data/btwkr21_umrechnung_btw17.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                       skip = 4)


# Add the first letter of the first row to the column names if the value is not NA
names(btwkr21_umrechnung_btw17)[!is.na(btwkr21_umrechnung_btw17[1,])] <- paste0(names(btwkr21_umrechnung_btw17)[!is.na(btwkr21_umrechnung_btw17[1,])], "_", substr(btwkr21_umrechnung_btw17[1,!is.na(btwkr21_umrechnung_btw17[1,])[1,]], 1, 1))
btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17[-1,]

names(btwkr21_umrechnung_btw17) <- names(btwkr21_umrechnung_btw17) %>% str_replace_all("\\.{3}", "_") %>% tolower()



btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17 %>% rename(wkr = "wkr-nr.", wkr_name = wahlkreisname)


# Pivot longer from col 14
btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17 %>% pivot_longer(cols = 10:ncol(btwkr21_umrechnung_btw17), names_to = "partei", values_to = "votes") # %>% 

btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17[, -c(4:7)]


btwkr21_umrechnung_btw17$wkr <- btwkr21_umrechnung_btw17$wkr %>% as.numeric
btwkr21_umrechnung_btw17 <- filter(btwkr21_umrechnung_btw17, wkr < 300)

# Pivot wider col partei, by endings _e and _z
btwkr21_umrechnung_btw17$stimme <- str_extract(btwkr21_umrechnung_btw17$partei, "[ez]$")
btwkr21_umrechnung_btw17$partei <- str_remove(btwkr21_umrechnung_btw17$partei, "_[ez]$")
btwkr21_umrechnung_btw17$partei <- str_remove(btwkr21_umrechnung_btw17$partei, "_[0-9]{2}$")

# Pivot wider by stimme
btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17 %>% pivot_wider(names_from = stimme, values_from = votes)

# Change all partei to "And" which are not cdu, csu, spd, afd, die linke, fdp, grüne
btwkr21_umrechnung_btw17$partei <- btwkr21_umrechnung_btw17$partei %>%
  str_replace_all("^(?!cdu|csu|spd|afd|die linke|fdp|grüne).*", "And")

# Filter all obs which are partei csu but not land BY
btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17 %>% filter(!(partei == "csu" & land != "BY"))


# aggregate gültige_e gültig_z votes by wkr land wahlkreisname partei stimme
btwkr21_umrechnung_btw17 <- aggregate(cbind(e, z) ~ gültige_8_e + gültige_9_z + wkr + land + wkr_name + partei, btwkr21_umrechnung_btw17, FUN = function (x) sum(as.numeric(x), na.rm = TRUE))

btwkr21_umrechnung_btw17$gültige_8_e <- btwkr21_umrechnung_btw17$gültige_8_e %>% as.numeric
btwkr21_umrechnung_btw17$gültige_9_z <- btwkr21_umrechnung_btw17$gültige_9_z %>% as.numeric

btwkr21_umrechnung_btw17$res_l1_E <- (btwkr21_umrechnung_btw17$e / btwkr21_umrechnung_btw17$gültige_8_e) # %>% str_replace("\\.", ",")

btwkr21_umrechnung_btw17$res_l1_Z <- (btwkr21_umrechnung_btw17$z / btwkr21_umrechnung_btw17$gültige_9_z) #  %>% str_replace("\\.", ",")

btwkr21_umrechnung_btw17$election <- 2021

# winner, second, party_incumbent

# Determine which obs won (had the most votes e)
btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17 %>% group_by(wkr) %>% mutate(incumbent_party = (e == max(e)) %>% as.numeric) %>% ungroup()

# Rename gültige_e to valid_E and gültig_z to valid_Z
btwkr21_umrechnung_btw17 <- btwkr21_umrechnung_btw17 %>% rename(valid_E_l1 = gültige_8_e, valid_Z_l1 = gültige_9_z)


###################
# Load 2021 results
###################

btw21_kerg2 <- read_delim("data/btw21_kerg2.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                          skip = 9)
df_gebiete <- data.frame(
  Gebietsnummer = c("01", "13", "02", "03", "04", "12", "15", "11", 
                    "05", "14", "06", "16", "07", "09", "08", "10"),
  Land = c("SH", "MV", "HH", "NI", "HB", "BB", "ST", "BE", 
           "NW", "SN", "HE", "TH", "RP", "BY", "BW", "SL")
)

btw21_kerg2 <- btw21_kerg2 %>% filter(Gebietsart == "Wahlkreis")

btw21_kerg2 <- merge(btw21_kerg2, df_gebiete, by.x = "UegGebietsnummer", by.y = "Gebietsnummer")


btw21_kerg2 <- btw21_kerg2 %>%
  dplyr::select(
    wkr = Gebietsnummer,
    wkr_name = Gebietsname,
    partei = Gruppenname,
    votes = Anzahl,
    land = Land,
    Stimme
  ) 

# If Stimme is not NA, add it after a _ to partei
btw21_kerg2$partei <- ifelse(is.na(btw21_kerg2$Stimme), btw21_kerg2$partei, paste0(btw21_kerg2$partei, "_", btw21_kerg2$Stimme))
btw21_kerg2 <- dplyr::select(btw21_kerg2, -Stimme)

# Pivot wider by stimme
btw21_kerg2 <- btw21_kerg2 %>% pivot_wider(id_cols = c(wkr, wkr_name, land), names_from = partei, values_from = votes)
btw21_kerg2 <- btw21_kerg2[, -c(4:7)]

# Pivot longer
btw21_kerg2 <- btw21_kerg2 %>% pivot_longer(cols = 6:ncol(btw21_kerg2), names_to = "partei", values_to = "votes") # %>% 


# Pivot wider col partei, by endings _e and _z
btw21_kerg2$stimme <- str_extract(btw21_kerg2$partei, "[12]$")
btw21_kerg2$partei <- str_remove(btw21_kerg2$partei, "_[12]$")
# btwkr21_umrechnung_btw17$partei <- str_remove(btwkr21_umrechnung_btw17$partei, "_[12]{2}$")


# Change all partei to "And" which are not cdu, csu, spd, afd, die linke, fdp, grüne
btw21_kerg2$partei <- btw21_kerg2$partei %>% tolower() %>% 
  str_replace_all("^(?!cdu|csu|spd|afd|die linke|fdp|grüne).*", "And")

# Filter all obs which are partei csu but not land BY
btw21_kerg2 <- btw21_kerg2 %>% filter(!(partei == "csu" & land != "BY"))

# aggregate gültige_e gültig_z votes by wkr land wahlkreisname partei stimme
btw21_kerg2 <- aggregate(cbind(votes) ~ Gültige_1 + Gültige_2 + wkr + land + wkr_name + partei + stimme, btw21_kerg2, FUN = function (x) sum(as.numeric(x), na.rm = TRUE))

btw21_kerg2$Gültige_1 <- btw21_kerg2$Gültige_1 %>% as.numeric()
btw21_kerg2$Gültige_2 <- btw21_kerg2$Gültige_2 %>% as.numeric()

# Pivot wider by stimme
btw21_kerg2 <- btw21_kerg2 %>% pivot_wider(names_from = stimme, values_from = votes)

# Rename column 1 to E and 2 to Z
btw21_kerg2 <- btw21_kerg2 %>% rename(e = `1`, z = `2`)

# Replace NA values with 0 for the relevant columns
btw21_kerg2[ , c("Gültige_1", "Gültige_2", "e", "z")] <- 
  lapply(btw21_kerg2[ , c("Gültige_1", "Gültige_2", "e", "z")], function(x) ifelse(is.na(x), 0, x))


btw21_kerg2 <- aggregate(cbind(Gültige_1, Gültige_2, e, z) ~ wkr + land + wkr_name + partei, btw21_kerg2, FUN = function (x) sum(as.numeric(x), na.rm = TRUE))


btw21_kerg2$resp_E <- (btw21_kerg2$e / btw21_kerg2$Gültige_1) # %>% str_replace("\\.", ",")

btw21_kerg2$resp_Z <- (btw21_kerg2$z / btw21_kerg2$Gültige_2)  # %>% str_replace("\\.", ",")

btw21_kerg2$election <- 2021

btw21_kerg2$wkr <- btw21_kerg2$wkr %>% as.numeric

btw21_kerg2 %>% dplyr::select(partei, z, Gültige_2) %>% 
  group_by(partei) %>% 
  summarise(sum(z)/46362013)


# winner, second, party_incumbent

# Determine which obs won (had the most votes e)
# btw21_kerg2 <- btw21_kerg2 %>% group_by(wkr) %>% mutate(incumbent_party = (e == max(e)) %>% as.numeric) %>% ungroup()



# dplyr::select(btw21_kerg2, c(land, wkr, res_l1_E, res_l1_Z, partei, election)) %>% View

# dplyr::select(btwkr21_umrechnung_btw17, c(wkr, land, wkr_name, partei, resp_E, resp_Z, election, incumbent_party)) %>% View

# btw21_kerg2 Rename Gültige_1 to valid_E and Gültige_2 to valid_Z
btw21_kerg2 <- btw21_kerg2 %>% rename(valid_E = Gültige_1, valid_Z = Gültige_2)

btw_candidates_2021 <- merge(dplyr::select(btwkr21_umrechnung_btw17, c(wkr, land, wkr_name, partei, res_l1_E, res_l1_Z, valid_E_l1, valid_Z_l1, election, incumbent_party)), 
              dplyr::select(btw21_kerg2, c(land, wkr, resp_E, resp_Z, partei, election, valid_Z, valid_E)), 
              by = c("wkr", "land", "partei", "election"), all = TRUE)
# btw_candidates_2021$resp_E <- btw_candidates_2021$resp_E %>% as.numeric


# 2017 in 2021 grenzen + 2021 wahl

# 2021 in 2025 grenzen -> 2025 wahl



btw_candidates_1983_2025 <- bind_rows(btw_candidates_1983_2017, btw_candidates_2021)

btw_candidates_1983_2025 <- bind_rows(btw_candidates_1983_2025, btwkr25_umrechnung_btw21)

# Harmonize the 'partei' variable
btw_candidates_1983_2025$partei <- btw_candidates_1983_2025$partei %>%
  toupper() %>%                                  # Convert to uppercase
  str_replace_all("^DIE\\s+", "") %>%            # Remove "DIE " at the start
  str_replace_all("GRÜNE", "GRUENE")             # Replace "GRÜNE" with "GRUENE"


btw_candidates_1983_2025 <- btw_candidates_1983_2025 %>% arrange(wkr, election) %>% dplyr::select(-c(e, z))

btw_candidates_1983_2025 <- btw_candidates_1983_2025 %>% filter(!(partei == "CDU" & land == "BY"))
btw_candidates_1983_2025 <- btw_candidates_1983_2025 %>% filter(!(partei == "CSU" & land != "BY"))

# save(btw_candidates_1983_2025, file = "raw-data/btw_candidates_1983-2025.RData")

# write.csv2(btw_candidates_1983_2025, "raw-data/btw_candidates_1983-2025.csv", row.names = F)

# load("raw-data/btw_candidates_1983-2025.RData")

# Summarise valid_E and valid_Z by wkr and election
btw_candidates_1983_2025 %>% filter(incumbent_party == 1) %>% group_by(election) %>% summarise(valid_E_l1 = sum(valid_E_l1), valid_Z_l1 = sum(valid_Z_l1)) %>% ungroup() %>% arrange(election) # Correct


btw_candidates_1983_2025 %>% filter(incumbent_party == 1 & election %in% c(2021, 2025)) %>% group_by(election, land) %>% summarise(valid_Z_l1 = sum(valid_Z_l1)) %>% ungroup() %>% arrange(election) %>% View



btw_candidates_1983_2025 %>% filter(incumbent_party == 1 & election %in% c(2021)) %>% group_by(election, land) %>% summarise(valid_Z_l1 = sum(valid_Z_l1), valid_Z = sum(valid_Z)) %>% ungroup() %>% arrange(election) %>% dplyr::select(valid_Z_l1, valid_Z) %>% cor


# Add no_cand_l1 indicator
btw_candidates_1983_2025$no_cand_l1 <- as.numeric(btw_candidates_1983_2025$res_l1_E == 0)



# Process LINKE and BSW data for 2025
linke_df <- btw_candidates_1983_2025 %>%
  filter(election == 2025, partei == "LINKE") %>%
  dplyr::mutate(res_l1_Z = res_l1_Z)

bsw_df <- btw_candidates_1983_2025 %>%
  filter(election == 2025, partei == "LINKE") %>%
  dplyr::mutate(
    partei = "BSW",
    res_l1_E = 0,
    res_l1_Z = res_l1_Z
  )

# Combine processed data
btw_candidates_1983_2025 <- bind_rows(
  filter(btw_candidates_1983_2025, !(partei == "LINKE" & election == 2025)),
  linke_df,
  bsw_df
)


# Load historical candidate data
# btw_candidates_1983_2025 <- read.csv2("/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2025.csv", stringsAsFactors = FALSE)

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
         propPlatz = 0, akad = 0, direct_list_incumbent = F, formercand = 0, name = "And")

# Remove "oth" from original data
kandidierende_2021 <- kandidierende_2021 %>%
  filter(party != "oth")


# Only keep SPD, CDU, CSU, GRÜNE, FDP, Die Linke, AfD, BSW
# kandidierende_2021 <- kandidierende_2021 %>% 
#   filter(Gruppenname %in% c("SPD", "CDU", "CSU", "GRÜNE", "GRÜNE/B 90", "FDP", "DIE LINKE", "AfD", "BSW"))


kandidierende_2021$akad <- as.numeric(!is.na(kandidierende_2021$Titel))

kandidierende_2021$female <- as.numeric(kandidierende_2021$Geschlecht == "w")

kandidierende_2021$name <- paste0(
  kandidierende_2021$Nachname, 
  ifelse(!is.na(kandidierende_2021$Namenszusatz) & kandidierende_2021$Namenszusatz != "", 
         paste0(" ", kandidierende_2021$Namenszusatz), ""),
  ", ",
  ifelse(!is.na(kandidierende_2021$Titel) & kandidierende_2021$Titel != "", 
         paste0(kandidierende_2021$Titel, " "), ""),
  kandidierende_2021$Vornamen
) %>% tolower()

## formercand
names2017 <- filter(btw_candidates_1983_2025, election == 2017 & party != "oth") %>%
  dplyr::select(wkr, party, name) %>% 
  mutate(name17 = str_extract(name, ".*(?=\\,)")) %>% 
  dplyr::select(-c(name))

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
  dplyr::select(c(wkr, party, direct_list_incumbent, akad, female, propPlatz, ncand, formercand, name21, name))

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
         propPlatz = 0, akad = 0, direct_list_incumbent = F, formercand = 0, name = "And")

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

kandidierende_2025$party %>% unique

kandidierende_2025$name <- paste0(
  kandidierende_2025$Nachname, 
  ifelse(!is.na(kandidierende_2025$Namenszusatz) & kandidierende_2025$Namenszusatz != "", 
         paste0(" ", kandidierende_2025$Namenszusatz), ""),
  ", ",
  ifelse(!is.na(kandidierende_2025$Titel) & kandidierende_2025$Titel != "", 
         paste0(kandidierende_2025$Titel, " "), ""),
  kandidierende_2025$Vornamen
) %>% tolower()

## formercand
names2021 <- filter(kandidierende_2021, party != "oth") %>%
  dplyr::select(wkr, party, name21)

kandidierende_2025$name25 <- tolower(kandidierende_2025$Nachname) %>% str_replace_all(c("ü" = "ue", "ö" = "oe", "ä" = "ae", "ß" = "ss"))

kandidierende_2025 <- left_join(kandidierende_2025, names2021, by = c("wkr", "party"))

kandidierende_2025$stringdist <- stringdist(kandidierende_2025$name25, kandidierende_2025$name21, method = "jaccard", q = 2)

kandidierende_2025$formercand <- as.numeric(kandidierende_2025$stringdist < 0.5)
kandidierende_2025$formercand[is.na(kandidierende_2025$stringdist)] <- 0

# Add back the aggregated "oth" observations
kandidierende_2025 <- bind_rows(kandidierende_2025, oth_summary)

# Keep only wkr party direct_list_incumbent akad
kandidierende_2025 <- kandidierende_2025 %>% 
  dplyr::select(c(wkr, party, direct_list_incumbent, akad, female, propPlatz, ncand, formercand, name))

kandidierende_2025$election <- 2025

kandidierende_2025$east <- as.numeric(kandidierende_2025$wkr %in% c(12:17, 56:65, 66:73,  150:165, 188:195, 74, 75, 82, 83, 84, 85))

# Get number of candidates per wkr

kandidierende_2025$propPlatz[is.na(kandidierende_2025$propPlatz)] <- 0


kandidierende_2025$party %>% unique

# Merge
kandidierende <- rbind(kandidierende_2021 %>% dplyr::select(-c(name21)), kandidierende_2025)
kandidierende$propPlatz[is.na(kandidierende$propPlatz)] <- 0
kandidierende$alsoList <- 0
kandidierende$alsoList[kandidierende$propPlatz == 0] <- 1

kandidierende$party %>% unique
btw_candidates_1983_2025$party %>% unique

# Merge kandidierende to btw_candidunique()# Merge kandidierende to btw_candidates_1983_2025 by election wkr party, fill existing columns when NA
test <- btw_candidates_1983_2025 %>% 
  left_join(kandidierende, by = c("election", "wkr", "party")) %>% 
  dplyr::mutate(akad = ifelse(is.na(akad.x), akad.y, akad.x),
                female = ifelse(is.na(female.x), female.y, female.x),
                propPlatz = ifelse(is.na(propPlatz.x), propPlatz.y, propPlatz.x),
                alsoList = ifelse(is.na(alsoList.x), alsoList.y, alsoList.x),
                formercand = ifelse(is.na(formercand.x), formercand.y, formercand.x),
                ncand = ifelse(is.na(ncand.x), ncand.y, ncand.x),  # Adding ncand
                name = ifelse(is.na(name.x), name.y, name.x),  # Adding name
                east = ifelse(is.na(east.x), east.y, east.x)  # Adding east
  ) %>% 
  dplyr::select(-akad.x, -akad.y, -female.x, -female.y, 
                -propPlatz.x, -propPlatz.y, -alsoList.x, -alsoList.y, 
                -formercand.x, -formercand.y, -ncand.x, -ncand.y,  # Dropping ncand.x, ncand.y
                -name.x, -name.y,  # Dropping name.x, name.y
                -east.x, -east.y)  # Dropping east.x, east.y


# When election is 2021 or 2025 and party is oth, set akad to 0 and direct_list_incumbent to FALSE
test <- test %>% 
  dplyr::mutate(akad = ifelse(election %in% c(2021, 2025) & party == "oth", 0, akad),
                propPlatz = ifelse(election %in% c(2021, 2025) & party == "oth", 0, propPlatz),
                alsoList = ifelse(election %in% c(2021, 2025) & party == "oth", 1, alsoList),
                female = ifelse(election %in% c(2021, 2025) & party == "oth", 0, female),
                formercand = ifelse(election %in% c(2021, 2025) & party == "oth", 0, formercand),
                direct_list_incumbent = ifelse(election %in% c(2021, 2025) & party == "oth", FALSE, direct_list_incumbent))


test <- test %>% 
  dplyr::mutate(incumbent = case_when(
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



write.csv(test[, names(test)[names(test) != "name"]], file = "data/btw_candidates_1983-2025_full.csv")

write.csv(test, file = "/mnt/forecasts/prediction-2025/temp/btw_candidates_1983-2025_full.csv")



