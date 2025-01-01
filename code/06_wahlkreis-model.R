rmse <- function(pred, obs) {
  sqrt(mean((pred
             - obs) ^ 2))
}

full_df <-
  read.csv2("data/btw_candidates_1983-2025.csv",
            stringsAsFactors = F)

full_df$partei[full_df$partei == "CSU"] <- "CDU/CSU"
full_df$partei[full_df$partei == "CDU"] <- "CDU/CSU"

res25 <- c(30.2, 16.7, 3.4, 12.2, 4.6, 17.9, 6.8, 8.2) / 100
res21 <- c(24.2, 25.7, 2.45, 14.7, 11.4, 10.4, 2.45, 8.7) / 100
res17 <- c(32.9, 20.5, 4.6, 8.9, 10.7, 12.6, 4.6, 5.9) / 100
res13 <- c(41.5, 25.7, 4.3, 8.4, 4.8, 4.7, 4.3, 6.3) / 100
res09 <- c(33.8, 23, 5.95, 10.7, 14.6, 0.1, 5.95, 6) / 100
res05 <- c(35.2, 34.2, 4.35, 8.1, 9.8, 0.1, 4.35, 3.9) / 100

btw_bund_res <- rbind(res21, res17, res13)

colnames(btw_bund_res) <-
  c("CDU/CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")
rownames(btw_bund_res) <- c("2021", "2017", "2013")


full_df$no_cand_l1 <- as.numeric(full_df$res_l1_E == 0)

# erststimme lag 0 fuer bsw und zweitstimme lag haelfte der linke, linke zweitstimme lag auch haelfte

# Half res_l1_Z when partei == LINKE

linke_df <- filter(full_df, election == 2025 & partei == "LINKE") %>% 
  mutate(res_l1_Z = res_l1_Z/2)

# Add BSW for 2025
bsw_df <- filter(full_df, election == 2025 & partei == "LINKE") %>% 
  mutate(partei = "BSW",
         res_l1_E = 0,
         res_l1_Z = res_l1_Z/2)


full_df <- bind_rows(filter(full_df, !(partei == "LINKE" & election == 2025)), linke_df, bsw_df)

election <- 2025
election_l1 <- election - 4



# Split into training and test data.

train <-
  full_df[(full_df$election < election &
             full_df$election != 1990) |
            (full_df$election == 1990 & full_df$east != 1) , ]

test <- full_df[full_df$election == election,]

test$weight <- 1

# Update swing weights if available.

# if (weights) {
#   for (i in 1:nrow(test)) {
#     if (paste0(test$partei[i], "_Z") %in% colnames(wkrweights0913) &
#         test$wkr_name[i] %in% rownames(wkrweights0913)) {
#       test$weight[i] <-
#         wkrweights0913[rownames(wkrweights0913) == test$wkr_name[i], colnames(wkrweights0913) == paste0(test$partei[i], "_Z")]
#     }
#     
#     if (paste0(test$partei[i], "_Z") %in% colnames(weights0913) &
#         !test$wkr_name[i] %in% rownames(weights0913)) {
#       test$weight[i] <-
#         weights0913[weights0913$L_code == test$land[i], colnames(weights0913) == paste0(test$partei[i], "_Z")]
#     }
#     
#     if (paste0(test$partei[i], "_Z") == "CSU_Z" &
#         test$wkr_name[i] %in% rownames(wkrweights0913)) {
#       test$weight[i] <-
#         wkrweights0913[rownames(wkrweights0913) == test$wkr_name[i], colnames(wkrweights0913) == "CDU_Z"]
#     }
#     
#     if (paste0(test$partei[i], "_Z") == "CSU_Z" &
#         !test$wkr_name[i] %in% rownames(weights0913)) {
#       test$weight[i] <-
#         weights0913[weights0913$L_code == test$land[i], colnames(weights0913) == "CDU_Z"]
#     }
#     
#     
#   }
# }

ff <-
  # "resp_E ~ ncand + propPlatz + resp_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr"
  "resp_E ~ resp_Z + res_l1_E + incumbent_party + no_cand_l1"

reg <-
  lm(ff,
     data = train)

summary(reg)

# Number of simulations for model uncertainty (lm and neural net)

mu_nsim <- 25

S <- MASS::mvrnorm(n = mu_nsim, coef(reg), vcov(reg))

rf_df <-
  model.frame(as.formula(ff),
              data = train)


rf_df <- rf_df[sample(1:nrow(rf_df), nrow(rf_df)), ]


# The test set without vote share predictions is using the last result.

ff_test <-
  # "resp_E ~ ncand + propPlatz + res_l1_Z + res_l1_E + formercand + east + female + incumbent + akad + incumbent_in_wkr"
  "resp_E ~ res_l1_Z + res_l1_E + incumbent_party + no_cand_l1"

rf_test <-
  model.frame(as.formula(ff_test),
              data = test)

# That it works with lm the variables in training and test have to have the
# same names though.

colnames(rf_df) <-
  colnames(rf_test) <-
  c(
    "resp_E",
    # "ncand",
    # "propPlatz",
    "resp_Z",
    "res_l1_E",
    # "formercand",
    # "east",
    # "female",
    # "incumbent",
    # "akad",
    # "incumbent_in_wkr"
    "incumbent_party",
    "no_cand_l1"
  )


# Cutoff has to be one of 92, 36 or 2

draws <- GET("polsci.uni-wh.de:8073/draws") %>% content

# draws is a named list, create a df where the names are the colnames
draws <- as.data.frame(do.call(rbind, draws %>% t))

draws <- map_df(draws, ~unlist(.), .id = "variable") %>% as.matrix()



# draws2 <- 
#   readRDS(paste0(
#     "raw-data/draws_forcast_levels_",
#     election,
#     "_",
#     cutoff,
#     ".RDS"
#   ))

# Get the correct current draws here

# forecast <- draws$forecast

# dim(draws2$forecast)

# str(draws2$forecast)

#nsim <- nrow(forecast)

nsim <- 500
adjustOrder <-
  match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "bsw", "oth"),
        colnames(draws))

forecast <- draws[1:3000, adjustOrder]


colnames(forecast) <-
  c("CDU/CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")


res_el <- btw_bund_res[paste0(election_l1),]


# str(draws2$forecast)
# str(forecast)

sim.swing <- -sweep(-forecast, 2, -res_el)
sim.prop.swing <- t(apply(sim.swing, 1, function(x)
  x / res_el))

zs_pred <- matrix(0, nrow = nrow(test), ncol = nrow(sim.swing))


for (i in 1:nrow(test)) {
  if (test[i, "partei"] %in% colnames(sim.prop.swing)) {
    zs_pred[i, ] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, colnames(sim.prop.swing) %in% test[i, "partei"]] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
  if (test[i, "partei"] == "CSU") {
    zs_pred[i, ] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, "CDU"] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
  
  if (!(test[i, "partei"] %in% colnames(sim.prop.swing)) &
      test[i, "partei"] != "CSU") {
    zs_pred[i, ] <-
      test[i, "res_l1_Z"] + sim.prop.swing[, "And"] * test[i, "res_l1_Z"] * (test[i, "weight"])
  }
}




test_sim <-
  data.frame(
    test$resp_E,
    # test$ncand,
    # test$propPlatz,
    zs_pred[, 2],
    test$res_l1_E,
    # test$formercand,
    # test$east,
    # test$female,
    # test$incumbent,
    # test$akad,
    # test$incumbent_in_wkr
    test$incumbent_party,
    test$no_cand_l1
  )




colnames(test_sim) <-
  c(
    "resp_E",
    # "ncand",
    # "propPlatz",
    "resp_Z",
    "res_l1_E",
    # "formercand",
    # "east",
    # "female",
    # "incumbent",
    # "akad",
    # "incumbent_in_wkr",
    "incumbent_party",
    "no_cand_l1"
  )

test_y <- test_sim$resp_E

vals <-
  t(sapply(aggregate(test, list(test$wkr), function(x)
    x)$partei, '[', seq(max(
      sapply(aggregate(test, list(test$wkr), function(x)
        x)$partei, length)
    ))))

res_list <- vector("list", length = nsim)

# Make a matrix that is nsim wide and nrow(test) tall
res_pred <- matrix(0, nrow = nrow(test), ncol = nsim)

for (zsim in 1:nsim) {
  test_sim$resp_Z <- zs_pred[, zsim]
  
  
  test_x <- as.matrix(test_sim[, 2:(ncol(test_sim))])
  
  
  
  reg_test_mat <- cbind(1, test_x)
  
  reg_preds <-
    reg_test_mat %*% t(S) + matrix(rnorm(nrow(reg_test_mat) * mu_nsim, 0, sd(residuals(reg))),
                                   nrow = nrow(reg_test_mat),
                                   ncol = mu_nsim)
  
  rmse_reg <- apply(reg_preds, 2, rmse, obs = test_y)
  
  reg_preds[reg_preds < 0] <- 0
  
  for (j in 1:299) {
    reg_preds[test$wkr == j,] <-
      sweep(reg_preds[test$wkr == j, ], 2, colSums(reg_preds[test$wkr == j,]), "/")
  }
  
  
  
  tmp_reg_winner <- future_lapply(1:mu_nsim, function(x) {
    select <-
      cbind(1:299, unlist(lapply(
        aggregate(-reg_preds[, x], list(test$wkr), order)[, 2], `[[`, 1
      )))
    
    vals[select]
  })
  
  res_list[[zsim]] <-
    list(
      # winner_nn = tmp_winner,
      winner_reg = tmp_reg_winner,
      # rmse_nn = rmse_nn,
      rmse_reg = rmse_reg
    )
  cat(zsim, "of", nsim, "\n")
  
  res_pred[, zsim] <- rowMeans(reg_preds)
  
}

# reg_pred <-
#   matrix(
#     unlist(lapply(res_list, "[[", "reg_pred")),
#     nrow = nsim * mu_nsim,
#     ncol = 299,
#     byrow = T
#   )

# Calculate the 17% (lower) and 83% (upper) quantiles for each row
test$low  <- round(apply(res_pred, 1, function(x) quantile(x, probs = 0.17))*100, 1)  # 17% quantile
test$high <- round(apply(res_pred, 1, function(x) quantile(x, probs = 0.83))*100, 1)  # 83% quantile

# Calculate the mean predicted value for each row
test$value <- round(rowMeans(res_pred)*100, 1)

# Calculate the zs_pred (mean across simulations for each candidate)
test$zs_pred <- rowMeans(zs_pred)


# Initialize vectors to store the results for each row
test$winner <- FALSE
test$probability <- NA

# Loop through each unique district (wkr) in the test data
for (district in unique(test$wkr)) {
  
  # Filter res_pred for the specific district (wkr == district)
  district_predictions <- res_pred[test$wkr == district, ]
  
  # Apply 'which.max' to each column (simulation) to find the row (candidate) with the highest value
  winners_per_simulation <- apply(district_predictions, 2, which.max)
  
  
  # Find the most frequent winner across simulations
  winner_frequency <- table(winners_per_simulation)
  most_frequent_winner <- as.numeric(names(winner_frequency)[which.max(winner_frequency)])
  
  # For each district, calculate the share of wins for each candidate (row)
  
  # Initialize a vector to store the probability (share of wins) for each candidate
  probabilities <- numeric(nrow(district_predictions))
  
  # Loop through each row (candidate) in district_predictions
  for (i in 1:nrow(district_predictions)) {
    
    # Count how many times this candidate (row) wins across all simulations
    candidate_wins <- sum(winners_per_simulation == i)
    
    # Calculate the probability (share of wins) for this candidate
    probabilities[i] <- candidate_wins / length(winners_per_simulation)
  }
  
  # View the probabilities (share of wins) for each candidate
  probabilities
  
  
  
  # Assign TRUE to the rows corresponding to the most frequent winner within this district
  test$winner[test$wkr == district] <- (1:nrow(district_predictions) == most_frequent_winner)
  
  # Assign the probability (share of wins) for each row (candidate) in the district
  test$probability[test$wkr == district] <- round(probabilities*100, 0)
}

# View the results
# head(test)

test$winner <- as.numeric(test$winner)

# Make a party var with different names c("cdu", "spd", "lin", "gru", "fdp", "afd", "bsw", "oth"), based on partei which is c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")
test$party <- ifelse(test$partei == "CDU/CSU", "cdu",
                     ifelse(test$partei == "SPD", "spd",
                            ifelse(test$partei == "LINKE", "lin",
                                   ifelse(test$partei == "GRUENE", "gru",
                                          ifelse(test$partei == "FDP", "fdp",
                                                 ifelse(test$partei == "AFD", "afd",
                                                        ifelse(test$partei == "BSW", "bsw", "oth")))))))

# test$partei[test$partei == "CDU"] <- "CDU/CSU"
# test$partei[test$partei == "CSU"] <- "CDU/CSU"

test$partei[test$partei == "AFD"] <- "AfD"

test$partei[test$partei == "GRUENE"] <- "Grüne"

test$partei[test$partei == "LINKE"] <- "Linke"

# test <- test %>% group_by(wkr) %>% mutate(winner = as.numeric(resp_E_pred == max(resp_E_pred)))

# View(test %>% dplyr::select(wkr, wkr_name, party, partei, winner, probability, value, low, high))
test$partei %>% unique

test <- filter(test, partei != "AND")

View(test %>% dplyr::select(wkr, wkr_name, party, partei, winner, probability, value, low, high, res_l1_E, res_l1_Z, zs_pred))


test <- test %>% dplyr::select(wkr, wkr_name, land, party, partei, winner, probability, value, low, high)


# Write into api folder as rds

party_colors <- c(
  "CDU/CSU" = "#000000",
  "SPD" = "#DD0000",
  "Grüne" = "#4C9A2A",
  "FDP" = "#FFCC00",
  "AfD" = "#009EE0",
  "Linke" = "purple",
  "BSW" = "#FF6A13"
)

test$color <- party_colors[test$partei]

# Output the forecast data for API
message("Saving the district forecast for API.")
saveRDS(test, file = str_c("output/districts/forecast_districts_", Sys.Date() ,".rds"))
saveRDS(test, file = "api/forecast_districts.rds")


# (test %>% filter(winner == 1))$party %>% table

# View(test %>% dplyr::select(resp_E_pred, res_l1_E, res_l1_Z, zs_pred, election, wkr, partei, wkr_name))
# saveRDS(res_co_el, "../processed-data/final_res_09_17.RDS")