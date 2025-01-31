

forecast_districts <- readRDS(str_c("output/districts/forecast_districts_2025-01-02.rds"))


# Make a table (wide format) of all district forecasts for Latex
forecast_districts %>% dplyr::select(wkr, wkr_name, party, value) %>% filter(party != "oth") %>% 
  pivot_wider(names_from = party, values_from = value) %>% 
  mutate(str_len_wkr_name = str_length(wkr_name)) %>% 
  mutate(wkr_name = ifelse(str_len_wkr_name > 10, str_c(substr(wkr_name, 1, 10), "..."), wkr_name)) %>% 
  dplyr::select(-str_len_wkr_name) %>%
  stargazer(summary = F, type = "latex", out = "output/tables/forecast_districts.tex", rownames = F)




forecast_districts$wkr_name[15] %>% str_length
# Make a table of vacant seat probabilities


pred_abandoned <- readRDS("output/districts/pred_abandoned.rds")


pred_abandoned %>% filter(abandoned) %>%  group_by(land, wkr, wkr_name, party) %>% mutate(n = n()/max(iteration)) %>%
  summarise(abandon_p = mean(n) %>% round(2),
            value_mean = mean(pred) %>% round(2)) %>% 
  mutate(str_len_wkr_name = str_length(wkr_name)) %>% 
  mutate(wkr_name = ifelse(str_len_wkr_name > 10, str_c(substr(wkr_name, 1, 10), "..."), wkr_name)) %>% 
  dplyr::select(-str_len_wkr_name) %>%
  arrange(-abandon_p) %>% 
  filter(abandon_p > 0.05) %>%
  stargazer(summary = F, type = "latex", out = "output/tables/vacant_districts.tex", rownames = F)

pred_abandoned %>% filter(abandoned) %>%  group_by(land, wkr, wkr_name) %>% mutate(n = n()/max(iteration)) %>%
  summarise(abandon_p = mean(n) %>% round(2),
            value_mean = mean(pred) %>% round(2)) %>% 
  mutate(str_len_wkr_name = str_length(wkr_name)) %>% 
  mutate(wkr_name = ifelse(str_len_wkr_name > 10, str_c(substr(wkr_name, 1, 10), "..."), wkr_name)) %>% 
  dplyr::select(-str_len_wkr_name) %>%
  arrange(-abandon_p) %>% 
  filter(abandon_p > 0.05) %>% View
