---
title: "Readme"
output: html_document
date: "2025-01-30"
---

# Replication Materials: [The Zweitstimme Forecast for the German Federal Election 2025]

This repository contains replication materials for "[The Zweitstimme Forecast for the German Federal Election 2025]".

## Repository Structure

### Data Files

#### Raw Data
- `./data/btw_2021_kerg2.csv`: 2021 German federal election data
- `./data/btw_candidates_1983-2025.csv`: Historical candidate data from 1983-2025 (csv format)
- `./data/btw_candidates_1983-2025.RData`: Historical candidate data from 1983-2025 (R format)
- `./data/btw25_geometrie_wahlkreise_shp/*`: Shapefile containing electoral district geometries
- `./data/pre_train_data_21.rds`: Pre-training model data including elections until 2021
- `./data/germany-federal-polls.csv`: Federal polling data
- `./data/germany-state-elections.csv`: State election results
- `./data/germany-state-polls.csv`: State-level polling data

#### Generated Data
- `./output/forecast_draws_2025-01-30.rds`: Model forecast draws (2025-01-30)
- `./output/forecast_party_vote.rds`: Party vote forecasts  (2025-01-30)
- `./output/district_reg_predictions.rds`: District-level predictions  (2025-01-30)
- `./output/prediction_data_districts.rds`: Prediction data for districts from regession (predicted party vote for each district)
- `./output/pred_probabilities.rds`: Probability calculations for scenarios (e.g. coalition majorities)
### Code Files

The analysis can be reproduced by running the scripts in the following order:

`./auxiliary/functions.R`: Helper functions
`./auxiliary/packages.r`: Install and load required R packages

`./code/00_run-model.R`: Main script to execute the full analysis

1. `./code/01_prepare-data.R`: Data preparation and cleaning for pre-training model
2. `./code/02_ger_structural_pre_train_stan.R`: Structural model for party vote
3. `./code/03_ger_combined_model_stan.R`: Combined forecasting model for party vote
4. `./code/04_party-vote-data.R`: Processing second vote forecast data
5. `./code/05_party-vote-figures.R`: Creating figures for party vote analysis
6. `./code/06_district-model.R`: Electoral district forecasting model
7. `./code/07_vacant-seats.R`: Analysis of potential vacant seats
8. `./code/08_district-figures.R`: Creating district-level figures
9. `./code/09_probabilities.R`: Probability calculations for scenarios (e.g. coalition majorities)

`./code/A_descriptives.R`: Descriptives

### Model Code
- `./model_code/combined_model_simple.stan`: Combined forecasting model specification
- `./model_code/structural_pre_train_simple.stan`: Structural model specification

### Output Files

#### Figures
- `./output/plots/figure_forecast_party_vote.pdf`: Main forecast visualization (Figure 1)
- `./output/plots/figure_forecast_party_vote.png`: Main forecast visualization (png) (Figure 1)
- `./output/plots/figure_forecast_districts.png`: District-level predictions (Figure 2)
- `./output/plots/state-rmse.pdf`: State-level polling accuracy RMSE (Figure 3)
- `./output/plots/federal-rmse.pdf`: Federal-level polling accuracy RMSE (Figure 4)

#### Tables
- `./output/tables/district_reg.tex`: District model regression results (Table B.2)
- `./output/tables/forecast_districts.tex`: District forecasts (Table B.3)
- `./output/tables/vacant_districts.tex`: Vacant district analysis (Table B.4)

## Requirements

### Software Requirements
- R version 4.4.1 (2024-06-14)
- RStudio 2024.9.1.394

### Required R Packages
The analysis requires several R packages for data manipulation, visualization, and statistical modeling. All required packages are listed in `auxiliary/packages.r` and will be automatically installed and loaded when running the code. Key packages include:

- Data manipulation: tidyverse, dplyr, haven
- Statistical modeling: rstan, MASS
- Visualization: ggplot2, plotly
- Spatial analysis: sf
- Output formatting: stargazer, knitr

The complete list of dependencies will be installed automatically when running `auxiliary/packages.r`.