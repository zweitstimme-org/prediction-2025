# List of required packages
p_needed <- c(
  
  # Statistical modeling
  "dlm", "MASS", "rstan", "shinystan", "mcmcplots", "superdiag",
  
  # Data manipulation and import
  "haven", "lubridate", "stringr", "tidyverse", "plyr", "tidyr", "readr", "rio",
  "dplyr", "magrittr", "broom", "reshape2", "openxlsx",
  
  # Visualization
  "ggplot2", "plotly", "htmlwidgets", "patchwork",
  

  
  # Web scraping and API
  "rvest", "httr", "jsonlite",
  
  # Spatial data
  "sf",
  
  # Parallel processing
  "parallel", "future.apply",
  
  # Machine learning
  "keras",
  
  # Output formatting
  "xtable", "knitr", "stargazer",
  
  # String processing
  "stringi",
  
  "sf", "rmapshaper", "geojsonsf", "rjson"
)

# Install missing packages
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install, dependencies = TRUE)
}

# Load all packages and return loading status
return(print(sapply(p_needed, require, character.only = TRUE))) %in% packages

