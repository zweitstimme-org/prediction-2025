
p_needed <- c("haven", "lubridate", "stringr", "ggplot2","dlm","plyr", "dplyr", 
			   "magrittr", "broom", "tidyr",  "stringr", "reshape2", "rjags", 
			   "runjags", "readr", "rvest", "mcmcplots", "parallel", "rstan", 
			   "shinystan", "superdiag", "xtable", "knitr")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install, dependencies = TRUE)
}
return(print(sapply(p_needed, require, character.only = TRUE)))

