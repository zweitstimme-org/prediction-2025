# Configure Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Type conversion helper functions
char <- function(x) as.character(x)
num <- function(x) as.numeric(x)

#' Summarize MCMC results
#' 
#' Calculate summary statistics from JAGS MCMC matrix including means,
#' standard deviations, and various credible intervals
#' @param x MCMC matrix with parameters in columns
#' @return Data frame with summary statistics for each parameter
jags_summary <- function(x) {
  data.frame(
    var = colnames(x),
    mean = apply(x, 2, mean),
    sd = apply(x, 2, sd),
    q95lo = apply(x, 2, quantile, probs = 0.025),
    q95hi = apply(x, 2, quantile, probs = 0.975),
    q90lo = apply(x, 2, quantile, probs = 0.05),
    q90hi = apply(x, 2, quantile, probs = 0.95),
    q80lo = apply(x, 2, quantile, probs = 0.10),
    q80hi = apply(x, 2, quantile, probs = 0.90),
    stringsAsFactors = FALSE
  )
}

#' Check Coalition Majority
#' 
#' Determine if a coalition has a majority of votes among parties above threshold
#' @param share Vote shares for coalition parties
#' @param share_above_hurdle Total vote share of parties above threshold
#' @return Logical indicating if coalition has majority
coal_majo <- function(share, share_above_hurdle) {
  if(any(share < 0.05)) {
    return(FALSE)
  } else {
    return(sum(share)/share_above_hurdle > 0.5)
  }
}

#' Calculate Root Mean Square Error
#' @param pred Vector of predictions
#' @param obs Vector of observed values
#' @return RMSE value
rmse <- function(pred, obs) {
  sqrt(mean((pred - obs) ^ 2))
}

#' Calculate Log Ratio Transformation
#' 
#' Transform percentage values to log ratios, handling zero values
#' @param x Numeric vector of percentages
#' @return Vector of log ratios
log_ratio <- function(x) {
  x <- x / 100                           # Scale to proportions
  x <- ifelse(x == 0, x + 0.01, x)      # Handle zeros
  log(x/(1-x))                          # Calculate log ratio
}

# Function to extract data from a single XML document
wahlrecht_xml_extract <- function(xml_url) {
  require(xml2)
  
  # xml_url <- "https://www.wahlrecht.de/umfragen/xml/bundestag_archiv.xml"
  
  # Load the XML document as a list
  # xml_url <- "https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml"
  xml_doc <- as_list(read_xml(xml_url))
  
  # Unnest the <umfragen> node to get individual <umfrage> entries
  xml_df <- as_tibble(xml_doc) %>% unnest_wider(umfragen) %>% 
    unnest_longer(werte) %>% 
    unnest(id) %>% 
    unnest(typ) %>% 
    unnest(reg) %>% 
    unnest(dat) %>% 
    unnest(dat) %>% 
    unnest(inst) %>% 
    unnest(inst) %>% 
    unnest(werte) %>% 
    unnest(werte) %>% 
    unnest(bfrg) %>% 
    unnest(bfrg) %>% 
    unnest(rnd) %>% 
    unnest(rnd) %>% 
    unnest(beg) %>% 
    unnest(beg) %>% 
    unnest(bfrg) %>% 
    unnest(bfrg) %>% 
    unnest(end) %>% 
    unnest(end) %>% 
    unnest(meth) %>% 
    unnest(meth) %>% 
    # unnest(agart) %>% 
    # unnest(agart) %>% 
    unnest(stat) %>% 
    unnest(stat) %>% 
    unnest(stand) %>% 
    unnest(stand) %>% 
    mutate(werte_id = case_when(werte_id == "grn" ~ "gru",
                                werte_id == "cxu" ~ "cdu",
                                werte_id == "lnk" ~ "lin",
                                werte_id == "fpd" ~ "fdp",
                                T ~ werte_id),
           werte = as.numeric(werte)
    ) %>%
    filter(!(werte_id %in% c("son", "frw"))) %>% 
    rename(institute = inst, date = dat, party = werte_id, value = werte, sample_size = bfrg) %>% 
    as.data.frame %>% 
    mutate(date = as.Date(date)) %>%
    pivot_wider(values_from = "value", names_from = "party") 
  
  if("agart" %in% names(xml_df)) xml_df <- dplyr::select(xml_df, -agart)
  
  # unnest_wider(ag) %>% 
  
  # unnest(ag1) %>% 
  #   unnest(ag1) %>% 
  #   unnest(ag2) %>% 
  #   unnest(ag2) %>% 
  
  
  
  xml_df
}

get_wahlrecht_polls <- function() {
  
  # down <- get_surveys()
  # xml_url <- "https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml"
  
  new_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml") %>% wahlrecht_xml_extract
  old_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_archiv.xml") %>% wahlrecht_xml_extract
  
  old_polls$date %>% min
  
  wahlrecht_polls <- bind_rows(new_polls, old_polls)
  
  
  # sample_size
  wahlrecht_polls <- wahlrecht_polls %>% 
    # unnest(surveys) %>% 
    # unnest(survey) %>% 
    # select(institut = pollster, date, party, poll_share = percent, sample_size = respondents) %>%
    # mutate(date = ymd(date),
    #        party = case_when(party == "greens" ~ "gru",
    #                          party == "left" ~ "lin",
    #                          party == "others" ~ "oth",
    #                          TRUE ~ party)) %>%     
    # pivot_wider(names_from = party, values_from = poll_share) %>%
    pivot_longer(cols = cdu:afd, names_to = "party", values_to = "poll_share") %>% 
    # rename(institute = institut) %>% 
    pivot_wider(names_from = party, values_from = poll_share, values_fn = mean) 
  
  
  # If lin is not NA, subtract form oth
  # wahlrecht_polls <- wahlrecht_polls %>% 
  #   mutate(oth = ifelse(!is.na(lin), oth - lin, oth),
  #          oth = ifelse(!is.na(bsw), oth - bsw, oth),
  #          oth = ifelse(!is.na(fdp), oth - fdp, oth))
  
  # Arrange by date, decreasing
  wahlrecht_polls <- wahlrecht_polls %>% arrange(desc(date))
  
  wahlrecht_polls$sample_size <- as.numeric(wahlrecht_polls$sample_size)
  
  return(wahlrecht_polls)
}
