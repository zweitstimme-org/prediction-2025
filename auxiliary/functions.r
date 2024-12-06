# Stan options

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#
char <- function(x) as.character(x)
num <- function(x) as.numeric(x)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

recode_partynames <- function(x, longnames = FALSE) {
  require(stringr)
  x_recoded <- x %>% str_replace("cdu", "Union") %>%
    str_replace("fdp", "FDP") %>% 
    str_replace("spd", "SPD") %>%
    str_replace("gru", "Grüne") %>%
    str_replace("lin", "Linke") %>%
    str_replace("afd", "AfD") %>%
    str_replace("oth", "Andere")
  if(longnames == TRUE) {
    x_recoded <- x_recoded %>% str_replace("Grüne", "B'90/Die Grünen") %>% str_replace("Union", "CDU/CSU") %>% str_replace("Linke", "Die Linke")
  }
  x_recoded
}

recode_years <- function(x) {
  x_recoded <- x %>% str_replace("19|20", "'")
  x_recoded
}


# function to get quantities of interest from JAGS MCMC matrix
jags_summary <- function(x) {
  dat <- data.frame(var = colnames(x),
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
  dat
}


### Transform Mean and Variance from Normal Prior to Beta

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = c(alpha,beta))
}



### GGplot Functtions

# Create a laballer function


party_labeller <- function(variable,value){
  
  party_names <- list(
    'cdu'="CDU/CSU",
    'fdp'="FDP",
    'gru'="B90/Die Grünen",
    'lin'="Die Linke",
    'spd'="SPD",
    "afd"="AfD"
  )
  
  return(party_names[as.character(value)])
}

party_labeller_eng <- function(variable,value){
  
  party_names <- list(
    'cdu'="CDU/CSU",
    'fdp'="FDP",
    'gru'="A90/The Greens",
    'lin'="The Left",
    'spd'="SPD",
    "afd"="AfD"
  )
  
  return(party_names[as.character(value)])
}

### JAGS Estimation function ---------------------

est_jags_model <- function(poll_data=polls, ger_results = ger_polls_results,
                           model_file="polling-model-01-hlv-smoother.jags",
                           save_var=c("alpha","sigma"),
                           time_points, ElectionYear = 2013,
                           nBurn = 5000, nThin=10, nIter=10000,
                           certainty = 1,brw=F, forcast_mean, forcast_var,
                           forecast=F, gamma_value_t =NULL,sigma_value_t =NULL, until=NULL ){
  
  # Define Data for Filter 
  Y <- round(as.matrix(poll_data[,grep("cdu|spd|fdp|lin|gru|afd|other", names(poll_data))]/100) * 
               poll_data$sample_size) # Grep existing Parties and transform share to number
  NObs <- apply(Y,1,sum)
  party_names <- colnames(Y)  
  nParties <-ncol(Y)
  pvs <- ger_results %>% filter(year == ElectionYear, party %in% party_names) %>% 
    group_by(party) %>% 
    summarise(result = mean(share)) 
  pvs$result[which(pvs$result==0)] <- 0.0001
  
  # Transform priors using alr transformation
  getALR <- function(x, ref = length(x)){log(x[-ref]/x[ref])}
  
  # Create rjags object 
  if(forecast==T){
    
    forJags <- list(y = Y,
                    nParties = nParties,
                    nPeriods = until+2,
                    nPolls = nrow(Y),
                    date = poll_data$t,
                    size = NObs, 
                    previous_vote_share = c(pvs$result, 100-sum(pvs$result)),
                    gamma_prior = gamma_value_t,
                    sigma_prior = sigma_value_t, 
                    f = forcast_mean,
                    v = forcast_var
    ) }
    
  if(brw==T){
      
      forJags <- list(y = Y,
                      nParties = nParties,
                      nPeriods =  time_points+1,
                      nPolls = nrow(Y),
                      date = poll_data$t,
                      size = NObs, 
                      f = forcast_mean,
                      v = forcast_var
      )
      
    
    
  } 
    
    

  
  jags.mod <- jags.model(file = model_file,
                         data=forJags,
                         n.chains=2,
                         n.adapt=100)
  
  
  update(jags.mod,nBurn)
  jags.out <- coda.samples(jags.mod,
                           n.iter=nIter,thin=nThin,
                           variable.names=save_var)
  
  return(jags.out)
  
}


### Post Estimation: Jags results Plot function ----------------------------

plot_model_res <- function(jags.out , polls_data_long=polls_long, pn = party_names){
  
  df <- t(apply(as.matrix(jags.out),2,quantile,c(0.95,0.5,0.05)))
  
  df <- as.data.frame(cbind(df,str_split_fixed(rownames(df),"\\[|,|\\]",3)))
  names(df) <- c("high","mid","low","par","time","party")
  df[,c(1:3,5)] <- apply(df[,c(1:3,5)],2,as.numeric)
  levels(df$party) <- c("",pn)
  
  ggplot() + 
    geom_line(data=df[df$par=="alpha",],aes(x=(time),y=mid)) + 
    geom_ribbon(data=df[df$par=="alpha",],aes(x=(time),y=mid,ymin=low,ymax=high,fill=party),alpha=0.3) + facet_wrap(~party,scales="free") + theme_bw() +
    geom_point(data=polls_data_long,aes(x=t,y=support/100))
  
}


### Post Estimation: Create mean filtered values

create_df_mean <- function(dta = model_res, pn = party_names){
  
    mr <- as.matrix(dta)
    sel <- grep("alpha",colnames(mr))
    
    df <- apply(mr[,sel],2,mean)
    df <- as.data.frame(cbind(df,str_split_fixed(names(df),"\\[|,|\\]",3)))
    names(df) <- c("mean","par","time","party")
    
    df$mean <- as.numeric(as.character(df$mean))
    df$time <- as.numeric(as.character(df$time))
    levels(df$party) <- c(party_names)
    
    df$year <- ElectionYear
    
    return(df)
  
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
    
    if("agart" %in% names(xml_df)) xml_df <- select(xml_df, -agart)
      
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
  
  return(wahlrecht_polls)
}


get_wahlrecht_polls_old <- function() {
  require(coalitions)
  
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
    pivot_wider(names_from = party, values_from = poll_share, values_fn = mean) 
    # Make var oth which is 100 minus these vars cdu + spd + gru + lin + afd + fdp, but sometimes they are NA
  
  # Arrange by date, decreasing
  wahlrecht_polls <- wahlrecht_polls %>% arrange(desc(date))
  
  return(wahlrecht_polls)
}

coal_majo <- function(share, share_above_hurdle){
  
  if(any(share < 0.05)){
    return(FALSE)
  } else {
    return(sum(share)/share_above_hurdle > 0.5)
  }
  
}
