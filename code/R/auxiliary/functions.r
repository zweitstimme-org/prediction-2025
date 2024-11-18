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


get_wahlrecht_polls <- function() {
  require(xml2)
  require(magrittr)
  data <-
    xml2::read_xml("https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml")
  
  
  wahlrecht_polls <- data %>% xml_find_all("//umfrage")
  test <- as_list(wahlrecht_polls)
  
  test <- lapply(test, unlist)
  columns <-
    c("dat", "inst", "bfrg", paste0(
      "werte.",
      c("cxu", "spd", "grn", "fdp", "fpd", "lnk", "afd", "son")
    ))
  
  no_bfrg <- which(!unlist(lapply(test, function(x) "bfrg" %in% names(x))))
  
  for(i in no_bfrg){
    
    test[[no_bfrg]]["bfrg"] <- NA
    
  }
  
  wahlrecht_df <- do.call(rbind, lapply(test, function(x) {
    match_vec <- na.omit(match(columns, names(x)))
    x[match_vec]
  }))
  
  df <- data.frame(wahlrecht_df)
  colnames(df) <-
    c("date",
      "institute",
      "sample_size",
      "cdu",
      "spd",
      "gru",
      "fdp",
      "lin",
      "afd",
      "oth")
  
  df[3:10] <- lapply(df[3:10], as.numeric)
  df$date <- as.Date(df$date)
  return(df)
}