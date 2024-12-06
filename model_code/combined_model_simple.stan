data {
  // Data for dynamic component of the model
  int<lower=0> nParties; // Number of Parties
  int<lower=0> nInst; // Number of Polling Institutes
  int<lower=0> nPolls; // Number of published Polls
  int<lower=0> nPeriods; // Number of days the model should account for
  int<lower=0, upper = nInst> iid[nPolls]; // Numeric IDs of the Polling Institutes
  int<lower=0, upper = nPeriods> date[nPolls]; // Date (as days from 1 to nPeriods) when the Poll was published
  int<lower=0> y[nPolls,nParties]; // The poll results
  
  // Data for sturctural part of the model
  int<lower=0> LA; 
  int<lower=0> L;
  int<lower=0> Nobs; // Number of observed party-election results 
  int<lower=0> Nmis; // Number of "missing" party-election results (No. of parties in the upcoming election)
  int<lower=0> N; // Total Number of Observations
  int<lower=0> election[N]; // Election ID
  int<lower=0> K; // Number of covariates in structural model
  vector[Nobs] v_obs; // observed vote shares
  matrix[N,K] x; // Matrix of Covariates
  matrix[Nmis,K] xE; // Matrix of Covariates for upcoming election

  // Auxilliary data
  int<lower = 1, upper = Nobs + Nmis> ii_obs[Nobs]; // index for observed party-election results
  int<lower = 1, upper = Nobs + Nmis> ii_mis[Nmis]; // index for missing party-election results
  int s[LA]; // Number of parties per election
  
    
  real b0_prior; // Prior on Intercept
  vector[K] b_prior; // Prior on coefficients
}
parameters {

  // Paramters of structural model
  simplex[Nmis] v_mis; 
  simplex[nParties] vE;
  real b0;
  vector[K] b;

  // Paramters of Poll Model
  cholesky_factor_corr[nParties-1] S_cor; 
  cholesky_factor_corr[nParties-1] S_shock_cor; 
  vector<lower=0>[nParties-1] sigma_evo;
  vector<lower=0>[nParties-1] sigma_shock;
  vector[nParties-1] alphastarforecast;
  matrix[nPeriods-1,nParties-1] alphastar;
  matrix[nInst-1,nParties-1] house_effect_raw;
}


transformed parameters {
  
  
  // Fundamnetals Model
  vector[N] v;
  vector[N] a;
  vector[nParties] a_pred;

  // Dynmic Polls Model
  matrix[nPeriods,nParties] alphastar_prior;
  vector[nParties] alphastarforecast_prior;
  matrix[nInst,nParties] house_effect;
  matrix[nPeriods,nParties] alpha;
  matrix[nPeriods,nParties] ea;
 
  // Combined
  vector[nParties] ef;
  vector[nParties] forecast;
  

  alphastar_prior[1:(nPeriods-1),1:(nParties-1)] = alphastar;
  alphastarforecast_prior[1:nParties-1] = alphastarforecast;
  
  house_effect[2:nInst, 2:nParties] = house_effect_raw;
  
  for (j in 2:nParties)
          house_effect[1, j] = 0 - sum(house_effect[2:nInst, j]);
      for(c in 1:nInst)  
          house_effect[c, 1] = 0 - sum(house_effect[c, 2:nParties] );
      

  v[ii_obs] = v_obs;
  v[ii_mis] = v_mis;
  
  for (i in 1:N)
    a[i] = exp(b0 + dot_product(b, x[i, ]));

  for (j in 1:nParties)
    a_pred[j] = exp(b0 + dot_product(b, xE[j, ]));

  // Transform structural forecast to log-ratio
  for (j in 1:(nParties))
    alphastar_prior[nPeriods,j] = log(vE[j]/vE[nParties]);

  for (i in 1:(nPeriods-1)) 
    alphastar_prior[i,nParties] = 0;   
    
  for(k in 1:nPolls)
    alphastar_prior[date[k],] = (alphastar_prior[date[k],]  + house_effect[iid[k],]);  
  

  alphastarforecast_prior[nParties] = 0;  

  // Transform values from log-ratio space back to vote shares
  for (i in 1:(nPeriods)) 
    for (j in 1:(nParties)) 
      ea[i,j] = exp(alphastar_prior[i,j]);

  for (i in 1:(nPeriods)) 
    for (j in 1:(nParties)) 
      alpha[i,j] = ea[i,j]/sum(ea[i,]);   

  for (j in 1:(nParties))
    ef[j] = exp(alphastarforecast_prior[j]);
  
  for (j in 1:(nParties))   
    forecast[j] = ef[j]/sum(ef[1:nParties]);
    

               
}
model {
  
  int pos; // Auxilliary counter
  pos = 1; // Pre-set counter to 1

  // Structural component of the model
  b0 ~ normal(b0_prior, 1); // Initialization of b0 in the structural component
  for (k in 1:K){
    b ~ normal(b_prior[k], 1);
  }
  
  
  for (l in 1:LA) {
    segment(v, pos, s[l]) ~ dirichlet(segment(a, pos, s[l]));
    pos = pos + s[l];
  }

  vE ~ dirichlet(a_pred);

  // Dynamic component of the model

  // Evolution variance prior
  sigma_evo ~ normal(0, 0.1);

  // Forecast variance shock prior
  sigma_shock ~ normal(0, 0.18);

  // Evolution covariance prior
  S_cor ~ lkj_corr_cholesky(50);

  // Forecast covariance prior
  S_shock_cor ~ lkj_corr_cholesky(100);

  // Backward Random Walk of latent support (in log-ratio space)
  for (i in 1:(nPeriods-1)){
    alphastar[i,1:(nParties-1)] ~ multi_normal_cholesky(alphastar_prior[(i+1),1:(nParties-1)], diag_pre_multiply(sigma_evo, S_cor)); 
    }

  // Add forecast variance schock 
  alphastarforecast[1:(nParties-1)] ~ multi_normal_cholesky(alphastar_prior[nPeriods, 1:(nParties-1)], diag_pre_multiply(sigma_shock, S_shock_cor));

  // Model Polls based on latent state (in vote share space)
  for (k in 1:nPolls)
    y[k,1:nParties] ~ multinomial(alpha[date[k],]'); 


  // Estimate house effects
  for (j in 1:(nParties-1)) 
        for (c in 1:(nInst-1))
          house_effect_raw[c, j] ~ normal(0, 0.001); // Prior 1 percent point sd
         
  
}