data {
  int<lower=0> LA;
  int<lower=0> L;
  int<lower=0> Nobs;
  int<lower=0> Nmis;
  int<lower=0> N;
  int<lower=0> election[N];
  int<lower=0> K;
  int<lower=0> nParties;
  int<lower = 1, upper = Nobs + Nmis> ii_obs[Nobs];
  int<lower = 1, upper = Nobs + Nmis> ii_mis[Nmis];
  vector[Nobs] y_obs;
  matrix[N,K] x;
  int s[LA];    
}
parameters {
  simplex[Nmis] y_mis;
  vector[L] b0;
  matrix[L,K] b;
  real<lower=0> sigma_b0;
  real<lower=0> sigma_b[K];
  real drift_b0;
  real drift[K];  
}
transformed parameters {
  vector[N] y;
  vector[N] a;

  real mu_b0[L];
  matrix[L,K] mu_b;

  y[ii_obs] = y_obs;
  y[ii_mis] = y_mis;
  
  for (i in 1:N)
    a[i] = exp(b0[election[i]] + b[election[i],] * x[i,]');
  
  mu_b0[1] = 0;
  for (k in 1:K)
    mu_b[1,k] = 0;

  for (j in 2:L)
    mu_b0[j] = b0[j-1] + drift_b0;

  for (k in 1:K)
    for(j in 2:L)
      mu_b[j,k] = b[j-1,k] + drift[k];
}
model {
  int pos; 
  pos = 1;

  b0[1] ~ normal(0, 10); // initialization
  drift_b0 ~ normal(0, 10);
  sigma_b0 ~ normal(0, 1);

  for (j in 2:L)
    b0[j] ~ normal(mu_b0[j], sigma_b0);

  for (k in 1:K) {
    b[1,k] ~ normal(0, 10);
    drift[k] ~ normal(0, 10);
    sigma_b[k] ~ normal(0, 1);
  }

  for (k in 1:K)
    for(j in 2:L)
      b[j,k] ~ normal(mu_b[j,k], sigma_b[k]);
  
  for (l in 1:LA) {
    segment(y, pos, s[l]) ~ dirichlet(segment(a, pos, s[l]));
    pos = pos + s[l];
  }
}
