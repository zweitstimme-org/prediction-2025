data {
  int<lower=0> LA;                   // Number of segments
  int<lower=0> L;                    // Number of hierarchical levels
  int<lower=0> Nobs;                 // Number of observed data points
  int<lower=0> Nmis;                 // Number of missing data points
  int<lower=0> N;                    // Total number of data points
  int<lower=0> election[N];          // Hierarchical index for elections
  int<lower=0> K;                    // Number of predictors
  int<lower=0> nParties;             // Number of parties
  int<lower=1, upper=Nobs+Nmis> ii_obs[Nobs];
  int<lower=1, upper=Nobs+Nmis> ii_mis[Nmis];
  vector[Nobs] y_obs;                // Observed data
  matrix[N, K] x;                    // Predictor matrix
  int s[LA];                         // Segment sizes for Dirichlet distribution
}
parameters {
  simplex[Nmis] y_mis;               // Missing data in simplex space
  vector[L-1] z_b0;                  // Non-centered b0 innovations
  matrix[L-1, K] z_b;                // Non-centered b innovations
  real<lower=0> sigma_b0;            // Std. dev. for b0
  vector<lower=0>[K] sigma_b;        // Std. dev. for b predictors
  real drift_b0;                     // Drift for b0
  vector[K] drift;                   // Drift for b predictors
  real b0_initial;                   // Initial value for b0
  vector[K] b_initial;               // Initial values for b
}
transformed parameters {
  vector[L] b0;                      // Hierarchical intercepts
  matrix[L, K] b;                    // Hierarchical slopes
  vector[N] y;
  vector[N] a;

  // Random walk for intercepts
  b0[1] = b0_initial; // Initial value for b0
  for (j in 2:L)
    b0[j] = b0[j-1] + drift_b0 + z_b0[j-1] * sigma_b0;
  
  // Random walk for slopes
  for (k in 1:K)
    b[1, k] = b_initial[k]; // Initial values for b predictors
  
  for (k in 1:K) {
    for (j in 2:L)
      b[j, k] = b[j-1, k] + drift[k] + z_b[j-1, k] * sigma_b[k];
  }

  // Combine observed and missing y
  y[ii_obs] = y_obs;
  y[ii_mis] = y_mis;

  // Exponentiated linear predictor
  for (i in 1:N)
    a[i] = exp(b0[election[i]] + b[election[i],] * x[i,]');
}
model {
  int pos;
  pos = 1;

  // Priors for hierarchical structure
  b0_initial ~ normal(0, 1);         // Prior for initial intercept
  b_initial ~ normal(0, 1);         // Priors for initial slopes
  z_b0 ~ normal(0, 1);              // Standard normal for innovations
  drift_b0 ~ normal(0, 1);          // Drift prior for intercept
  drift ~ normal(0, 1);             // Drift prior for slopes
  sigma_b0 ~ normal(0, 1);          // Std. dev. prior for intercept
  sigma_b ~ normal(0, 1);           // Std. dev. prior for slopes


  for (l in 1:LA) {
    z_b[l,] ~ normal(0, 10);
  }

  // Dirichlet likelihood
  for (l in 1:LA) {
    vector[s[l]] pi = softmax(segment(a, pos, s[l])); // Stabilized softmax
    segment(y, pos, s[l]) ~ dirichlet(pi);           // Dirichlet distribution
    pos += s[l];
  }
}
