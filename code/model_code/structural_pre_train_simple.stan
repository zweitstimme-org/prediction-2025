data {
  int<lower=0> LA; // Number of elections
  int<lower=0> L; // Number of elections (not used for drift now, but retained for structure)
  int<lower=0> Nobs; // Number of observed results
  int<lower=0> Nmis; // Number of missing results
  int<lower=0> N; // Total number of results
  int<lower=0> election[N]; // Election IDs
  int<lower=0> K; // Number of covariates
  int<lower=0> nParties; // Number of parties
  int<lower=1, upper=Nobs + Nmis> ii_obs[Nobs]; // Indices for observed results
  int<lower=1, upper=Nobs + Nmis> ii_mis[Nmis]; // Indices for missing results
  vector[Nobs] y_obs; // Observed vote shares
  matrix[N, K] x; // Covariates matrix
  int s[LA]; // Number of parties per election
}

parameters {
  simplex[Nmis] y_mis; // Missing vote shares
  real b0; // Single intercept for all elections
  vector[K] b; // Single set of regression coefficients for all elections
}

transformed parameters {
  vector[N] y; // Combined observed and missing vote shares
  vector[N] a; // Structural component (linear predictor)

  // Combine observed and missing vote shares
  y[ii_obs] = y_obs;
  y[ii_mis] = y_mis;

  // Compute structural component
  for (i in 1:N)
    a[i] = exp(b0 + dot_product(b, x[i, ]));
}

model {
  int pos = 1; // Auxiliary counter for Dirichlet likelihood

  // Dirichlet likelihood for vote shares
  for (l in 1:LA) {
    segment(y, pos, s[l]) ~ dirichlet(segment(a, pos, s[l]));
    pos += s[l];
  }
}
