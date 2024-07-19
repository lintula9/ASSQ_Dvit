//
// P -factor STAN

data {
  int<lower=0> N;                // number of observations
  int<lower=1> K;                // number of variables
  int<lower=2> J[K];             // number of categories for each ordinal variable
  int Y[N, K];                   // ordinal data matrix, with missing values as -1
  int<lower=0> N_mis;            // number of missing values
  int<lower=1,upper=N> ii[N_mis];  // row indices for missing values
  int<lower=1,upper=K> jj[N_mis];  // column indices for missing values
}

parameters {
  vector[K] lambda;                // factor loadings
  vector[N] eta;                   // latent factor
  ordered[max(J)-1] thresholds[K]; // thresholds for ordinal variables
  vector[N_mis] Y_latent_mis; // missing values as parameters
}

model {
  // Priors
  lambda ~ normal(0, 0.1);         // more informative priors on factor loadings
  eta ~ normal(0, 1);              // standard normal priors on latent factors
  for (k in 1:K) {
    thresholds[k][1:(J[k]-1)] ~ normal(0, 1);  // standard normal priors on thresholds
  }

  // Likelihood for observed data
  for (n in 1:N) {
    for (k in 1:K) {
      if (Y[n, k] != -1) { // observed data
        int nk;
        nk = J[k] - 1;  // number of thresholds for variable k
        Y[n, k] ~ ordered_ogive(lambda[k] * eta[n], thresholds[k][1:nk]);
      }
    }
  }

  // Likelihood for missing data
  for (m in 1:N_mis) {
    int n = ii[m];
    int k = jj[m];
    int nk = J[k] - 1;  // number of thresholds for variable k
    Y_latent_mis[m] ~ logistic(lambda[k] * eta[n], 1);
  
}
}

