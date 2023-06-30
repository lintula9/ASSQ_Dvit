data {
  int<lower=0> N;
  int<lower=0> NCovariates;
  int<lower=0> Nmeasurements;
  matrix[N, Nmeasurements] Measurements;
  matrix[N, NCovariates] Covariates;
  vector[N] ASSQ;
  vector[Nmeasurements] alpha;
  vector[Nmeasurements] ExpSensitiveWeights[Nmeasurements];
  vector[Nmeasurements] ExpAccumulationWeights;
  vector[Nmeasurements] ExpCriticalChildhoodWeights;
}


parameters {
  real Intercept;
  cholesky_factor_corr[Nmeasurements] Omega_L; // Define cholesky decompositions for corrmat.
  vector<lower=0>[Nmeasurements] sigma_L;
  vector[NCovariates] Coefficients;
  real<lower=0> sigma;
  
  vector[Nmeasurements] Loadings;
  vector[N] Composite;

}

transformed parameters {
  vector[N] LinearFunction;
  MeanASSQ = Intercept + Composite + (Covariates * Coefficients);
}

model {
  Intercept ~ normal(0, .5);
  
  matrix[Nmeasurements, Nmeasurements] Sigma_L; // Sigma for loadings.
  Omega_L ~ lkj_corr_cholesky(4); // Weak prior for cholesky decomposed corrs.
  sigma_L ~ cauchy(0, 2.5); // Weak prior for standard deviations, used to rescale.
  Sigma_L = diag_pre_multiply(sigma_L, Omega_L); // Rescale the cholesky decomposed corrs.
  Loadings ~ multi_normal(0, Sigma_L);
  
  Composite ~ normal(Measurements * Loadings, 1)


  for(i in 1:NCovariates){
    Coefficients[i] ~ normal(0, .5); 
  }
  
  
  sigma ~ exponential(1);
  ASSQ ~ normal(MeanASSQ, sigma);

}





generated quantities {
  vector[ N ] logLikelihood;
  vector[ N ] yrep;
  vector[ N ] residuals;

  for( n in 1:N ) {
    logLikelihood[ n ] = normal_lpdf( ASSQ[ n ] | MeanASSQ[ n ], sigma );
    yrep[ n ] = normal_rng( MeanASSQ[ n ], sigma );
    residuals[ n ] = yrep[ n ] - ASSQ[ n ];
  }
}
