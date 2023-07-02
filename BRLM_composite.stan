data {
  int<lower=0> N;
  int<lower=0> NCovariates;
  int<lower=0> Nmeasurements;
  matrix[N, Nmeasurements] Measurements;
  matrix[N, NCovariates] Covariates;
  vector[N] ASSQ;
}
transformed data{
}
parameters {
  real Intercept;
  real CompBeta;
  vector[N] Composite;

  vector[NCovariates] Coefficients;
  real<lower=0> sigmaASSQ;
  vector[Nmeasurements] Loadings;

}
transformed parameters {

}
model {

  Intercept ~ normal(0, 1);

  Loadings ~ normal(0, 1);
  Composite ~ normal(Measurements * Loadings, 1);
  CompBeta ~ normal(0, 4);

  Coefficients ~ normal(0, 1);
  sigmaASSQ ~ exponential(1);
  
  ASSQ ~ normal( Intercept + CompBeta * Composite + ( Covariates * Coefficients ), sigmaASSQ);

}





generated quantities {

}
