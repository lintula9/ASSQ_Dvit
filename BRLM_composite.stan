data {
  int<lower=0> N;
  int<lower=0> NCovariates;
  int<lower=0> Nmeasurements;
  matrix[N, Nmeasurements] Measurements;
  matrix[N, NCovariates] Covariates;
  vector[N] ASSQ;
}
transformed data{
  matrix[N, Nmeasurements] Q_ast;
  matrix[Nmeasurements, Nmeasurements] R_ast;
  matrix[Nmeasurements, Nmeasurements] R_ast_inverse;

  Q_ast = qr_Q(Measurements)[, 1:Nmeasurements] * sqrt(N - 1);
  R_ast = qr_R(Measurements)[1:Nmeasurements, ] / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
}
parameters {
  real Intercept;
  real CompBeta;
  vector[Nmeasurements] Loadings;
  vector[N] Composite;

  real<lower=0> sigmaASSQ;
}

transformed parameters {

}
model {

  Intercept ~ normal(0, .5);
  Loadings ~ normal(0, .5);
  Composite ~ normal(Q_ast * Loadings, 1);
  CompBeta ~ normal(0, .5);

  sigmaASSQ ~ exponential(1);
  
  ASSQ ~ normal( Intercept + CompBeta * Composite, sigmaASSQ );

}





generated quantities {
  vector[Nmeasurements] ScaledLoadings;
  ScaledLoadings = R_ast_inverse * Loadings; // coefficients on x
}
