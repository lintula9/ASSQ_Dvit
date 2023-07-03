// Composite modeling gives boring results, but showcases that there really is no strong evidence for huge differences
// between the measurement points: Perhaps suggesting that the additive model works best.


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

  Q_ast = qr_thin_Q(Measurements) * sqrt(N - 1);
  R_ast = qr_thin_R(Measurements) / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
  

}
parameters {
  real<upper=0> CompBeta;
  vector[Nmeasurements] Loadings;
  vector[NCovariates] Coefficients;
  
  real Intercept;
  real<lower=0> sigmaASSQ;
}

transformed parameters {

}
model {

  Intercept ~ normal(0, .5);
  Loadings ~ normal(0, 1);
  CompBeta ~ normal(0, .5);
  Coefficients ~ normal(0, .5);
  sigmaASSQ ~ exponential(1);

  ASSQ ~ normal( Intercept + CompBeta * ( Measurements * Loadings ) + 
  Covariates * Coefficients, sigmaASSQ );
  
  real LoadingsNorm;
  LoadingsNorm = sqrt(Loadings' * Loadings); // Euclidean norm
  target += normal_lpdf( LoadingsNorm | 0, .5); // That adds to the likelihood, resulting in regularization of loadings.


}

generated quantities {
  vector[Nmeasurements] ScaledLoadings;
  ScaledLoadings = R_ast_inverse * Loadings; // coefficients on x
}
