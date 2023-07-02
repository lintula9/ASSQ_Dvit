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
  real<upper=0> CompBeta;
  real<lower=0> First; // Constrain the first to be positive, for identifiability.
  vector[Nmeasurements - 1] Others;
  vector[N] Composite;

  real<lower=0> sigmaASSQ;
}

transformed parameters {

}
model {

  First ~ normal(0, .5);
  Others ~ normal(0, .5);

  Intercept ~ normal(0, .5);
  Composite ~ normal(Q_ast * append_row(First, Others), 1);
  CompBeta ~ normal(0, .5);

  sigmaASSQ ~ exponential(1);
  
  ASSQ ~ normal( Intercept + CompBeta * Composite, sigmaASSQ );

}

generated quantities {
  vector[Nmeasurements] ScaledLoadings;
  ScaledLoadings = R_ast_inverse * append_row(First, Others); // coefficients on x
}
