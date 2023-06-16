// Stancode for BRLM.

data {
  int<lower=0> N;
  int<lower=0> Ncovariates;
  int Nmeasurements;
  
  matrix[N, Nmeasurements] Measurements;
  matirx[N, Ncovariates] Covariates;
  vector[N] ASSQ;
  vector[Nmeasurements] alpha;
  vector[Nmeasurements] ExpCriticalWeights;
  vector[Nmeasurements] ExpAccumulationWeights;
}


parameters {
  real Intercept;
  real Delta;
  simplex[Nmeasurements] Weights;
  vector[Ncovariates] Coefficients;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] LinearFunction;
  LinearFunction = Intercept + Delta * (Measurements*Weights) + (Covariates * Coefficients)
}

model {
  Intercept ~ normal(0, .5);
  Weights ~ dirichlet(alpha);
  Delta ~ normal(0, .5);
  sigma ~ exponential(1);
  for(i in 1:Ncovariates){
    Coefficients[i] ~ normal(0, .5); 
  }
  
  ASSQ ~ normal(LinearFunction, sigma);


}

generated quantities {
  vector[Nmeasurements+1] EuclideanDistances;
  vector[N] logLikelihood;
  vector[N] 
  
}