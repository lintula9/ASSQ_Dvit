data {
  int<lower=0> N;
  int<lower=0> NCovariates;
  int<lower=0> Nmeasurements;
  matrix[N, Nmeasurements] Measurements;
  matrix[N, NCovariates] Covariates;
  vector[N] ASSQ;
  vector[Nmeasurements] alpha;
  vector[Nmeasurements] ExpCriticalWeights[Nmeasurements];
  vector[Nmeasurements] ExpAccumulationWeights;
}


parameters {
  real Intercept;
  real Delta;
  simplex[Nmeasurements] Weights;
  vector[NCovariates] Coefficients;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] LinearFunction;
  LinearFunction = Intercept + Delta * (Measurements * Weights) + (Covariates * Coefficients);
}

model {
  Intercept ~ normal(0, .5);
  Weights ~ dirichlet(alpha);
  Delta ~ normal(0, .5);
  sigma ~ exponential(1);
  for(i in 1:NCovariates){
    Coefficients[i] ~ normal(0, .5); 
  }
  
  ASSQ ~ normal(LinearFunction, sigma);

}

generated quantities {
  vector[ Nmeasurements + 1 ] EuclideanDistances;
  vector[ N ] logLikelihood;
  vector[ N ] yrep;
  vector[ N ] residuals;
  
  for( i in 1:Nmeasurements){
    EuclideanDistances[ i ] = distance( Weights, ExpCriticalWeights[ i ] );
  }
  EuclideanDistances[ Nmeasurements + 1 ] = distance( Weights, ExpAccumulationWeights );
  
  for( n in 1:N ) {
    logLikelihood[ n ] = normal_lpdf( ASSQ[ n ] | LinearFunction[ n ], sigma );
    yrep[ n ] = normal_rng( LinearFunction[ n ], sigma );
    residuals[ n ] = yrep[ n ] - ASSQ[ n ];
  }
}
