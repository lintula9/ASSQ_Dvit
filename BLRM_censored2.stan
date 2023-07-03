data {
  
  int<lower=0> N;
  int<lower=0> NCovariates;
  int<lower=0> Nmeasurements;
  int zeroVal[N];
  matrix[N, Nmeasurements] Measurements;
  matrix[N, NCovariates] Covariates;
  vector[N] ASSQ;
  real<upper=min(ASSQ)> ASSQ_L;
  vector[Nmeasurements] alpha;
  vector[Nmeasurements] ExpSensitiveWeights[Nmeasurements];
  vector[Nmeasurements] ExpAccumulationWeights;
  vector[Nmeasurements] ExpCriticalChildhoodWeights;
}


parameters {
  real Intercept;
  real Delta;
  simplex[ Nmeasurements ] Weights;
  vector[ NCovariates ] Coefficients;
  real<lower=0> sigma;
}

transformed parameters {
  vector[ N ] Mu;
  Mu = Intercept + Delta * ( Measurements * Weights ) + ( Covariates * Coefficients);
}

model {
  Intercept ~ normal( 0, .5 );
  Weights ~ dirichlet( alpha );
  Delta ~ normal( 0, .5 );
  sigma ~ exponential( 1 );
  for( i in 1 : NCovariates ){
    Coefficients[ i ] ~ normal(0, .5); 
  }
  
  for( i in 1 : N ) {
    if( zeroVal[ i ] == 1 ) target += normal_lcdf( ASSQ_L | Mu[ i ], sigma );
  }
  for(i in 1 : N){
    if( zeroVal[ i ] == 0 ) ASSQ[ i ] ~ normal( Mu[ i ], sigma );
  }
    
}

generated quantities {
  vector[ Nmeasurements + 2 ] EuclideanDistances;

  for( i in 1:Nmeasurements){
    EuclideanDistances[ i ] = distance( Weights, ExpSensitiveWeights[ i ] );
  }
  EuclideanDistances[ Nmeasurements + 1 ] = distance( Weights, ExpAccumulationWeights );
  EuclideanDistances[ Nmeasurements + 2 ] = distance( Weights, ExpCriticalChildhoodWeights );
  

}
