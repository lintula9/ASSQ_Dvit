// Interaction model:
// Below is the interaction model that was run for ASSQ as outcome.
// Variables are written by their direct name as readably as I've could.


data {
  int<lower=0> N; // Number of observations.
  int<lower=0> NCovariates; // Number of covariates.
  int<lower=0> Nmeasurements; // Number of measurements (as in BRLM different time periods).
  matrix[N, Nmeasurements] Measurements; // Combined into a matrix.
  matrix[N, NCovariates] Covariates; // Sex needs to be the second column of this matrix!! Otherwise specify differently below.
  vector[N] ASSQ; // Outcome.
  vector[Nmeasurements] alpha; // Dirichlet priors for the BRLM model.
  vector[Nmeasurements] ExpSensitiveWeights[Nmeasurements]; // Hypothetical weights.
  vector[Nmeasurements] ExpAccumulationWeights;
  vector[Nmeasurements] ExpCriticalChildhoodWeights;
}

// As happens in normal linear regression, an interaction term is included:
// Outcome = Intercept + Delta * (Measurements * Weights) + Interaction-Coefficient * (Sex * (Measurements * Weights) ) +
//           Covariate-Coefficients * Covariates 




parameters {
  real Intercept;
  real Delta;
  real SexInteraction; // An additional parameter that is used for the interaction.
  simplex[Nmeasurements] Weights;
  vector[NCovariates] Coefficients;
  real<lower=0> sigma;
}

// .* Operator needed to be used, because otherwise matrix algebra would've been used.
// Also good to know, that Sex is an ordinary covariate in Covariates matrix, and so included in the model as a normal explanatory variable. 
transformed parameters {
  vector[N] LinearFunction;
  LinearFunction = Intercept + // Regression model is specified as 'LinearFunction'.
  Delta * (Measurements * Weights) +
  SexInteraction * (Covariates[ , 2] .* (Measurements * Weights)) + // .* is a techincal detail.
  (Covariates * Coefficients);
}

// Here are the priors.
// Note that I used more informative priors than possibly typically!!
// This is due to recommendations (McElreath - statistical rethinking, book).
// Normal(0, 1) is possibly more conventional.

model {
  Intercept ~ normal(0, .5); // .5 sd for normal priors!
  Weights ~ dirichlet(alpha);
  Delta ~ normal(0, .5);
  SexInteraction ~ normal(0, .5); 
  sigma ~ exponential(1);
  for(i in 1:NCovariates){
    Coefficients[i] ~ normal(0, .5); 
  }
  
  ASSQ ~ normal(LinearFunction, sigma);

}

// I have removed some generated quantities, because I didn't always need them.
// Check if something is amiss!
generated quantities {
  vector[ Nmeasurements + 2 ] EuclideanDistances;
  vector[ N ] logLikelihood;
  vector[ N ] yrep;
  vector[ N ] residuals;
  
  for( i in 1:Nmeasurements){
    EuclideanDistances[ i ] = distance( Weights, ExpSensitiveWeights[ i ] );
  }
  EuclideanDistances[ Nmeasurements + 1 ] = distance( Weights, ExpAccumulationWeights );
  EuclideanDistances[ Nmeasurements + 2 ] = distance( Weights, ExpCriticalChildhoodWeights );
  
  for( n in 1:N ) {
    logLikelihood[ n ] = normal_lpdf( ASSQ[ n ] | LinearFunction[ n ], sigma );
    yrep[ n ] = normal_rng( LinearFunction[ n ], sigma );
    residuals[ n ] = yrep[ n ] - ASSQ[ n ];
  }
}

// Hopefully this was readable. - Sakari