# Bayesian Relevant Life-Course Modelling 

library(rstan)

# Data handling -----
bayesVars <- c( "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdf <- na.exclude( df[ , bayesVars ] )

stan_data = list(
  
  # D-vitamin variables:
  Toddlerhood = scale(bayesdf$S25OHD_12kk),
  Infancy = scale(bayesdf$S25OHD_24kk),
  Childhood = scale(bayesdf$D25OHD_nmol_l_6to8),
  measurements = cbind(cbind(Toddlerhood, Infancy), Childhood),
  Nmeasurements = ncol(measurements),
  
  # Covariates:
  Age = scale(bayesdf$ikäASSQ),
  Sex = bayesdf$sukupuoli,
  Covariates = cbind(Age,Sex),
  NCovariates = ncol(Covariates),
  
  # Y outcome:
  ASSQ = scale(bayesdf$ASSQ_6to8_mean),
  
  # N:
  N = dim(bayesdf)[1],
  
  # Prior parameters:
  alpha = c(1/3, 1/3, 1/3), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpCriticalWeights = list(c(2/3, 1/6, 1/6), c(1/6, 2/3, 1/6), c(1/6, 1/6, 2/3)),
  ExpAccumulationWeights = c(0, 0, 1)
    )


# Set up the model:

stan_model("BRLM.stan", 
           model_name = "BRLM",
           auto_write = T)