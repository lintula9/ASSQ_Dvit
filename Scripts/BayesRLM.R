# Bayesian Relevant Life-Course Modelling 

library(rstan)

# 3 Toddlerhood, infancy, childhood -----
bayesVars <- c( "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdf <- na.exclude( df[ , bayesVars ] )
Covariates <- bayesdf[ , c("ikäASSQ", "sukupuoli")]
names(Covariates) <- c("Age", "Sex")
Covariates <- as.matrix(Covariates)

stan_data = list(
  
  # D-vitamin variables:
  Measurements = matrix(cbind(cbind(
    scale( bayesdf$S25OHD_12kk ), 
    scale( bayesdf$S25OHD_24kk )),
    scale( bayesdf$D25OHD_nmol_l_6to8 )),
                        ncol = 3, 
                        dimnames = list(NULL, c("Toddlerhood", "Infancy", "Childhood"))),
  Nmeasurements = 3,
  
  # Covariates:
  Covariates = Covariates,
  NCovariates = ncol(Covariates),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpCriticalWeights = list( c( 2/3, 1/6, 1/6 ), c( 1/6, 2/3, 1/6 ), c( 1/6, 1/6, 2/3 ) ),
  ExpAccumulationWeights = c( 0, 0, 1 )
  
    )


# Set up the model:

if(FALSE){ # To prevent sourcing problems.
brlmFit <- stan( file = "BRLM.stan", data = stan_data, 
      chains = 4, iter = 2000, cores = 4 )
}

# Summarize results:
if(FALSE) { # To prevent sourcing problems.
  
  
  
  print(brlmFit, pars = c("Delta","Weights", "Coefficients"))
  
  plot(brlmFit, pars = c("Delta","Weights", "Coefficients"))
  
  # Access the generated quantities from the stanfit object
  generated_quantities <- extract(brlmFit)

  par(mfrow = c( 2 , 2 ))
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", "Childhood sensitive", "Childhood critical")
  for( i in 1:( ncol( generated_quantities$EuclideanDistances ) ) ){
    dens <- density( generated_quantities$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
}

# For PDF summary: 
if(FALSE) { # To prevent sourcing problems.
pdf( "Figures/BRLMFigs.pdf" , # Create PDF.
     pointsize = 12, 
     width = 12, 
     height = 12,
     family = "serif" )
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  plot(brlmFit, pars = c("Delta","Weights", "Coefficients"))
  par(mfrow = c( 2 , 2 ))
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", "Childhood sensitive", "Childhood critical")
  for( i in 1:( ncol( generated_quantities$EuclideanDistances ) ) ){
    dens <- density( generated_quantities$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
  dev.off()
}


# 4 measurements --------------

bayesVars2 <- c( "Rask_S25OHD", "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdf2 <- na.exclude( df[ , bayesVars2 ] )
Covariates2 <- bayesdf2[ , c("ikäASSQ", "sukupuoli")]
names(Covariates2) <- c("Age", "Sex")
Covariates2 <- as.matrix(Covariates2)
Measurements2 <- bayesdf2[ , c("Rask_S25OHD", "S25OHD_12kk" , 
                               "S25OHD_24kk" , "D25OHD_nmol_l_6to8")]

stan_data2 = list(
  
  # D-vitamin variables:
  Measurements = Measurements2,
  Nmeasurements = ncol(Measurements2),
  
  # Covariates:
  Covariates = Covariates2,
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf2 )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpCriticalWeights = list( c( 2/4, 1/4, 1/4, 1/4 ), 
                             c( 1/4, 2/4, 1/4, 1/4 ), 
                             c( 1/4, 1/4, 2/4, 1/4 ),
                             c( 1/4, 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 0, 0, 0, 1 )
  
)


# Set up the model:

if(FALSE){ # To prevent sourcing problems.
  brlmFit2 <- stan( file = "BRLM.stan", data = stan_data2, 
                   chains = 4, iter = 2000, cores = 4 )
}

# Summarize results:
if(FALSE) { # To prevent sourcing problems.
  
  
  
  print(brlmFit2, pars = c("Delta","Weights", "Coefficients"))
  
  plot(brlmFit2, pars = c("Delta","Weights", "Coefficients"))
  
  # Access the generated quantities from the stanfit object
  generated_quantities <- extract(brlmFit2)
  
  par(mfrow = c( 2 , 2 ))
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive", "Childhood critical")
  for( i in 1:( ncol( generated_quantities$EuclideanDistances ) ) ){
    dens <- density( generated_quantities$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
}

# For PDF summary: 
if(FALSE) { # To prevent sourcing problems.
  pdf( "Figures/BRLMFigs_4measures.pdf" , # Create PDF.
       pointsize = 12, 
       width = 12, 
       height = 12,
       family = "serif" )
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  plot(brlmFit, pars = c("Delta","Weights", "Coefficients"))
  par(mfrow = c( 2 , 2 ))
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", "Childhood sensitive", "Childhood critical")
  for( i in 1:( ncol( generated_quantities$EuclideanDistances ) ) ){
    dens <- density( generated_quantities$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
  dev.off()
}