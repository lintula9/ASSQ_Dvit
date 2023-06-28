# Bayesian Relevant Life-Course Modelling 

library(rstan)

# 3 Toddlerhood, infancy, childhood -----
bayesVars <- c( "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdf <- na.exclude( df[ , bayesVars ] )
Covariates <- bayesdf[ , c("ikäASSQ", "sukupuoli")]
names(Covariates) <- c("Age", "Sex")
Covariates$Age <- scale(Covariates$Age)
Covariates$Sex <- factor(Covariates$Sex, levels = c(1,2), labels = c("Male", "Female"))

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
  Covariates = subset( model.matrix( ~ Age + Sex, data = Covariates) , select = -c(`(Intercept)`) ),
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
  
  saveRDS(brlmFit, file = "brlmfit")
  brlmFit <- readRDS("brlmfit")
}



# 4 measurements --------------

bayesVars2 <- c( "Rask_S25OHD", "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdf2 <- na.exclude( df[ , bayesVars2 ] )
Covariates2 <- bayesdf2[ , c( "ikäASSQ", "sukupuoli" ) ]
names(Covariates2) <- c( "Age", "Sex" )

Covariates2$Age <- scale( Covariates2$Age )
Covariates2$Sex <- factor( Covariates2$Sex, levels = c(1,2), labels = c("Male", "Female") )


Measurements2 <- scale(bayesdf2[ , c( "Rask_S25OHD", "S25OHD_12kk" , 
                               "S25OHD_24kk" , "D25OHD_nmol_l_6to8" ) ])

stan_data2 = list(
  
  # D-vitamin variables:
  Measurements = Measurements2,
  Nmeasurements = ncol(Measurements2),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) ),
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
  
  saveRDS(brlmFit2, file = "brlmfit2")
  brlmFit <- readRDS("brlmfit2")
}




# Censored analysis ---- 

bayesVars3 <- c( "Rask_S25OHD", "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                 "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 


bayesdf3 <- na.exclude( df[ , bayesVars3 ] )

obsVals <- bayesdf3$ASSQ_6to8_mean != 0
censVals <- bayesdf3$ASSQ_6to8_mean == 0

Covariates3 <- bayesdf3[ , c( "ikäASSQ", "sukupuoli" ) ]
names(Covariates3) <- c( "Age", "Sex" )
Covariates3$Age <- scale( Covariates3$Age )
Covariates3$Sex <- factor( Covariates3$Sex, levels = c(1,2), labels = c("Male", "Female") )

Covariates3_obs <- Covariates3[ obsVals , ]
Covariates3_cens <- Covariates3[ censVals , ]

Measurements3 <- scale(bayesdf3[ , c( "Rask_S25OHD", "S25OHD_12kk" , 
                                      "S25OHD_24kk" , "D25OHD_nmol_l_6to8" ) ])

Measurements3_obs <- Measurements3[ obsVals , ]
Measurements3_cens <- Measurements3[ censVals, ]
ASSQ <- as.vector(scale( bayesdf2$ASSQ_6to8_mean ))

stan_data3 = list(
  
  # D-vitamin variables:
  Measurements = Measurements3,
  Nmeasurements = ncol( Measurements3 ),
  
  # Covariates:
  Covariates = Covariates3,
  NCovariates = ncol( Covariates3 ),
  
  # Y outcome:
  ASSQ = ASSQ,
  ASSQ_L = min(ASSQ),
  
  # N:
  N = dim(Covariates3)[ 1 ],
  N_obs = dim( Covariates3_obs )[ 1 ],
  N_cens = dim( Covariates3_cens )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpCriticalWeights = list( c( 2/4, 1/4, 1/4, 1/4 ), 
                             c( 1/4, 2/4, 1/4, 1/4 ), 
                             c( 1/4, 1/4, 2/4, 1/4 ),
                             c( 1/4, 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 0, 0, 0, 1 ),
  
  # Is the value zero?
  zeroVal = as.numeric(censVals)
  
)

# Set up the model:

if(FALSE){ # To prevent sourcing problems.
  
  brlmFit3 <- stan( file = "BLRM_censored2.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 2000, 
                    cores = 4 )
  
  saveRDS(brlmFit3, file = "brlmfit3")
  brlmFit <- readRDS("brlmfit3")
}

# For PDF summary: --------
if(FALSE) { # To prevent sourcing problems.
  
  pdf( "Figures/BRLMFigs.pdf" , # Create PDF.
       pointsize = 12, 
       width = 12, 
       height = 12,
       family = "serif" )
  
  generated_quantities <- extract(brlmFit)
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  plot(brlmFit, pars = c("Delta","Weights", "Coefficients"))
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
  
  generated_quantities2 <- extract(brlmFit2)
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit2, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  plot(brlmFit2, pars = c("Delta","Weights", "Coefficients"))
  par(mfrow = c( 2 , 2 ))
  hypotheses2 <- c("pregnancy sensitive", "Toddlerhood sensitive", 
                   "Infancy sensitive", "Childhood sensitive", "Childhood critical")
  for( i in 1:( ncol( generated_quantities2$EuclideanDistances ) ) ){
    dens <- density( generated_quantities2$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses2[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities2$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
  
  
  
  
  generated_quantities3 <- extract(brlmFit3)
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit3, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  plot(brlmFit3, pars = c("Delta","Weights", "Coefficients"))
  par(mfrow = c( 2 , 2 ))
  hypotheses2 <- c("pregnancy sensitive", "Toddlerhood sensitive", 
                   "Infancy sensitive", "Childhood sensitive", "Childhood critical")
  for( i in 1:( ncol( generated_quantities3$EuclideanDistances ) ) ){
    dens <- density( generated_quantities3$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses2[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities3$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
  
  hist(scale(df$ASSQ_6to8_mean), 
       probability = T, 
       xlim = c(-4,4), 
       breaks = 25, 
       main = "Sample distribution vs. Estimated distribution",
       xlab = "ASSQ standardized")
  roughMeanEst <- mean(summary(brlmFit3, "Mu")$summary[ , "mean"])
  roughSDEst <- summary(brlmFit3, "sigma")$summary[ , "mean"]
  lines( dnorm(seq(-5,5, by = 0.1), 
               mean = roughMeanEst, 
               sd = roughSDEst),
         x = seq(-5,5, by = 0.1))
  lines( dnorm(seq(-5,5, by = 0.1), 
               mean = mean(scale(bayesdf3$ASSQ_6to8_mean)), 
               sd = 1),
         x = seq(-5,5, by = 0.1), 
         lty = "dashed")
  legend(x = 2, y = .7,
         lty = c(1,2), 
         legend = c("Estimated", "Sample"), 
         border = "",  
         bty = "n", 
         cex = 1, 
         y.intersp = 1.5, 
         seg.len = 2, 
         text.width = .25, x.intersp = .1)
  
  
  
  dev.off()
}

