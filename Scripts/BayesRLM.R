# Bayesian Relevant Life-Course Modelling 

# Load data.

if(!exists("package_names")) source("Scripts/Libraries.R")
if(!exists("dataPath2")) source("Scripts/DataFetch_DataManagement.R")



library(rstan)

# 3 Toddlerhood, infancy, childhood -----
bayesVars <- c( "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 

bayesdf <- na.exclude( df[ , bayesVars ] )
Covariates <- bayesdf[ , c("ikäASSQ", "sukupuoli")]
names(Covariates) <- c("Age", "Sex")
Covariates$Age <- scale(Covariates$Age)
Covariates$Sex <- factor(Covariates$Sex, levels = c(1,2), labels = c("Male", "Female"))

 # Create data list for stan.
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
  ExpCriticalWeights = list( c( 2/3, 1/6, 1/6 ), 
                             c( 1/6, 2/3, 1/6 ), 
                             c( 1/6, 1/6, 2/3 ) ),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpChildhoodCriticalWeights = c( 0, 0, 1 )
  
    )


# Set up the model:

if(FALSE){ # To prevent sourcing problems.

  brlmFit <- stan( file = "BRLM.stan", data = stan_data, 
      chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit, file = "brlmfit")
  if(!exists("brlmFit")) brlmFit <- readRDS("brlmfit")
  
  # Check the chains:
  mcmc_trace(brlmFit, pars = c("Delta", "Coefficients[1]"))
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
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4, 1/4 ), 
                             c( 1/4, 2/4, 1/4, 1/4 ), 
                             c( 1/4, 1/4, 2/4, 1/4 ),
                             c( 1/4, 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/4, 1/4, 1/4, 1/4 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0, 1 )
  
)

# Set up the model:

if(FALSE){ # To prevent sourcing problems.
  
  brlmFit2 <- stan( file = "BRLM.stan", 
                    data = stan_data2, 
                   chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit2, file = "brlmfit2")
  if(!exists("brlmFit2")) brlmFit2 <- readRDS("brlmfit2")
}





# 4 measurements Interaction --------------

# Set up the model:

if(FALSE){ # To prevent sourcing problems.
  
  brlmFit2Interaction <- stan( file = "BRLM_InteractionModel.stan", 
                               data = stan_data2, 
                    chains = 4, iter = 4000, cores = 8 )
  
  saveRDS(brlmFit2Interaction, file = "brlmFit2Interaction")
  if(!exists("brlmFit2Interaction")) brlmFit2Interaction <- readRDS("brlmFit2Interaction")
  
  
}








# Censored analysis ---- 


stan_data3 = append(stan_data2,list(
  
  # Where ASSQ is censored:
  ASSQ_L = min(scale( bayesdf2$ASSQ_6to8_mean )),

  # Is the value zero?
  zeroVal = as.numeric(bayesdf2$ASSQ_6to8_mean == 0)
  )
  )

# Set up the model:

if(FALSE){ # To prevent sourcing problems.
  
  brlmFit3 <- stan( file = "BLRM_censored2.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 2000, 
                    cores = 4 )
  
  saveRDS(brlmFit3, file = "brlmfit3")
  if(!exists("brlmFit3")) brlmFit3 <- readRDS("brlmfit3")
}

# Censored analysis with Interaction ------

if(FALSE){ # To prevent sourcing problems.
  
  brlmFit3_Interaction <- stan( file = "BRLM_censored_Interaction.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 4000, 
                    cores = 4 )
  
  saveRDS(brlmFit3_Interaction, file = "brlmFit3_Interaction")
  if(!exists("brlmFit3_Interaction")) brlmFit3_Interaction <- readRDS("brlmFit3_Interaction")
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
  
  generated_quantities2Int <- extract(brlmFit2Interaction)
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit2Interaction, 
              pars = c("Delta","Weights", "Coefficients", "SexInteraction"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit2Interaction, pars = c("Delta","SexInteraction", "Coefficients[1]", "Coefficients[2]"))
  mcmc_areas(brlmFit2Interaction, pars = "EuclideanDistances")
  
  par(mfrow = c( 2 , 2 ))
  hypotheses2 <- c("pregnancy sensitive", "Toddlerhood sensitive", 
                   "Infancy sensitive", "Childhood sensitive", 
                   "Accumulation", "Childhood critical")
  for( i in 1:( ncol( generated_quantities2Int$EuclideanDistances ) ) ){
    dens <- density( generated_quantities2Int$EuclideanDistances[ , i ] )
    plot( dens, main = hypotheses2[ i ], xlab = "Distance")
    meanQuant <- mean(generated_quantities2Int$EuclideanDistances[ , i ])
    segments(x0 = meanQuant, x1 = meanQuant,
             y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
    
  }
  par(mfrow = c(1,1))
  
  dens <- density( generated_quantities2Int$EuclideanDistances[,4] - generated_quantities2Int$EuclideanDistances[,5] ) 
  plot(dens,
       main = "Distribution of the difference between childhood sensitive and accumulation distances.",
       xlab = "Difference in euclidean distance")
  meanQuant <- mean(generated_quantities2Int$EuclideanDistances[,4] - generated_quantities2Int$EuclideanDistances[,5])
  modeQuant <- which.max(dens$y)
  medianQuant <- median(generated_quantities2Int$EuclideanDistances[,4] - generated_quantities2Int$EuclideanDistances[,5])
  segments(x0 = meanQuant, x1 = meanQuant,
           y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(meanQuant, 4) ) )])
  segments(x0 = dens$x[modeQuant], x1 = dens$x[modeQuant],
           y0 = 0, y1 = dens$y[modeQuant], lty = "dashed")
  segments(x0 = medianQuant, x1 = medianQuant,
           y0 = 0, y1 = dens$y[which.min( abs( round(dens$x, 4) - round(medianQuant, 4) ) )], lty = 3)
  rm(dens, meanQuant, modeQuant, medianQuant)
  
  
  
  
  
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

