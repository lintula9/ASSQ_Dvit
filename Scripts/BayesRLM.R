# Bayesian Relevant Life-Course Modelling 

# Load data.

if(!exists("package_names")) source("Scripts/Libraries.R")
if(!exists("dataPath2")) source("Scripts/DataFetch_DataManagement.R")



library(rstan)

# 3 Toddlerhood, infancy, childhood -----


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



# Composite setting. --------------

if(FALSE){ # To prevent sourcing problems.
  
  brlmFitC <- stan( file = "BRLM_composite.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 4000,
                    cores = 8)
                    )
  
  saveRDS(brlmFitC, file = "brlmFitC")
  if(!exists("brlmFitC")) brlmFitC <- readRDS("brlmFitC")
}

print(brlmFitC)
round(summary(brlmFitC, pars = c("CompBeta", "Loadings"))$summary, 3)



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

