##### LCA:

source("Scripts/DataFetch_DataManagement.R")

# Fit LCAs with different k amount of classes. A priori is 3?
lcavars <- c("Rask_S25OHD", "cesd_mean")

# Distributional assumptions 
hist(df[,lcavars[1]])
hist(df[,lcavars[2]])

frml <- as.formula(paste(paste("cbind(Rask_S25OHD,cesd_mean)"), " ~ 1"))

# Compare fits with k classes - choose by AIC, BIC, % assigned criteria.
latentFit <- Mclust( na.omit ( df[ , lcavars ] ), G = 1:6 )

# Visualize classes.
nclass <- latentFit$G
plot(latentFit$data[,1], 
     y = latentFit$data[,2], pch = 15,
     col = gray(seq(0,.7, length.out = nclass), alpha = 1)[latentFit$classification], 
     ylab = "CES-D", 
     xlab = "25(OH)D",
     )
sapply(1:nclass, FUN = function( i ){
  
  ellipse(mu = latentFit$parameters$mean[ , i ], 
          sigma = latentFit$parameters$variance$sigma[,, i ], 
          newplot = F, 
          draw = T, 
          alpha = .315, 
          col = gray(seq(0,.7, length.out = nclass)[ i ]),
          lwd = 2, 
          lty = "dashed")
})
points(t(latentFit$parameters$mean), 
       pch = 23, 
       cex = 3,
       bg = gray(seq(0,.7, length.out = nclass), alpha = 1))

# Relate classes by regresisons to distal variables, while accounting for uncertainty in class assignment.

