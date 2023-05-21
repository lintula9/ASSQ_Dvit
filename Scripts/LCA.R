##### LCA:
source("Scripts/DataFetch_DataManagement.R")

# Fit LCAs with different k amount of classes. A priori is 3?
lcavars <- c("Rask_S25OHD", "cesd_mean")
frml <- as.formula(paste(paste("cbind(Rask_S25OHD,cesd_mean)"), " ~ 1"))
nclass <- 3
# Compare fits with k classes - choose by AIC, BIC, % assigned criteria.

lcafit <- flexmix(data = df, 
                  k = nclass, 
                  formula = frml)

# Visualize classes: curves for each classes.

# Relate classes by regresisons to distal variables, while accounting for uncertainty in class assignment.



