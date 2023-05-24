##### LCA:

source("Scripts/DataFetch_DataManagement.R")

# Fit LCAs with different k amount of classes. 
lcavars <- c("Rask_S25OHD", 
             "Korj_Napa25OHD",
             "S25OHD_12kk",
             "S25OHD_24kk")

# Visualize realtionships between D-vitamin variables.

ggpairs(df[,c(lcavars,"sukupuoli")], 
        columns = lcavars, 
        mapping = aes(col = factor(sukupuoli)))

  ### ########################################  ###
  ### These models have no auxiliary variables. ###
  ### ########################################  ###

# LCA with all d-vitamin concentration measurements.

LatentGaussians <- Mclust(data = na.omit(df[,lcavars]), G = 1:6)
summary(LatentGaussians)
