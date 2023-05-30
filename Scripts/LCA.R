##### LCA:

source("Scripts/DataFetch_DataManagement.R")

# Select variables.
lcavars <- c("Rask_S25OHD", 
             "Korj_Napa25OHD",
             "S25OHD_12kk",
             "S25OHD_24kk")

lcavars_nonapa <- c("Rask_S25OHD",
                    "S25OHD_12kk",
                    "S25OHD_24kk")

lcavars_6to8Included <- c(lcavars, "D25OHD_nmol_l_6to8") # Variable was not in the dataset.



# Visualize relationships between D-vitamin variables.

ggpairs(na.omit(df[,c(lcavars,"sukupuoli")]), 
        columns = lcavars, 
        mapping = aes(col = factor(sukupuoli)))


          ####################################
          # Fit models ------------###########
          
# LCA with all d-vitamin concentration measurements.

LatentGaussians <- Mclust( data = na.omit( df[ , lcavars ] ), 
                          G = 1:6 )
# LCA without Napa25OHD

LatentGaussians_NoNapa <- Mclust( data = na.omit( df[ , lcavars_nonapa ] ), 
                                 G = 1:6 )

# LCA with 6-8 year old included

LatentGaussians_All <- Mclust( data = na.omit( df[ , lcavars_6to8Included ] ), 
                               G = 1:6 )
  # Runs into missing data problems: only 224 available.

# Gaussians, stratified by sex. Using all pre 6-8 year data.

LatentGaussians_sexstrat1 <- Mclust( data = na.omit( df[ df$sukupuoli == 1 , lcavars ] ), 
                           G = 1:6 )
LatentGaussians_sexstrat2 <- Mclust( data = na.omit( df[ df$sukupuoli == 2 , lcavars ] ), 
                           G = 1:6 )

