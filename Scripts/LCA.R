##### LCA:

source("Scripts/DataFetch_DataManagement.R")

# Fit LCAs with different k amount of classes. 
lcavars <- c("Rask_S25OHD", 
             "Korj_Napa25OHD",
             "S25OHD_12kk",
             "S25OHD_24kk")

lcavars_nonapa <- c("Rask_S25OHD",
                    "S25OHD_12kk",
                    "S25OHD_24kk")
lcavars_6to8Included <- c("")

# Visualize realtionships between D-vitamin variables.

ggpairs(df[,c(lcavars,"sukupuoli")], 
        columns = lcavars, 
        mapping = aes(col = factor(sukupuoli)))

  ### ########################################  ###
  ### These models have no auxiliary variables. ###
  ### ########################################  ###

# LCA with all d-vitamin concentration measurements.

LatentGaussians <- Mclust( data = na.omit( df[ , lcavars ] ), 
                          G = 1:6 )
summary(LatentGaussians)

LatentGaussians_NoNapa <- Mclust( data = na.omit( df[ , lcavars_nonapa ] ), 
                                 G = 1:6 )
summary(LatentGaussians_NoNapa)

# Result plots.

pdf( "Figures/LPAFigs.pdf" , 
    pointsize = 12, 
    width = 12, 
    height = 12,
    family = "serif" )
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
text("1. Plots of 3 latent profiles including: 
     \nRask_S25OHD 
     \nKorj_Napa25OHD
     \nS25OHD_12kk
     \nS25OHD_24kk",
     x = .5, y = .6, cex = 2)
plot( LatentGaussians, 
      what = "classification")
par(mfrow = c(2, 2))
sapply(1:length(lcavars), FUN = function(x) {
  
  plot( LatentGaussians, 
        what = "classification", x)
})
grid.arrange(tableGrob(round(
  LatentGaussians$parameters$mean, 1), 
  cols = as.character(1:3)
))

plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
text("2. Plots 3 of latent profiles including: 
     \nRask_S25OHD 
     \nS25OHD_12kk
     \nS25OHD_24kk",
     x = .5, y = .6, cex = 2)
plot( LatentGaussians_NoNapa, 
      what = "classification")
par(mfrow = c(2, 2))
sapply(1:length(lcavars_nonapa), FUN = function(x) {
  
  plot( LatentGaussians_NoNapa, 
        what = "classification", x)
})
grid.arrange(tableGrob(round(
  LatentGaussians_NoNapa$parameters$mean, 1), 
  cols = as.character(1:3)
))


dev.off()


