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

LatentGaussians_6to8Included <- Mclust( data = na.omit( df[ , lcavars_6to8Included ] ), 
                                        G = 1:6 ) # 


      ##############################################
      ################## Result plots. -------- ####

pdf( "Figures/LPAFigs.pdf" , # Create PDF.
    pointsize = 12, 
    width = 12, 
    height = 12,
    family = "serif" )

  # Introduction text for results using all measurement :
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
text("1. Plots of 3 latent profiles including: 
     \nRask_S25OHD 
     \nKorj_Napa25OHD
     \nS25OHD_12kk
     \nS25OHD_24kk",
     x = .5, y = .6, cex = 2)

  # Plots/tables using all measurements results:
plot( LatentGaussians, # Classification plots
      what = "classification")
par(mfrow = c(2, 2))
sapply(1:length(lcavars), FUN = function(x) {
  
  plot( LatentGaussians, 
        what = "classification", x)
})
par(mfrow = c(1,1))

grid.arrange(tableGrob(round(
  LatentGaussians$parameters$mean, 1), 
  cols = as.character(1:3)
))
matplot( LatentGaussians$parameters$mean , 
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2, 3))
axis( 1, at = c( 1, 2, 3 , 4) , 
      labels = lcavars )
axis( 2, at = seq( 50, 150, length.out = 5 ) )
legend( x = 3.5, y = 70, 
        col = c("blue", 2, 3), 
        lwd = 1, 
        title = "Class",
        legend = 1:3)

  # No napa measurement results:
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
  # Introduction text ....
text("2. Plots of 3 latent profiles including: 
     \nRask_S25OHD 
     \nS25OHD_12kk
     \nS25OHD_24kk",
     x = .5, y = .6, cex = 2)

  # Plots/tables using no-napa measurmenet
plot( LatentGaussians_NoNapa, # Classification plots
      what = "classification")
par(mfrow = c(2, 2))
sapply(1:length(lcavars_nonapa), FUN = function(x) {
  
  plot( LatentGaussians_NoNapa, 
        what = "classification", x)
})
par(mfrow = c(1,1))
grid.arrange(tableGrob(round(
  LatentGaussians_NoNapa$parameters$mean, 1), 
  cols = as.character(1:3)
))

matplot( LatentGaussians_NoNapa$parameters$mean , 
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2, 3))
axis( 1, at = c( 1, 2, 3 ) , 
      labels = lcavars_nonapa )
axis( 2, at = seq( 50, 150, length.out = 5 ) )
legend( x = 2.5, y = 70, 
        col = c("blue", 2, 3), 
        lwd = 1, 
        title = "Class",
        legend = 1:3)

  # Additional sex stratified plot
ggpairs(na.omit(df[,c(lcavars,"sukupuoli")]), 
        columns = lcavars, 
        mapping = aes(col = factor(sukupuoli))) + 
  ggtitle("Sex stratified distributions")

dev.off()


