if( !exists( "LatentGaussians" ) ) { source( "Scripts/LCA.R" ) }

# Result PDF: -----------------------

pdf( "Figures/LPAFigs.pdf" , # Create PDF.
     pointsize = 12, 
     width = 12, 
     height = 12,
     family = "serif" )

informal_names <- c("Raskaus", "Napa", "12kk", "24kk", "6-8v")

# Introduction text for results using all measurement :

# Header
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
text("Plots of 3 latent profiles including: 
     \nRask_S25OHD 
     \nKorj_Napa25OHD
     \nS25OHD_12kk
     \nS25OHD_24kk",
     x = .5, y = .6, cex = 2)

## Plots/tables using all pre 6-8 year results: -----

plot(LatentGaussians, what = "BIC")

plot( LatentGaussians, # Classification plots
      what = "classification")


matplot( LatentGaussians$parameters$mean , 
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2:LatentGaussians$G))
axis( 1, at = 1:(LatentGaussians$G+1) , 
      labels = informal_names[ -5 ] )
axis( 2, at = seq( 50, 150, length.out = 5 ) )



legend( x = 3.5, y = 70, 
        col = c("blue", 2, 3), 
        lwd = 1, 
        title = "Class",
        legend = 1:3)

## No napa measurement results: -----
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
# Introduction text ....
text("Plots of 3 latent profiles including: 
     \nRask_S25OHD 
     \nS25OHD_12kk
     \nS25OHD_24kk
     \n(i.e., without napa)",
     x = .5, y = .6, cex = 2)

# Plots/tables using no-napa measurmenet

plot(LatentGaussians_NoNapa, what = "BIC")

plot( LatentGaussians_NoNapa, # Classification plots
      what = "classification")

matplot( LatentGaussians_NoNapa$parameters$mean , 
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2:LatentGaussians_NoNapa$G))
axis( 1, at = c( 1, 2, 3 ) , 
      labels = informal_names[ -c(which(informal_names %in% c("Napa", "6-8v"))) ] )
axis( 2, at = seq( 50, 150, 
                   length.out = 5 ) )
legend( x = 2.5, y = 70, 
        col = c("blue", 2:LatentGaussians_NoNapa$G), 
        lwd = 1, 
        title = "Class",
        legend = 1:LatentGaussians_NoNapa$G)


# Intervention groups -------
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
# Introduction text ....
text("Intervention group means.",
     x = .5, y = .6, cex = 2)

matplot( t(aggregate(df[ , lcavars_nonapa ], by = list(df$Ryhmä) , mean, na.rm = T ))[ -1 , ] , 
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2))
axis( 1, at = c( 1, 2, 3 ) , 
      labels = informal_names[ -c(which(informal_names %in% c("Napa", "6-8v"))) ] )
axis( 2, at = seq( 50, 150, 
                   length.out = 5 ) )
legend( x = 2.5, y = 70, 
        col = c("blue", 2), 
        lwd = 1, 
        title = "Intervention group",
        legend = 1:2)


## Sex stratified results: ------
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
# Subheader
text("Sex stratified results, using all pre 6 to 8 year data:
     \n 1. Distributions
     \n 2. Female class plots
     \n 3. Female averages
     \n 4. Male class plots
     \n 5. Male averages",
     x = .5, y = .6, cex = 2)

ggpairs(na.omit(df[,c(lcavars,"sukupuoli")]), # Distributional plots.
        columns = lcavars, 
        mapping = aes(col = factor(sukupuoli))) + 
  ggtitle("Sex stratified distributions")

# Females

plot(LatentGaussians_sexstrat1, "BIC")
plot(LatentGaussians_sexstrat1, "classification")
matplot( LatentGaussians_sexstrat1$parameters$mean , # sex stratified mean plots: female
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2, 3))
axis( 1, at = c( 1, 2, 3 , 4) , 
      labels = informal_names[-which(informal_names %in% "6-8v")] )
axis( 2, at = seq( 50, 150, length.out = 5 ) )
legend( x = 3.5, y = 70, 
        col = c("blue", 2, 3), 
        lwd = 1, 
        title = "Class",
        legend = 1:LatentGaussians_sexstrat1$G)

# Males

plot(LatentGaussians_sexstrat2, "BIC")
plot(LatentGaussians_sexstrat2, "classification")
matplot( LatentGaussians_sexstrat2$parameters$mean , # mean plots: male
         type = "l", 
         ylim = c( 50, 150 ),
         axes = F,
         xlab = "Measurement",
         ylab = "D-vitamin",
         lty = 1,
         col = c("blue", 2, 3))
axis( 1, at = c( 1, 2, 3 , 4) , 
      labels = informal_names[-which(informal_names %in% "6-8v")] )
axis( 2, at = seq( 50, 150, length.out = 5 ) )
legend( x = 3.5, y = 70, 
        col = c("blue", 2, 3), 
        lwd = 1, 
        title = "Class",
        legend = 1:LatentGaussians_sexstrat2$G) 

## Missingness patterns: ------
plot(type = "n", 
     x = 0:1, y = 0:1, bty='n',
     xaxt='n', yaxt='n', xlab='', ylab='')
# Subheader
text("Missigness patterns",
     x = .5, y = .6, cex = 2)

missingness <- md.pattern( df[ , lcavars_6to8Included ] , plot = F)
missingness <- missingness[ , lcavars_6to8Included ]
missingness[ missingness == 0 ] <- NA
missingness[ missingness == 1 ] <- ""
grid.arrange( tableGrob(missingness,
                        cols = c( informal_names ), 
                        theme = ttheme_minimal()
))


dev.off()
