if( !exists( "LatentGaussians" ) ) { source( "Scripts/LCA.R" ) }

# LPA all fits. ------

pdf( "Figures/LPAallFits.pdf" , # Create PDF.
     pointsize = 12, 
     width = 12, 
     height = 12,
     family = "serif" )

modelSaveNames <- sort(names(globalenv())[grep("LatentGaussians", names(globalenv()))])
for(i in modelSaveNames) {
  fit <- as.list(.GlobalEnv)[[ i ]]

  plot(0,0, type = "n", ylab="", xlab ="", axes = F)
  text(0,0, attr(fit, "Desc"))
  
  plot( fit , "classification")
  plot( fit , "BIC")
  matplot(fit$parameters$mean, type = "l", axes = F, ylim = c(50,150), ylab = "Mean D-vitamin", xlab = "Measurement", main = i)
  axis( 1, at = 1:nrow(fit$parameters$mean) , 
        labels = rownames(fit$parameters$mean) )
  axis( 2, at = seq( 50, 150, length.out = 5 ) )
  legend(x=1, y = 60, 
         text.col = 1:fit$G,
         legend = paste(1:fit$G,"n:",table(fit$classification)))
  
}
dev.off()
