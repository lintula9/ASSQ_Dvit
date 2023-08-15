# Publication figures

# LPA FIGURE 1, nonapa.
tiff("Figures/Trajectories.tiff", family = "serif", width = 4, height = 4, units = "in", res = 640, pointsize = 8)
matplot(LatentGaussians_6to8_nonapa$parameters$mean, type = "l", 
        axes = F, ylim = c(50,150), 
        ylab = "Mean D-vitamin", xlab = "Measurement time", 
        main = "", cex.lab = 1, cex.axis = 1, col = cols[1:2])
axis( 1, at = 1:nrow(LatentGaussians_6to8_nonapa$parameters$mean) , 
      labels = c("Prenatal", "1 Year", "2 Year", "6-8 Year"), cex.axis = 1 )
axis( 2, at = seq( 50, 150, length.out = 5 ), cex.axis = 1 )
# legend(x=1, y = 60, lty = c(1,2), col = cols[1:2],
#        legend = paste(1:2,"n:",table(LatentGaussians_6to8_nonapa$classification)))
var1 <- LatentGaussians_6to8_nonapa$parameters$variance$sigma[,,1]
var2 <- LatentGaussians_6to8_nonapa$parameters$variance$sigma[,,2]

sd1 <- sqrt(diag(var1)) / sqrt(113)
sd2 <- sqrt(diag(var2)) / sqrt(119)

means <- LatentGaussians_6to8_nonapa$parameters$mean

upper1 <- means[,1] + 2*sd1
lower1 <- means[,1] - 2*sd1

upper2 <- means[,2] + 2*sd2
lower2 <- means[,2] - 2*sd2

# plotrix::plotCI(x = 1:4 ,F = means[,1], ui = upper1, li = lower1,)
segments(x0 = 1:4, x1 = 1:4, y1 = upper1, y0 = lower1, col = cols[1])
segments(x0 = 1:4, x1 = 1:4, y1 = upper2, y0 = lower2, col = cols[2])

dev.off()

# LPA figure, napa

tiff("Figures/Trajectories_napa.tiff", family = "serif", width = 4, height = 4, units = "in", res = 640, pointsize = 8)
matplot(LatentGaussians_All$parameters$mean, type = "l", 
        axes = F, ylim = c(50,150), 
        ylab = "Mean D-vitamin", xlab = "Measurement time", 
        main = "", cex.lab = 1, cex.axis = 1, col = cols[1:2])
axis( 1, at = 1:nrow(LatentGaussians_All$parameters$mean) , 
      labels = c("Prenatal","Cord", "1 Year", "2 Year", "6-8 Year"), cex.axis = 1 )
axis( 2, at = seq( 50, 150, length.out = 5 ), cex.axis = 1 )
# legend(x=1, y = 60, lty = c(1,2), col = cols[1:2],
#        legend = paste(1:2,"n:",table(LatentGaussians_6to8_nonapa$classification)))
vars <- LatentGaussians_All$parameters$variance$sigma
sds <- apply(vars, MARGIN = 3, FUN = function(x) sqrt(diag(x))) * c(1/sqrt(22), 1/sqrt(202))
means <- LatentGaussians_All$parameters$mean


upper1 <- means[,1] + 2*sds[,1]
lower1 <- means[,1] - 2*sds[,1]

upper2 <- means[,2] + 2*sds[,2]
lower2 <- means[,2] - 2*sds[,2]


# plotrix::plotCI(x = 1:4 ,F = means[,1], ui = upper1, li = lower1,)
segments(x0 = 1:5, x1 = 1:5, y1 = upper1, y0 = lower1, col = cols[1])
segments(x0 = seq(1.01,5.01, by = 1), x1 = seq(1.01,5.01, by = 1), y1 = upper2, y0 = lower2, col = cols[2])

dev.off()

tiff("Figures/Trajectories_napa_classificatio.tiff", family = "serif", width = 4, height = 4, units = "in", res = 640, pointsize = 4)

plot(LatentGaussians_All, "classification")

dev.off()
