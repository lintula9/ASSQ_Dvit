# Publication figures

# LPA FIGURE 1, nonapa.
tiff("Figures/Trajectories.tiff", family = "serif", width = 4, height = 4, units = "in", res = 480, pointsize = 8)
matplot(LatentGaussians_NoNapa$parameters$mean, type = "l", 
        axes = F, ylim = c(50,150), 
        ylab = "Mean 25(OH)D", xlab = "Measurement time", 
        main = "", cex.lab = 1, cex.axis = 1, col = cols[1:2])
axis( 1, at = 1:nrow(LatentGaussians_NoNapa$parameters$mean) , 
      labels = c("Pregnancy", "12 months", "24 months"), cex.axis = 1 )
axis( 2, at = seq( 50, 150, length.out = 5 ), cex.axis = 1 )
# legend(x=1, y = 60, lty = c(1,2), col = cols[1:2],
#        legend = paste(1:2,"n:",table(LatentGaussians_6to8_nonapa$classification)))
var1 <- LatentGaussians_NoNapa$parameters$variance$sigma[,,1]
var2 <- LatentGaussians_NoNapa$parameters$variance$sigma[,,2]

sd1 <- sqrt(diag(var1)) / sqrt(113)
sd2 <- sqrt(diag(var2)) / sqrt(119)

means <- LatentGaussians_NoNapa$parameters$mean

upper1 <- means[,1] + 2*sd1
lower1 <- means[,1] - 2*sd1

upper2 <- means[,2] + 2*sd2
lower2 <- means[,2] - 2*sd2

# plotrix::plotCI(x = 1:4 ,F = means[,1], ui = upper1, li = lower1,)
segments(x0 = 1:3, x1 = 1:3, y1 = upper1, y0 = lower1, col = cols[1])
segments(x0 = 1:3, x1 = 1:3, y1 = upper2, y0 = lower2, col = cols[2])

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

tiff("Figures/Trajectories_eAppendix2F2a.tiff", family = "serif", width = 6, height = 4, units = "in", res = 640, pointsize = 10)

colnames(LatentGaussians_NoNapa$data) <- c("Pregnancy", "1-year", "2-year")
plot(LatentGaussians_NoNapa, "classification", add = T)
par(mfrow = c(1,1))
dev.off()

tiff("Figures/Trajectories_eAppendix2F2b.tiff", family = "serif", width = 6, height = 4, units = "in", res = 640, pointsize = 10)

plotres <- Mclust(na.omit(df[,lcavars_nonapa]),G=2, modelNames = "VEI")
colnames(plotres$data) <- c("Pregnancy", "1-year", "2-year")

plot(plotres, "classification", add = T)

dev.off()

table(Mclust(na.omit(df[,lcavars_nonapa]), G = 2, model = "VEI")$classification, LatentGaussians_NoNapa$classification)



# BIC + AIC figure rask, 12mo, 24mo.

BICs <- mclustBIC( data = na.omit( df[ , lcavars_nonapa ] ))
BICs <- matrix(as.numeric(BICs), ncol = 14, nrow = 9)

LLs <- mclustLoglik( mclustBIC( data = na.omit( df[ , lcavars_nonapa ] ) ) )
modelnames <- colnames(LLs)
LLs <- matrix(LLs, byrow = T, ncol = 14, nrow = 9)
colnames(LLs) <- modelnames
colnames(BICs) <- modelnames
params <- matrix(ncol = 14, nrow = 9, NA)
colnames(params) <- colnames(LLs)

for(i in colnames(params)) params[,i] <- sapply(1:9, function(x) nMclustParams(modelName = i, G = x , d= 3))

AICs <- -(2*params - 2*LLs)

tiff("Figures/BIC_AIC.tiff", family = "serif", width = 6, height = 4, units = "in", res = 640, pointsize = 10)
par(mfrow = c(1,2))
matplot(-BICs, ylab = "BIC", type = "l", xlab = "k", ylim = c(17300, 17900), xaxp = c(1,9,8))
matplot(-AICs, ylab = "AIC", type = "l", xlab = "k", ylim = c(17300, 17900), xaxp = c(1,9,8))
par(mfrow = c(1,1))

# Test AIC suggested 3 groups ...?

dev.off()




