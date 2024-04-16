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


means <- LatentGaussians_NoNapa$parameters$mean



# plotrix::plotCI(x = 1:4 ,F = means[,1], ui = upper1, li = lower1,)
segments(x0 = 1:3, x1 = 1:3, y1 = upper1, y0 = lower1, col = cols[1])
segments(x0 = 1:3, x1 = 1:3, y1 = upper2, y0 = lower2, col = cols[2])

dev.off()

var1 <- LatentGaussians_NoNapa$parameters$variance$sigma[,,1]
var2 <- LatentGaussians_NoNapa$parameters$variance$sigma[,,2]

sd1 <- sqrt(diag(var1)) / sqrt(113)
sd2 <- sqrt(diag(var2)) / sqrt(119)

upper1 <- means[,1] + 2*sd1
lower1 <- means[,1] - 2*sd1

upper2 <- means[,2] + 2*sd2
lower2 <- means[,2] - 2*sd2

plotData = data.frame(cbind(as.vector(LatentGaussians_NoNapa$parameters$mean), c(sd1,sd2)))
plotData$group = rep(c("Low", "High"), each = 3)

tiff("Figures/Trajectories.tiff", family = "serif", width = 12, height = 12, units = "in", res = 480, pointsize = 10, compression = "jpeg")

ggplot2::ggplot(data = plotData, mapping = aes(x = rep(1:3, times = 2), colour = factor(group, levels = c("Low", "High")))) + 
  geom_point(mapping = aes(y = X1), size = 3) + 
  geom_line(mapping = aes(y = X1)) + 
  geom_errorbar(aes(ymin = X1 - 2*X2, 
                    ymax = X1 + 2*X2), width=.05, lwd = 1) + 
  coord_cartesian(ylim = c(50,150)) + 
  scale_color_manual(values = c("#003366", "darkorange3"), guide = guide_legend(position = "bottom", title = "Trajectory", )) + 
  xlab("Measurement time") + ylab("Mean 25(OH)D") + 
  scale_x_continuous(breaks = 1:3, labels = c("Pregnancy", "12 Months", "24 Months")) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(text = element_text(size = 24, family = "serif"),
        axis.text.x = element_text(size = 20), 
        axis.title.x = element_text(vjust = -1),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.margin = margin(20,0.5,0.5,0.5) ) 
dev.off()













# Addition with limiting to those who had ASSQ available:
tiff("Figures/TrajectoriesASSQavailable.tiff", family = "serif", width = 4, height = 4, units = "in", res = 480, pointsize = 8)

matplot(t(aggregate(na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum), 
                      c("LDvitProfile_NoNapa_to_2year",
                lcavars_nonapa) ])[ , lcavars_nonapa], 
          by = list((na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum) , c("LDvitProfile_NoNapa_to_2year",
                                     lcavars_nonapa) ]))$LDvitProfile_NoNapa_to_2year),
          mean)[,2:4]), type = "l", 
        axes = F, ylim = c(50,150), 
        ylab = "Mean 25(OH)D", xlab = "Measurement time", 
        main = "", cex.lab = 1, cex.axis = 1, col = cols[1:2])
means <- t(aggregate(na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum), 
                                 c("LDvitProfile_NoNapa_to_2year",
                                   lcavars_nonapa) ])[ , lcavars_nonapa], 
                     by = list((na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum) , c("LDvitProfile_NoNapa_to_2year",
                                                                               lcavars_nonapa) ]))$LDvitProfile_NoNapa_to_2year),
                     mean)[,2:4])
sds <- t(aggregate(na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum), 
                               c("LDvitProfile_NoNapa_to_2year",
                                 lcavars_nonapa) ])[ , lcavars_nonapa], 
                   by = list((na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum) , c("LDvitProfile_NoNapa_to_2year",
                                                                             lcavars_nonapa) ]))$LDvitProfile_NoNapa_to_2year),
                   sd)[,2:4])
smplsizes <- t(aggregate(na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum), 
                                     c("LDvitProfile_NoNapa_to_2year",
                                       lcavars_nonapa) ])[ , lcavars_nonapa], 
                         by = list((na.omit(df[ !is.na(df$Zsqrt_ASSQ_6to8_sum) , c("LDvitProfile_NoNapa_to_2year",
                                                                                   lcavars_nonapa) ]))$LDvitProfile_NoNapa_to_2year),
                         length)[,2:4])

sdmean <- sds / sqrt(smplsizes)

upper1 <- means[,1] + 2*sdmean[,1]
lower1 <- means[,1] - 2*sdmean[,1]

upper2 <- means[,2] + 2*sdmean[,2]
lower2 <- means[,2] - 2*sdmean[,2]

segments(x0 = 1:3, x1 = 1:3, y1 = upper1, y0 = lower1, col = cols[1])
segments(x0 = 1:3, x1 = 1:3, y1 = upper2, y0 = lower2, col = cols[2])


axis( 1, at = 1:nrow(LatentGaussians_NoNapa$parameters$mean) , 
      labels = c("Pregnancy", "12 months", "24 months"), cex.axis = 1 )
axis( 2, at = seq( 50, 150, length.out = 5 ), cex.axis = 1 )

dev.off()


# LPA figure, napa -----

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




