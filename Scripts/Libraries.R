# Packages.

package_names <- c("haven", "ggplot2", "mclust", 
                   "mixtools", "flexmix", "mice",
                   "GGally", "gridExtra", "scales",
                   "tidyverse", "MASS", "mvtnorm",
                   "rstan", "RColorBrewer", "bayesplot",
                   "emmeans", "plotrix", "ProDenICA")

for (i in package_names){
  if ( !requireNamespace( i, 
                          quietly = T )) {
    message( "\n\n\t\tNo package ", i, ". Installing package ", i, ".\n\n")
    install.packages( i )
  }
  library( i, character.only = TRUE )}

# Graphical settings
cols <- brewer.pal(n = 8, name = "Dark2")[1:3]
names(cols) <- c("Total", "Male", "Female")
theme_set(theme_bw(base_size = 16))
options(ggplot2.discrete.colour= cols)
par(family = "serif")

# Brlmfitplotfunction 
SensVsAccPlot <- function(model) {
  
  samples <- extract(model, pars = "EuclideanDistances")
  location <- ncol(samples$EuclideanDistances) - 2
  diff <- samples$EuclideanDistances[ , location ] - samples$EuclideanDistances[ , location + 1 ]
  dens <- density(diff)
  plot(dens,
       main = "Childhood sensitive - accumulative \n95% CI", xlab = "Difference in distance")
  segments(x0=quantile(diff, probs = .025), x1 = quantile(diff, probs = .025),
           y0=0, y1=dens$y[which.min(abs(dens$x - quantile(diff, probs = .025)))],lty = "dashed")
  segments(x0=quantile(diff, probs = .975), x1 = quantile(diff, probs = .975),
           y0=0, y1=dens$y[which.min(abs(dens$x - quantile(diff, probs = .975)))],lty = "dashed")
}
