# Bayesian Relevant Life-Course Modelling 

# Load data. ----

if(!exists("package_names")) source("Scripts/Libraries.R")
if(!exists("dataPath2")) source("Scripts/DataFetch_DataManagement.R")



library(rstan)


# Upload models: -----
if(FALSE) {
  
if(!exists("brlmFit")) brlmFit <- readRDS("SaveFiles/brlmfit")
if(!exists("brlmFit2")) brlmFit2 <- readRDS("SaveFiles/brlmfit2")
if(!exists("brlmFit2Interaction")) brlmFit2Interaction <- readRDS("SaveFiles/brlmFit2Interaction")
if(!exists("brlmFit3")) brlmFit3 <- readRDS("SaveFiles/brlmfit3")
if(!exists("brlmFit3_Interaction")) brlmFit3_Interaction <- readRDS("SaveFiles/brlmFit3_Interaction")
if(!exists("brlmFitC")) brlmFitC <- readRDS("brlmFitC")

}

# 3 Toddlerhood, infancy, childhood -----

if(FALSE){ 
  brlmFit <- stan( file = "BRLM.stan", data = stan_data, 
      chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit, file = "SaveFiles/brlmfit")
  if(!exists("brlmFit")) brlmFit <- readRDS("brlmfit")
}
if(FALSE){ 
  brlmFit_males <- stan( file = "BRLM.stan", data = stan_data_males, 
                   chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit_males, file = "SaveFiles/brlmFit_males")
  if(!exists("brlmFit_males")) brlmFit_males <- readRDS("brlmFit_males")
}
if(FALSE){ 
  brlmFit_females <- stan( file = "BRLM.stan", data = stan_data_females, 
                   chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit_females, file = "SaveFiles/brlmFit_females")
  if(!exists("brlmFit_females")) brlmFit_females <- readRDS("brlmFit_females")
}

# 4 measurements --------------
if(FALSE){ 
  brlmFit2 <- stan( file = "BRLM.stan", 
                    data = stan_data2, 
                   chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit2, file = "SaveFiles/brlmfit2")
  if(!exists("brlmFit2")) brlmFit2 <- readRDS("brlmfit2")
}
if(FALSE){ 
  brlmFit2_males <- stan( file = "BRLM.stan", data = stan_data2_males, 
                         chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit2_males, file = "SaveFiles/brlmFit2_males")
  if(!exists("brlmFit2_males")) brlmFit_males <- readRDS("brlmFit2_males")
}
if(FALSE){ 
  brlmFit2_females <- stan( file = "BRLM.stan", data = stan_data2_females, 
                           chains = 4, iter = 2000, cores = 4 )
  
  saveRDS(brlmFit2_females, file = "SaveFiles/brlmFit2_females")
  if(!exists("brlmFit2_females")) brlmFit2_females <- readRDS("brlmFit2_females")
}

if(FALSE){ 
  
  brlmFit2Interaction <- stan( file = "BRLM_InteractionModel.stan", 
                               data = stan_data2, 
                    chains = 4, iter = 4000, cores = 8 )
  
  saveRDS(brlmFit2Interaction, file = "SaveFiles/brlmFit2Interaction")
  if(!exists("brlmFit2Interaction")) brlmFit2Interaction <- readRDS("brlmFit2Interaction")
  }
# Rask, 12mo, 24mo  -------

if(FALSE){ 
  
  brlmFit_123 <- stan( file = "BRLM.stan", 
                       data = stan_data123, 
                       chains = 4, 
                       iter = 4000,
                       cores = 8)

saveRDS(brlmFit_123, file = "SaveFiles/brlmFit_123")
if(!exists("brlmFit_123")) brlmFit_123 <- readRDS("brlmFit_123")
}
if(FALSE){ 
  
  brlmFit_123_males <- stan( file = "BRLM.stan", 
                       data = stan_data123_males, 
                       chains = 4, 
                       iter = 4000,
                       cores = 8)

saveRDS(brlmFit_123_males, file = "SaveFiles/brlmFit_123_males")
if(!exists("brlmFit_123_males")) brlmFit_123_males <- readRDS("brlmFit_123_males")
}
if(FALSE){ 
  
  brlmFit_123_females <- stan( file = "BRLM.stan", 
                       data = stan_data123_females, 
                       chains = 4, 
                       iter = 4000,
                       cores = 8)

saveRDS(brlmFit_123_females, file = "SaveFiles/brlmFit_123_females")
if(!exists("brlmFit_123_females")) brlmFit_123_females <- readRDS("brlmFit_123_females")
}


# Censored analysis ---- 
if(FALSE){ 
  
  brlmFit3 <- stan( file = "BLRM_censored2.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 2000, 
                    cores = 4 )
  
  saveRDS(brlmFit3, file = "SaveFiles/brlmfit3")
  if(!exists("brlmFit3")) brlmFit3 <- readRDS("brlmfit3")
}
if(FALSE){ 
  
  brlmFit3_Interaction <- stan( file = "BRLM_censored_Interaction.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 4000, 
                    cores = 4 )
  
  saveRDS(brlmFit3_Interaction, file = "SaveFiles/brlmFit3_Interaction")
  if(!exists("brlmFit3_Interaction")) brlmFit3_Interaction <- readRDS("brlmFit3_Interaction")
}

# Composite setting. --------------

if(FALSE){ 
  
  brlmFitC <- stan( file = "BRLM_composite.stan", 
                    data = stan_data3, 
                    chains = 4, 
                    iter = 4000,
                    cores = 8)
  
  saveRDS(brlmFitC, file = "SaveFiles/brlmFitC")
  if(!exists("brlmFitC")) brlmFitC <- readRDS("brlmFitC")
}

# Napameasures included. -------

if(FALSE){ 
    
    brlmFitNapa <- stan( file = "BRLM.stan", 
                      data = stan_dataNapa, 
                      chains = 4, 
                      iter = 4000,
                      cores = 8)

  saveRDS(brlmFitNapa, file = "SaveFiles/brlmFitNapa")
  if(!exists("brlmFitNapa")) brlmFitNapa <- readRDS("brlmFitNapa")
}
  
if(FALSE){ 
    
    brlmFitNapaInteraction <- stan( file = "BRLM_InteractionModel.stan", 
                         data = stan_dataNapa, 
                         chains = 4, 
                         iter = 4000,
                         cores = 8)

  saveRDS(brlmFitNapaInteraction, file = "SaveFiles/brlmFitNapaInteraction")
  if(!exists("brlmFitNapaInteraction")) brlmFitNapa <- readRDS("brlmFitNapaInteraction")
  }




  



# For PDF summary: --------
if(FALSE) { 

  pdf( "Figures/BRLMFigs.pdf" , # Create PDF.
       pointsize = 12, 
       width = 12, 
       height = 12,
       family = "serif" )
  
  # brlmFit:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 3 measures: 12 months (1), 24 months (2) and 6 to 8 years (3)", 
       cex = 1.5) # Title

  grid.arrange(tableGrob(round ( 
    summary ( brlmFit, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit)

  # brlmFit2:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures: Raskaus (1) 12 months (2), 24 months (3) and 6 to 8 years (4)", 
       cex = 1.5) # Title

  grid.arrange(tableGrob(round ( 
    summary ( brlmFit2, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit2, regex_pars = c("Delta","Weights", "Coefficients") , prob = .95)
  hypotheses <- c("Pregnancy sensitive","Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit2, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit2)
  
  # brlmFit2 with interaction:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures, and interaction delta x sex", 
       cex = 1.5) # Title
  

  grid.arrange(tableGrob(round ( 
    summary ( brlmFit2Interaction, 
              pars = c("Delta","Weights", "Coefficients", "SexInteraction"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit2Interaction, regex_pars = c("Delta","Weights") , prob = .95)
  mcmc_areas(brlmFit2Interaction, regex_pars = c("Coefficients", "SexInteraction") , prob = .95)
  hypotheses <- c("Pregnancy sensitive","Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit2Interaction, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit2Interaction)
  # brlmFit123, Raskaus, 12mo, 24mo:
  
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 3 measures, raskaus, 1-year, 2-year", 
       cex = 1.5) # Title
  
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit_123, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                                          "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit_123, regex_pars = c("Delta","Weights") , prob = .95)
  mcmc_areas(brlmFit_123, regex_pars = c("Coefficients", "SexInteraction") , prob = .95)
  hypotheses <- c("Pregnancy sensitive","Toddlerhood sensitive", "Infancy sensitive",
                  "Accumulative", "Childhood critical")
  mcmc_areas(brlmFit2Interaction, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit_123)
  
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"Sex stratified BRLMs.", 
       cex = 1.5) # Title
  # Sex stratified analyses...
  # brlmFit male:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 3 measures: 12 months (1), 24 months (2) and 6 to 8 years (3)\nmales only", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit_males, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit_males, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit_males, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit_males)
  
  # brlmFit females:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 3 measures: 12 months (1), 24 months (2) and 6 to 8 years (3)\nfemales only", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit_females, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit_females, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit_females, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit_females)
  
  # brlmFit2 male:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures: rask (1), 12 months (2), 24 months (3) and 6 to 8 year (4)\nmales only", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit2_males, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit2_males, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Pregnancy sensitive", "Toddlerhood sensitive", 
                  "Infancy sensitive", "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit2_males, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit2_males)
  
  # brlmFit2 females:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures: rask (1), 12 months (2), 24 months (3) and 6 to 8 year (4)\nfemales only", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit2_females, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit2_females, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Pregnancy sensitive", "Toddlerhood sensitive", 
                  "Infancy sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit2_females, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit2_females)
  
  # brlmFit123 male:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 3 measures: rask (1), 12 months (2) and 24 months (3)\nmales only", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit_123_males, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit_123_males, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Pregnancy sensitive", "Toddlerhood sensitive", 
                  "Infancy sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit_123_males, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit_123_males)
  
  # brlmFit123 females:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 3 measures: rask (1), 12 months (2) and 24 months (3)\nfemales only", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit_123_females, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas(brlmFit_123_females, regex_pars = c("Delta","Weights", "Coefficients"), prob = .95)
  hypotheses <- c("Pregnancy sensitive", "Toddlerhood sensitive", 
                  "Infancy sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit_123_females, 
             regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit_123_females)
  
  # Break
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"Some extras.", 
       cex = 1.5) # Title
  # brlmFit3 censored analysis:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures, censored analysis", 
       cex = 1.5) # Title
  {hist(scale(df$ASSQ_6to8_mean), 
       probability = T, 
       xlim = c(-4,4), 
       breaks = 25, 
       main = "Sample distribution vs. Estimated distribution",
       xlab = "ASSQ standardized")
  roughMeanEst <- mean(summary(brlmFit3, "Mu")$summary[ , "mean"])
  roughSDEst <- summary(brlmFit3, "sigma")$summary[ , "mean"]
  lines( dnorm(seq(-5,5, by = 0.1), 
               mean = roughMeanEst, 
               sd = roughSDEst),
         x = seq(-5,5, by = 0.1))
  lines( dnorm(seq(-5,5, by = 0.1), 
               mean = mean(scale(bayesdf2$ASSQ_6to8_mean)), 
               sd = 1),
         x = seq(-5,5, by = 0.1), 
         lty = "dashed")
  legend(x = 2, y = .7,
         lty = c(1,2), 
         legend = c("Estimated", "Sample"), 
         border = "",  
         bty = "n", 
         cex = 1, 
         y.intersp = 1.5, 
         seg.len = 2, 
         text.width = .25, x.intersp = .1)} # Sample dist vs. estimated dist
  
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFit3, 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                                          "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas( brlmFit3, regex_pars = c("Delta","Weights", "Coefficients") , prob = .95)
  hypotheses <- c("Pregnancy sensitive","Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit3, regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit3)
  
  
  # brlmFit3 censored analysis, with interaction:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures, censored analysis with interaction", 
       cex = 1.5) # Title

  grid.arrange(tableGrob(round ( 
    summary ( brlmFit3_Interaction, 
              pars = c("Delta","Weights", "Coefficients", "SexInteraction"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas( brlmFit3_Interaction, regex_pars = c("Delta","Weights") , prob = .95)
  mcmc_areas( brlmFit3_Interaction, regex_pars = c("Coefficients", "SexInteraction") , prob = .95)
  
  hypotheses <- c("Pregnancy sensitive","Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFit3_Interaction, regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFit3_Interaction)
  
  # Including napa measures:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 5 measures, including napa", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFitNapa , 
              pars = c("Delta","Weights", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas( brlmFitNapa , regex_pars = c("Delta","Weights") , prob = .95)
  mcmc_areas( brlmFitNapa , regex_pars = c("Coefficients", "SexInteraction") , prob = .95)
  
  hypotheses <- c("Pregnancy sensitive","Napa sensitive?","Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFitNapa , regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFitNapa)
  
  # Including napa measures and interaction:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 5 measures, interaction included", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFitNapaInteraction , 
              pars = c("Delta","Weights", "Coefficients", "SexInteraction"))$summary[ , c("mean","se_mean", 
                                                                        "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas( brlmFitNapaInteraction , regex_pars = c("Delta","Weights") , prob = .95)
  mcmc_areas( brlmFitNapaInteraction , regex_pars = c("Coefficients", "SexInteraction") , prob = .95)
  
  hypotheses <- c("Pregnancy sensitive","Napa sensitive?","Toddlerhood sensitive", "Infancy sensitive", 
                  "Childhood sensitive",
                  "Accumulative",  "Childhood critical")
  mcmc_areas(brlmFitNapaInteraction , regex_pars = "EuclideanDistances", prob = .95) + scale_y_discrete(labels = hypotheses)
  SensVsAccPlot(brlmFitNapaInteraction)
  
  # Composite model with 4 measurements:
  plot(0,0,type = "n", axes = F, xlab = "", ylab = "")
  text(0,0,"BRLM with 4 measures, composite model:\nComposite model uses a weighted sum\n of D-vit measurements and does not require\n same directions for all associations.", 
       cex = 1.5) # Title
  
  grid.arrange(tableGrob(round ( 
    summary ( brlmFitC , 
              pars = c("CompBeta","ScaledLoadings", "Coefficients"))$summary[ , c("mean","se_mean", 
                                                                                          "2.5%","50%","97.5%")], 3), 
    theme = ttheme_minimal()))
  mcmc_areas( brlmFitC , regex_pars = c("CompBeta", "ScaledLoadings") , prob = .95)
  mcmc_areas( brlmFitC , regex_pars = c("Coefficients") , prob = .95)

dev.off()
}

