##### LCA:
if(!requireNamespace( "haven", quietly = F )) source("Scripts/Libraries.R")
if(!exists("df")) source("Scripts/DataFetch_DataManagement.R")

# Select variables.
lcavars <- c("Rask_S25OHD", 
             "Korj_Napa25OHD",
             "S25OHD_12kk",
             "S25OHD_24kk")

lcavars_nonapa <- c( "Rask_S25OHD",
                    "S25OHD_12kk",
                    "S25OHD_24kk" )

lcavars_6to8Included <- c(lcavars, "D25OHD_nmol_l_6to8") # Variable was not in the dataset.



# Visualize relationships between D-vitamin variables.

ggpairs(na.omit( df[ , c(lcavars,"sukupuoli") ] ), 
        columns = lcavars, 
        mapping = aes( col = factor( sukupuoli ) ) )


          ####################################
          # Fit models ------------###########
          
# LCA with all d-vitamin concentration pre 6 to 8 year measurements.

LatentGaussians <- Mclust( data = na.omit( df[ , lcavars ] ), 
                          G = 1:6 )
attr(LatentGaussians, "Desc") <- "Rask, napap, 12mo, 24mo."

LatentGaussians_2group <- Mclust( data = na.omit( df[ , lcavars ] ), 
                           G = 2 )
attr(LatentGaussians_2group, "Desc") <- "Rask, napa, 12mo, 24mo - forced to 2 groups."

# LCA without Napa25OHD; pre 6 to 8 year ------------

LatentGaussians_NoNapa <- Mclust( data = na.omit( df[ , lcavars_nonapa ] ), 
                                 G = 1:6 )
attr(LatentGaussians_NoNapa, "Desc") <- "Rask, 12mo, 24mo."
# LCA with 6-8 year measurements included --------------

LatentGaussians_All <- Mclust( data = na.omit( df[  , lcavars_6to8Included ] ), 
                               G = 1:6 )   # Runs into missing data problems: only 224 available.
LatentGaussians_6to8_nonapa <- Mclust( data = na.omit( df[  , c(lcavars_6to8Included[ -2 ]) ] ), 
                                       G = 1:6 )
attr(LatentGaussians_6to8_nonapa, "Desc") <- "Rask, 12mo, 24mo, 6to8years"
df$LDvitProfile_NoNapa_to_8year <- NA
df[ which( df$id %in% na.exclude(df[ , c(lcavars_6to8Included[ -2 ], "id" ) ])$id ) , ]$LDvitProfile_NoNapa_to_8year <- LatentGaussians_6to8_nonapa$classification
attr(df$LDvitProfile_NoNapa_to_8year , "label") <- "Latent profile indicator; when using no napa measurement"
attr(df$LDvitProfile_NoNapa_to_8year , "format.spss") <- "F12.1"


# LCA, stratified by sex. Using all pre 6-8 year data. ---------

LatentGaussians_sexstratMal <- Mclust( data = na.omit( df[ df$sukupuoli == 1 , lcavars ] ), 
                           G = 1:6 )
attr(LatentGaussians_sexstratMal, "Desc") <- "Males only\nRask, napa, 12mo, 24mo."
LatentGaussians_sexstratFem <- Mclust( data = na.omit( df[ df$sukupuoli == 2 , lcavars ] ), 
                           G = 1:6 )
attr(LatentGaussians_sexstratMal, "Desc") <- "Males only\nRask, napa, 12mo, 24mo."
attr(LatentGaussians_sexstratFem, "Desc") <- "Females only\nRask, napa, 12mo, 24mo."


# LCA, including cases with ASSQ scores available, using 4 measuremnets (no 6 to 8 year measure) -----

LatentGaussians_OnlyWith_ASSQavailable <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars ] ), 
                               G = 1:6 )
attr(LatentGaussians_OnlyWith_ASSQavailable, "Desc") <- "Only if ASSQ not NA\nRask, napa, 12mo, 24mo."


# LCA, including cases with ASSQ scores available, using 3 measuremnets (no 6 to 8 year measure) -----

LatentGaussians_OnlyWith_ASSQavailable_3measures <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars_nonapa ] ), 
                                                  G = 1:6 )
attr(LatentGaussians_OnlyWith_ASSQavailable_3measures, "Desc") <- "Only if ASSQ not NA\nRask, 12mo, 24mo."


# LCA, including cases with ASSQ scores available, forcing 2 groups.
LatentGaussians_OnlyWith_ASSQavailable_2group <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars ] ), 
                                                  G = 2 )
LatentGaussians_OnlyWith_ASSQavailable_2group_nonapa <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars_nonapa ] ), 
                                                         G = 2 )
attr(LatentGaussians_OnlyWith_ASSQavailable_2group, "Desc") <- "Only if ASSQ not NA\nRask, napa, 12mo, 24mo.\nForced to 2 group."
attr(LatentGaussians_OnlyWith_ASSQavailable_2group_nonapa, "Desc") <- "Only if ASSQ not NA\nRask, 12mo, 24mo.\nForced to 2 group."

# Save classification results into df as variables.

df$LDvitProfile_NoNapa_to_2year <- NA # No napa, 3 measurements.
df[ which( df$id %in% na.exclude(df[ , c(lcavars_nonapa, "id" ) ])$id ) , ]$LDvitProfile_NoNapa_to_2year <- LatentGaussians_NoNapa$classification
attr(df$LDvitProfile_NoNapa_to_2year , "label") <- "Latent profile indicator; when using no napa measurement"
attr(df$LDvitProfile_NoNapa_to_2year , "format.spss") <- "F12.1"

df$LDvitProfile_NoNapa_to_2year_ASSQcases <- NA # No napa, 3 measurements.
df[ which( df$id %in% 
             na.exclude(df[ !is.na(df$ASSQ_6to8_mean) , c(lcavars_nonapa, "id") ])$id ) , ]$LDvitProfile_NoNapa_to_2year_ASSQcases <- LatentGaussians_OnlyWith_ASSQavailable_2group_nonapa$classification
attr(df$LDvitProfile_NoNapa_to_2year_ASSQcases, "label") <- "Latent profile indicator; when using no napa measurement; only cases with ASSQ measurement"
attr(df$LDvitProfile_NoNapa_to_2year_ASSQcases , "format.spss") <- "F12.1"

  # ASSQ scores available vs. all data: Confusion matrix: ----

if ( FALSE ) {
# Print confusion matrix

table( df$LDvitProfile_NoNapa_to_2year, df$LDvitProfile_NoNapa_to_2year_ASSQcases ) # confusion matrix

# Log likelihood comparisons using the two estimated normals :

# Only ASSQ cases normals:
LatentGaussians_OnlyWith_ASSQavailable_2group_nonapa$loglik

# Using all cases (no napa) estimated normals:

p <- LatentGaussians_NoNapa$parameters$pro
m <- c()
for ( i in 1:LatentGaussians_NoNapa$G ) m[[ i ]] <- LatentGaussians_NoNapa$parameters$mean[ , i ]
S <- c()
for ( i in 1:LatentGaussians_NoNapa$G ) S[[ i ]] <- LatentGaussians_NoNapa$parameters$variance$sigma[ , , i ]

likelihoods <- c()
for ( i in 1:nrow(na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars_nonapa ] ))) {
  x <- na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars_nonapa ] )[ i , ]
  for( k  in 1:length( m ) ) {
    likelihoods[ i ] <- sum(p[[ k ]] * mvtnorm::dmvnorm( x , mean = m[[ k ]], sigma = S[[ k ]])) 
  } } 
  
LL <- sum(log(likelihoods))

# Compare using relative likelihood of models:

AIC1 <- 2 * 14 - 2 * LL
AIC2 <- 2 * 14 - 2 * LatentGaussians_OnlyWith_ASSQavailable_2group_nonapa$loglik

RelativeLL <- exp( (AIC2 - AIC1) / 2 )

# Compare trajectories: 
par(mfrow = c(1,2))
matplot(LatentGaussians_OnlyWith_ASSQavailable_2group_nonapa$parameters$mean, type = "b")
matplot(LatentGaussians_NoNapa$parameters$mean, type = "b")
par(mfrow = c(1,1))

}

  # Simultaneous modelling of ASSQ and S25OHD -----

LatentGaussians_Simultaneous_ASSQ <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_sum) , c(lcavars_nonapa, "ASSQ_6to8_sum") ] ), 
                                             G = 2 )
attr(LatentGaussians_Simultaneous_ASSQ, "Desc") <- "Simultaneous estimation of latent groups\nwith D-vitmain levels and ASSQ\n"

with(na.omit( df[ !is.na(df$ASSQ_6to8_sum) , c(lcavars_nonapa, "ASSQ_6to8_sum") ] ) , expr = {
  
  par(mfrow = c(2,2))
  
  matplot(LatentGaussians_Simultaneous_ASSQ$parameters$mean[1:3,], type = "b")
  hist(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ$classification == 1], col = cols[1], 
       main = "", breaks = seq(from = 0, to = 30, by = 2), freq = F, xlab = "", ylab = "", xlim = c(0,30))
  lines(density(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ$classification == 1]), col = cols[1], main = "")
  hist(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ$classification == 2], col = cols[2],
       main = "", breaks = seq(from = 0, to = 30, by = 2), freq = F, xlab = "", xlim = c(0,30))
  lines(density(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ$classification == 2]), col = cols[2], main = "")

  par(mfrow = c(1,1))
  
  
  
})

LatentGaussians_Simultaneous_ASSQ_12_24_6to8 <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_sum) , c(lcavars_6to8Included[-(1:2)], "ASSQ_6to8_sum") ] ), 
                                             G = 1:3 )
attr(LatentGaussians_Simultaneous_ASSQ_12_24_6to8, "Desc") <- "Simultaneous ASSQ and d-vit\n12mo, 24mo, 6 to 8."

with(na.omit( df[ !is.na(df$ASSQ_6to8_sum) , c(lcavars_6to8Included[-(1:2)], "ASSQ_6to8_sum") ] ) , expr = {
  
  par(mfrow = c(2,2))
  
  matplot(LatentGaussians_Simultaneous_ASSQ_12_24_6to8$parameters$mean[1:3,], type = "b")
  hist(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ_12_24_6to8$classification == 1], col = cols[1], 
       main = "", breaks = seq(from = 0, to = 30, by = 2), freq = F, xlab = "", ylab = "", xlim = c(0,30))
  lines(density(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ_12_24_6to8$classification == 1]), col = cols[1], main = "")
  hist(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ_12_24_6to8$classification == 2], col = cols[2],
       main = "", breaks = seq(from = 0, to = 30, by = 2), freq = F, xlab = "", xlim = c(0,30))
  lines(density(ASSQ_6to8_sum[LatentGaussians_Simultaneous_ASSQ_12_24_6to8$classification == 2]), col = cols[2], main = "")
  
  par(mfrow = c(1,1))
  
  
  
})
# LPA with censored data ------

df$Korj_Napa25OHD_censored <- df$Korj_Napa25OHD
df$Korj_Napa25OHD_censored[df$Korj_Napa25OHD_censored > 250] <- 250
LatentGaussians_censoredData <- Mclust( data = na.omit( df[ , c(lcavars_nonapa, "Korj_Napa25OHD_censored") ] ), 
                                     G = 1:6 )
attr(LatentGaussians_censoredData, "Desc") <- "Censored data\nRask, 12mo, 24mo, 6 to 8."
LatentGaussians_censoredData2g <- Mclust( data = na.omit( df[ , c(lcavars_nonapa, "Korj_Napa25OHD_censored") ] ), 
                                        G = 2 )
attr(LatentGaussians_censoredData2g, "Desc") <- "Censored data\nRask, 12mo, 24mo, 6 to 8.\n Forced to 2 groups."

# LPA with 12mo, 24mo, 6to8 -----

LatentGaussians_1_2_6to8 <- Mclust( data = na.omit( df[  , lcavars_6to8Included[ -c(1,2) ] ] ), 
                               G = 1:6 )
attr(LatentGaussians_1_2_6to8, "Desc") <- "12mo, 24mo, 6 to 8 years."


  #
########################### Save ############################### 

if( FALSE ) {

writepath <- "Z:/psy_vidi/Samuel VIDI 6-8y follow-up/ASSQMaster data - Sakari/ASSQMaster_Profiles.sav"
write_sav(df, writepath)

}


# Regressions ------


with(df[ which(df$id %in% na.omit( df[ , c("id", lcavars_6to8Included[ -(1:2) ])] )$id) ,  ], {
  classific <- factor(as.list(.GlobalEnv)$LatentGaussians_1_2_6to8$classification)
  summary(lm( ASSQ_6to8_mean ~ classific ))
  } )


with(df[ which(df$id %in% na.omit( df[ , c("id", lcavars_6to8Included[ -(2) ])] )$id) ,  ], {
  classific <- factor(as.list(.GlobalEnv)$LatentGaussians_6to8_nonapa$classification)
  summary(lm( ASSQ_6to8_mean ~ classific ))
} )

with(df[ which(df$id %in% na.omit( df[ , c("id", lcavars_6to8Included[ -(2) ])] )$id) ,  ], {
  classific <- factor(as.list(.GlobalEnv)$LatentGaussians_6to8_nonapa$classification)
  summary(lm( ASSQ_6to8_mean ~ classific ))
} )

summary(lm( df,
    formula = ASSQ_6to8_mean ~ LDvitProfile_NoNapa_to_8year))

summary(lm( df[df$sukupuoli == 1, ],
            formula = ASSQ_6to8_mean ~ LDvitProfile_NoNapa_to_8year)) # Males
summary(lm( df[df$sukupuoli == 2, ],
            formula = ASSQ_6to8_mean ~ LDvitProfile_NoNapa_to_8year)) # Females
summary(lm( df,
            formula = ASSQ_6to8_mean ~ LDvitProfile_NoNapa_to_8year * factor(sukupuoli))) # Interaction
summary(lm( df,
            formula = ASSQ_6to8_mean ~ 
              LDvitProfile_NoNapa_to_8year * factor(sukupuoli) + 
              factor(äidinkoulutus))) # Interaction and SES contrl
summary(lm( df,
            formula = scale(ASSQ_6to8_mean) ~ 
              factor(LDvitProfile_NoNapa_to_8year) * factor(sukupuoli) + 
              factor(äidinkoulutus) + 
              factor(isankoulutus))) # Interaction and SES contrl
emmeans(lm( df,
            formula = scale(ASSQ_6to8_sum) ~ 
              factor(LDvitProfile_NoNapa_to_8year) * factor(sukupuoli) + 
              factor(äidinkoulutus) + 
              factor(isankoulutus)), specs = c("LDvitProfile_NoNapa_to_8year", "sukupuoli"), )
hist(df$ASSQ_6to8_mean)
plot(density(na.omit(df[df$LDvitProfile_NoNapa_to_8year == 2, ]$ASSQ_6to8_sum)), col = "blue")
lines(density(na.omit(df[df$LDvitProfile_NoNapa_to_8year == 1, ]$ASSQ_6to8_sum)), col = "red")






