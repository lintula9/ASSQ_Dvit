##### LCA:

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



# LCA without Napa25OHD; pre 6 to 8 year ------------

LatentGaussians_NoNapa <- Mclust( data = na.omit( df[ , lcavars_nonapa ] ), 
                                 G = 1:6 )



# LCA with 6-8 year measurements included --------------

LatentGaussians_All <- Mclust( data = na.omit( df[  , lcavars_6to8Included ] ), 
                               G = 1:6 )   # Runs into missing data problems: only 224 available.




# LCA, stratified by sex. Using all pre 6-8 year data. ---------

LatentGaussians_sexstrat1 <- Mclust( data = na.omit( df[ df$sukupuoli == 1 , lcavars ] ), 
                           G = 1:6 )
LatentGaussians_sexstrat2 <- Mclust( data = na.omit( df[ df$sukupuoli == 2 , lcavars ] ), 
                           G = 1:6 )


# LCA, including cases with ASSQ scores available, using 4 measuremnets (no 6 to 8 year measure) -----

LatentGaussians_OnlyWith_ASSQavailable <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars ] ), 
                               G = 1:6 )

# LCA, including cases with ASSQ scores available, using 3 measuremnets (no 6 to 8 year measure) -----

LatentGaussians_OnlyWith_ASSQavailable_3measures <- Mclust( data = na.omit( df[ !is.na(df$ASSQ_6to8_mean) , lcavars_nonapa ] ), 
                                                  G = 1:6 )

















# Save results, as a new SPSS file. -----

  # df$profile <- NA
  # df[ which( df$id %in% na.exclude(df[ , c(lcavars_nonapa, "id" ) ])$id ) , ]$profile <- LatentGaussians_NoNapa$classification
  # df$LDvitProfile_Rask_to_2year <- NA # 4 measurements profiles.
  # df[ which( df$id %in% na.exclude(df[ , c(lcavars, "id" ) ])$id ) , ]$LDvitProfile_Rask_to_2year <- LatentGaussians$classification
  # 
  # df$LDvitProfile_NoNapa_to_2year <- NA # No napa, 3 measurements.
  # df[ which( df$id %in% na.exclude(df[ , c(lcavars_nonapa, "id" ) ])$id ) , ]$LDvitProfile_NoNapa_to_2year <- LatentGaussians_NoNapa$classification
  # 
  # writepath <- "Z:/psy_vidi/Samuel VIDI 6-8y follow-up/ASSQMaster data - Sakari/ASSQMaster_Profiles.sav"
  # write_sav(df, writepath)
