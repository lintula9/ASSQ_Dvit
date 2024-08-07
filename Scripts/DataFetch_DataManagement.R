# Data fetch and data management.

source("Scripts/Libraries.R", echo = T)

# Newer data set became avaialable 30.5.
# 6 to 8 year old data is available.
# dataPath <- "Z:/psy_vidi/Samuel VIDI 6-8y follow-up/Master2022.sav"
# df <- read_spss(dataPath)

# Data modifications (if necessary) here:

dataPath2 <- "Z:/psy_vidi/Samuel VIDI 6-8y follow-up/ASSQMaster data - Sakari/ASSQMaster_SakariMod.sav"
message("Loading data....")
df <- read_spss(dataPath2)
message("Done.")
# Remove case 313.

df$id[grep("313", df$id)] # WHO IS IT?

# Mclust AIC calculator.

AIC.mclust <- function(x) 2*x$df - 2*x$loglik


# Add cognitive measure availability indicator --------------

CognitiveAvailable <- strsplit(read_file("IdsWithCognitiveMeasures.txt"), split = "\n")[[1]]
CognitiveAvailable <- gsub("\r", x = CognitiveAvailable, replacement = "")
CognitiveAvailable <- df$id %in% CognitiveAvailable


cognitive_data <- "Z:/psy_vidi/Data 6 to 8 yrs/Cognitive tests/Cognitive tests data/Data_cognitive_2excluded__3subjects_excluded_combined_S25OHD_Finnish_speaking.sav"
df_cognitive <- read_spss(cognitive_data)

df <- left_join(df, df_cognitive[ , c("ID","FSIQ")], by = c("id" = "ID"))






# Bayesian analysis data: -------------

bayesVars <- c( "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 

bayesdf <- na.exclude( df[ , bayesVars ] )
Covariates <- bayesdf[ , c("ikäASSQ", "sukupuoli")]
names(Covariates) <- c("Age", "Sex")
Covariates$Age <- scale(Covariates$Age)
Covariates$Sex <- factor(Covariates$Sex, levels = c(1,2), labels = c("Male", "Female"))

# Create data list for stan.
stan_data = list(
  
  # D-vitamin variables:
  Measurements = matrix(cbind(cbind(
    scale( bayesdf$S25OHD_12kk ), 
    scale( bayesdf$S25OHD_24kk )),
    scale( bayesdf$D25OHD_nmol_l_6to8 )),
    ncol = 3, 
    dimnames = list(NULL, c("Toddlerhood", "Infancy", "Childhood"))),
  Nmeasurements = 3,
  
  # Covariates:
  Covariates = subset( model.matrix( ~ Age + Sex, data = Covariates) , select = -c(`(Intercept)`) ),
  NCovariates = ncol(Covariates),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 1/3, 1/6, 1/6 ), 
                             c( 1/6, 1/3, 1/6 ), 
                             c( 1/6, 1/6, 1/3 ) ),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 1 )
  
)


bayesVars2 <- c( "Rask_S25OHD", "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                 "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdf2 <- na.exclude( df[ , bayesVars2 ] )
Covariates2 <- bayesdf2[ , c( "ikäASSQ", "sukupuoli" ) ]
names(Covariates2) <- c( "Age", "Sex" )

Covariates2$Age <- scale( Covariates2$Age )
Covariates2$Sex <- factor( Covariates2$Sex, levels = c(1,2), labels = c("Male", "Female") )


Measurements2 <- scale(bayesdf2[ , c( "Rask_S25OHD", "S25OHD_12kk" , 
                                      "S25OHD_24kk" , "D25OHD_nmol_l_6to8" ) ])

stan_data2 = list(
  
  # D-vitamin variables:
  Measurements = Measurements2,
  Nmeasurements = ncol(Measurements2),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) ),
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf2 )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4, 1/4 ), 
                              c( 1/4, 2/4, 1/4, 1/4 ), 
                              c( 1/4, 1/4, 2/4, 1/4 ),
                              c( 1/4, 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/4, 1/4, 1/4, 1/4 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0, 1 )
  
)


stan_data3 = append(stan_data2,list(
  
  # Where ASSQ is censored:
  ASSQ_L = min(scale( bayesdf2$ASSQ_6to8_mean )),
  
  # Is the value zero?
  zeroVal = as.numeric(bayesdf2$ASSQ_6to8_mean == 0)
)
)



# Include napa:

bayesVarsNapa <- c( "Rask_S25OHD", "Korj_Napa25OHD", "S25OHD_12kk" , "S25OHD_24kk" , "D25OHD_nmol_l_6to8" , # Toddlerhood, infancy and pre-school.
                 "ASSQ_6to8_mean", "sukupuoli" , "ikäASSQ" ) 
bayesdfNapa <- na.exclude( df[ , bayesVarsNapa ] )
CovariatesNapa <- bayesdfNapa[ , c( "ikäASSQ", "sukupuoli" ) ]
names(CovariatesNapa) <- c( "Age", "Sex" )

CovariatesNapa$Age <- scale( CovariatesNapa$Age )
CovariatesNapa$Sex <- factor( CovariatesNapa$Sex, levels = c(1,2), labels = c("Male", "Female") )


MeasurementsNapa <- scale(bayesdfNapa[ , c( "Rask_S25OHD","Korj_Napa25OHD",
                                            "S25OHD_12kk" , "S25OHD_24kk" , 
                                            "D25OHD_nmol_l_6to8" ) ])

stan_dataNapa = list(
  
  # D-vitamin variables:
  Measurements = MeasurementsNapa,
  Nmeasurements = ncol(MeasurementsNapa),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = CovariatesNapa) , select = -c(`(Intercept)`) ),
  NCovariates = ncol(CovariatesNapa),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdfNapa$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdfNapa )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/5, 1/5, 1/5, 1/5, 1/5 ), 
                              c( 1/5, 2/5, 1/5, 1/5, 1/5  ), 
                              c( 1/5, 1/5, 2/5, 1/5, 1/5  ), 
                              c( 1/5, 1/5, 1/5, 2/5, 1/5  ),
                              c( 1/5, 1/5, 1/5, 1/5, 2/5  )),
  ExpAccumulationWeights = c( 1/5, 1/5, 1/5, 1/5, 1/5 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0, 0, 1 )
  
)


stan_dataNapa = append(stan_dataNapa,list(
  
  # Where ASSQ is censored:
  ASSQ_L = min(scale( bayesdfNapa$ASSQ_6to8_mean )),
  
  # Is the value zero?
  zeroVal = as.numeric(bayesdfNapa$ASSQ_6to8_mean == 0)
)
)



stan_data123 = list( # Pregnancy (1), 12mo (2), 24mo (3)
  
  # D-vitamin variables:
  Measurements = Measurements2[ , 1:3],
  Nmeasurements = ncol(Measurements2[ , 1:3]),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) ),
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf2 )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4 ), 
                              c( 1/4, 2/4, 1/4 ), 
                              c( 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0 )
  
)

stan_data123 = list( # Pregnancy (1), 12mo (2), 24mo (3)
  
  # D-vitamin variables:
  Measurements = Measurements2[ , 1:3],
  Nmeasurements = ncol(Measurements2[ , 1:3]),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) ),
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf2 )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4 ), 
                              c( 1/4, 2/4, 1/4 ), 
                              c( 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0 )
  
)

stan_data123 = list( # Pregnancy (1), 12mo (2), 24mo (3)
  
  # D-vitamin variables:
  Measurements = Measurements2[ , 1:3],
  Nmeasurements = ncol(Measurements2[ , 1:3]),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) ),
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean )),
  
  # N:
  N = dim( bayesdf2 )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4 ), 
                              c( 1/4, 2/4, 1/4 ), 
                              c( 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0 )
  
)

# Sex stratification:

males <- bayesdf$sukupuoli == 1

stan_data_males = list(
  
  # D-vitamin variables:
  Measurements = matrix(cbind(cbind(
    scale( bayesdf$S25OHD_12kk ), 
    scale( bayesdf$S25OHD_24kk )),
    scale( bayesdf$D25OHD_nmol_l_6to8 )),
    ncol = 3, 
    dimnames = list(NULL, c("Toddlerhood", "Infancy", "Childhood")))[ males, ],
  Nmeasurements = 3,
  
  # Covariates:
  Covariates = subset( model.matrix( ~ Age + Sex, data = Covariates) , select = -c(`(Intercept)`) )[ males, ],
  NCovariates = ncol(Covariates),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf$ASSQ_6to8_mean ))[ males ],
  
  # N:
  N = dim( bayesdf[males, ] )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 1/3, 1/6, 1/6 ), 
                              c( 1/6, 1/3, 1/6 ), 
                              c( 1/6, 1/6, 1/3 ) ),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 1 )
  
)
stan_data_females = list(
  
  # D-vitamin variables:
  Measurements = matrix(cbind(cbind(
    scale( bayesdf$S25OHD_12kk ), 
    scale( bayesdf$S25OHD_24kk )),
    scale( bayesdf$D25OHD_nmol_l_6to8 )),
    ncol = 3, 
    dimnames = list(NULL, c("Toddlerhood", "Infancy", "Childhood")))[ !males, ],
  Nmeasurements = 3,
  
  # Covariates:
  Covariates = subset( model.matrix( ~ Age + Sex, data = Covariates) , select = -c(`(Intercept)`) )[ !males, ],
  NCovariates = ncol(Covariates),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf$ASSQ_6to8_mean ))[ !males ],
  
  # N:
  N = dim( bayesdf[ !males, ] )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 1/3, 1/6, 1/6 ), 
                              c( 1/6, 1/3, 1/6 ), 
                              c( 1/6, 1/6, 1/3 ) ),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 1 )
  
)

males2 <- bayesdf2$sukupuoli == 1

stan_data2_males = list(
  
  # D-vitamin variables:
  Measurements = Measurements2[ males2, ],
  Nmeasurements = ncol(Measurements2),
  
  # Covariates:
  Covariates = subset( model.matrix( ~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) )[ males2, ],
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean ))[ males2 ],
  
  # N:
  N = dim( bayesdf2[ males2, ] )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 1/3, 1/6, 1/6, 1/6 ), 
                              c( 1/6, 1/3, 1/6, 1/6 ), 
                              c( 1/6, 1/6, 1/3, 1/6 ), 
                              c( 1/6, 1/6, 1/6, 1/3 ) ),
  ExpAccumulationWeights = c( 1/4, 1/4, 1/4, 1/4 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0, 1 )
  
)
stan_data2_females = list(
  
  # D-vitamin variables:
  Measurements = Measurements2[ !males2, ],
  Nmeasurements = ncol(Measurements2),
  
  # Covariates:
  Covariates = subset( model.matrix( ~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) )[ !males2, ],
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean ))[ !males2 ],
  
  # N:
  N = dim( bayesdf2[ !males2, ] )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 1/3, 1/6, 1/6, 1/6 ), 
                              c( 1/6, 1/3, 1/6, 1/6 ), 
                              c( 1/6, 1/6, 1/3, 1/6 ), 
                              c( 1/6, 1/6, 1/6, 1/3 ) ),
  ExpAccumulationWeights = c( 1/4, 1/4, 1/4, 1/4 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0, 1 )
  
)

stan_data123_males = list( # Pregnancy (1), 12mo (2), 24mo (3)
  
  # D-vitamin variables:
  Measurements = Measurements2[ males2, 1:3],
  Nmeasurements = ncol(Measurements2[ males2, 1:3]),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) )[ males2 ,],
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean ))[ males2 ],
  
  # N:
  N = dim( bayesdf2[ males2, ] )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4 ), 
                              c( 1/4, 2/4, 1/4 ), 
                              c( 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0 )

)

stan_data123_females = list( # Pregnancy (1), 12mo (2), 24mo (3)
  
  # D-vitamin variables:
  Measurements = Measurements2[ !males2, 1:3],
  Nmeasurements = ncol(Measurements2[ !males2, 1:3]),
  
  # Covariates:
  Covariates = subset( model.matrix(~ Age + Sex, data = Covariates2) , select = -c(`(Intercept)`) )[ !males2 ,],
  NCovariates = ncol(Covariates2),
  
  # Y outcome:
  ASSQ = as.vector(scale( bayesdf2$ASSQ_6to8_mean ))[ !males2 ],
  
  # N:
  N = dim( bayesdf2[ !males2, ] )[ 1 ],
  
  # Prior parameters:
  alpha = c( 1, 1, 1 ), # Dirichlet priors.
  
  # Expected weights for critical period hypotheses
  ExpSensitiveWeights = list( c( 2/4, 1/4, 1/4 ), 
                              c( 1/4, 2/4, 1/4 ), 
                              c( 1/4, 1/4, 2/4 )),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpCriticalChildhoodWeights = c( 0, 0, 0 )
  
)

# Load the data for possible p factor
library(haven)
vastsynt <- read_sav("Z:/psy_vidi/Vanha_VIDI-kansio/0 Vastasyntyneisyys seuranta/VIDI_vastasyntynyt_all_21032018.sav")
