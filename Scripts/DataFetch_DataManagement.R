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


# Add cognitive measure availability indicator

CognitiveAvailable <- strsplit(read_file("IdsWithCognitiveMeasures.txt"), split = "\n")[[1]]
CognitiveAvailable <- gsub("\r", x = CognitiveAvailable, replacement = "")
CognitiveAvailable <- df$id %in% CognitiveAvailable













# Bayesian analysis data:

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
  ExpCriticalWeights = list( c( 2/3, 1/6, 1/6 ), 
                             c( 1/6, 2/3, 1/6 ), 
                             c( 1/6, 1/6, 2/3 ) ),
  ExpAccumulationWeights = c( 1/3, 1/3, 1/3 ),
  ExpChildhoodCriticalWeights = c( 0, 0, 1 )
  
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
