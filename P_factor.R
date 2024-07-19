## P -factor


# vars <- scan("clipboard", what = "character")
# write(vars, file = "vars.txt")
vars <- readLines("vars.txt")

# Remove 0.5 value from data and drop variables with only one cell with more than 5 observations and make them ordered.
keepvars = c()

for ( i in vars) {
  
  vastsynt = vastsynt[ !( vastsynt[ , i, drop = T ] %in% c(0.5,1.5, 9) ) , ]
  class(vastsynt[, i, drop = T ]) <- c(class(vastsynt[ , i, drop  = T  ]), "ordered")
  keepvars[ i ] = length(table((na.omit(vastsynt[ , vars])[ , i, drop = T]))) > 1
  
  }

# Which variables were dropped?
droppedvars = vars[!keepvars]; message(paste0("Variables ",droppedvars), " had only 1 category, with non-zero cell count, and were dropped.")


# scree plot.
pcor = lavaan::lavCor(na.omit(vastsynt[ , vars[keepvars]]), ordered = T)
plot(eigen(pcor)$values, type = "b", xlab = "Component", 
     ylab = "Eigenvalue", main = "Scree plot"); abline(h = 1, col = "red")

# Run the EFA
library(rstan)

# Define data for stan
# Convert the data frame to a matrix and identify missing values
Y <- as.matrix(vastsynt[ , vars[keepvars]])
Y[is.na(Y)] <- -1  # Use -1 to indicate missing values in the data

# Identify the missing value indices
ii <- which(Y == -1, arr.ind = TRUE)[, 1]
jj <- which(Y == -1, arr.ind = TRUE)[, 2]

N = nrow(Y) # Number of observations
K = length(vars[keepvars]) # Number of variables
J = sapply(na.omit(vastsynt[ , vars[keepvars]]), 
           function(x) as.integer(length(table(x))) , simplify = "vector") # Number of categories in the ordinal variables

# Define your data list for Stan
data_list <- list(
  N = nrow(vastsynt),
  K = length(vars[keepvars]),
  J = J,
  Y = Y,
  N_mis = length(ii),
  ii = ii,
  jj = jj
)

# Retrieve the model from P_factor.stan
model <- stan_model("P_factor.stan")

# Run the model.
fit <- sampling(model, 
                data = data_list, 
                chains = 4, iter = 2000, 
                warmup = 1000, 
                cores = 8)

# Save the fitted model
saveRDS(fit, file = "P_factor.rds")
