# Packages.

package_names <- c("haven", "ggplot2", "poLCA")

for (i in package_names){
  if ( !requireNamespace( i, 
                          quietly = F )) {
    install.packages( i )
  }
  library( i, character.only = TRUE )}
