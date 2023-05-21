# Packages.

package_names <- c("haven", "ggplot2", "flexmix")

for (i in package_names){
  if ( !requireNamespace( i, 
                          quietly = F )) {
    message("No package", i, ". Installing package.")
    install.packages( i )
  }
  library( i, character.only = TRUE )}
