# Packages.

package_names <- c("haven", "ggplot2", "mclust", "mixtools", "flexmix")

for (i in package_names){
  if ( !requireNamespace( i, 
                          quietly = T )) {
    message( "\n\n\t\tNo package ", i, ". Installing package ", i, ".\n\n")
    install.packages( i )
  }
  library( i, character.only = TRUE )}