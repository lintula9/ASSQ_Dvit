# ICA tests for d-vitamin and inflammation
# Source Libraries.R

lcavars <- c("Rask_S25OHD", 
             "Korj_Napa25OHD",
             "S25OHD_12kk",
             "S25OHD_24kk")
ProDenICA(x = na.omit(df[,lcavars]),trace = T)