# ICA tests for d-vitamin and inflammation
# Source Libraries.R

lcavars <- c("Rask_S25OHD", 
             "Korj_Napa25OHD",
             "S25OHD_12kk",
             "S25OHD_24kk")
ICAfitDvit <- ProDenICA(x = na.omit(df[,lcavars]),trace = T, k = 4)



CRPvars <- names(df)[grep("CRP",names(df))][1:4]
ICAfitCRP <- ProDenICA(x = na.omit(df[,CRPvars]),trace = T, k = 4)

ICAfitCRP$W
var(ICAfitCRP$s)

ICAfitCRP2 <- fastICA::fastICA(na.omit(df[,CRPvars]), n.comp = 4)

pca(na.omit(df[,CRPvars]), nfactors = 1)
