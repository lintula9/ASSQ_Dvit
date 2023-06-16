# Data fetch and data management.

source("Scripts/Libraries.R")

# Newer data set became avaialable 30.5.
# 6 to 8 year old data is available.
# dataPath <- "Z:/psy_vidi/Samuel VIDI 6-8y follow-up/Master2022.sav"
# df <- read_spss(dataPath)

# Data modifications (if necessary) here:

dataPath2 <- "Z:/psy_vidi/Samuel VIDI 6-8y follow-up/ASSQMaster data - Sakari/ASSQMaster_SakariMod.sav"
df <- read_spss(dataPath2)

# Remove case 313.

df$id[grep("313", df$id)]
