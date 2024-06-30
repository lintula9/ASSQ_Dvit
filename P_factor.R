## P -factor

# Load the data
library(haven)
vastsynt <- read_sav("Z:/psy_vidi/Vanha_VIDI-kansio/0 Vastasyntyneisyys seuranta/VIDI_vastasyntynyt_all_21032018.sav")

# Remove 0.5 value from data.
for ( i in vars) {
  
  vastsynt = vastsynt[ !( vastsynt[ , i, drop = T ] %in% c(0.5,1.5) ) , ]
  }

vastsynt = vastsynt[ vastsynt$s17k35 != 0.5 , ]
# vars <- scan("clipboard", what = "character")
# write(vars, file = "vars.txt")

library(lavaan)

efares = efa(vastsynt[ , vars], nfactors = 1, rotation = "none", estimator = "DWLS", ordered = T, std.lv = T, )
