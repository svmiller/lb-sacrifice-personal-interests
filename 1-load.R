library(foreign)
library(countrycode)
library(car)
library(lattice)
library(lme4)
library(Zelig)

# Make sure you know where these are. Be mindful of differences in your versions.
LB03 <- read.dta("~/Dropbox/data/latinobarometro/dta/latinobarometro2003_eng.dta", convert.factors = FALSE)
LB06 <- read.dta("~/Dropbox/data/latinobarometro/dta/Latinobarometro_2006_eng.dta", convert.factors = FALSE)
