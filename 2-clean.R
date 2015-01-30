#######################
## Clean LB03 first. ##
#######################

LB03$year <- 2003

LB03$country <- NA

LB03$country[LB03$idenpa == 1] <- "Argentina"
LB03$country[LB03$idenpa == 2] <- "Bolivia"
LB03$country[LB03$idenpa == 3] <- "Brazil"
LB03$country[LB03$idenpa == 4] <- "Colombia"
LB03$country[LB03$idenpa == 5] <- "Costa Rica"
LB03$country[LB03$idenpa == 6] <- "Chile"
LB03$country[LB03$idenpa == 7] <- "Ecuador"
LB03$country[LB03$idenpa == 8] <- "El Salvador"
LB03$country[LB03$idenpa == 9] <- "Guatemala"
LB03$country[LB03$idenpa == 10] <-  "Honduras"
LB03$country[LB03$idenpa == 11] <- "Mexico"
LB03$country[LB03$idenpa == 12] <- "Nicaragua"
LB03$country[LB03$idenpa == 13] <- "Panama"
LB03$country[LB03$idenpa == 14] <- "Paraguay"
LB03$country[LB03$idenpa == 15] <- "Peru"
LB03$country[LB03$idenpa == 16] <- "Uruguay"
LB03$country[LB03$idenpa == 17] <- "Venezuela"

LB03$ccode <- countrycode(LB03$country,"country.name","cown")

## Clean response variable of interest (p22gb_b)
################################################
# "People should be prepared to sacrifice personal interest if best for country.
# 1 = strongly agree, 2 = agree, 3 = disagree, 4 = strongly disagree, 0 = DK/NA

LB03$sacrifice <- LB03$p22gb_b

# Create a dummy. 1 or 2 = 1, 3 or 4 = 0.

LB03$sacrificedummy <- with(LB03, recode(p22gb_b, "1=1; 2=1; 3=0; 4=0"))
with(LB03, table(sacrificedummy, sacrifice)) # Yep, we did it right.


## Get basic sociodemographic stuff. We'll keep it simple.
##########################################################

# Age, in years. Let's check first to see if it's actually interval. Hopefully they didn't condense it.
summary(LB03$s2)
histogram(LB03$s2)
LB03$age <- with(LB03, recode(s2, "0=NA"))

# Gender. 1 = Male, 2 = Female
summary(LB03$s1)
LB03$female <- with(LB03, s1 - 1)
with(LB03, table(female, s1)) # Yep, we did it right.



# Did respondent complete high school? This variable is kind of wonky.
LB03$hsed <- with(LB03, recode(s6, "1:12=0; 13:17=1")) 

# I'm going to take a few other variables from my judicial confidence analyses.

LB03$ses <- with(LB03, recode(s16, "1=2; 2=1; 3=0; 4=-1; 5=-2"))

LB03$unemployed <- with(LB03, recode(s8a, "1:3=0; 4=1; 5:7=0"))
LB03$ideology <- with(LB03, recode(p60st, "96:99 = NA"))

LB03$satisdem <- with(LB03, recode(p15st, "0=NA; 8=NA; 1=4; 2=3; 3=2; 4=1")) # We just inverted the scale here.
LB03$preseconc <- with(LB03, recode(p1st, "0=NA; 8=NA; 1=2; 2=1; 3=0; 4=-1; 5=-2")) # Higher = country doing good.

LB03 <- with(LB03, data.frame(year, country, ccode, sacrifice, sacrificedummy, age, female, hsed, ses, unemployed, ideology, satisdem, preseconc))


#####################
## Clean LB06 now. ##
#####################

LB06$year <- 2006

LB06$country <- NA

LB06$country[LB06$idenpa == 1] <- "Argentina"
LB06$country[LB06$idenpa == 2] <- "Bolivia"
LB06$country[LB06$idenpa == 3] <- "Brazil"
LB06$country[LB06$idenpa == 4] <- "Colombia"
LB06$country[LB06$idenpa == 5] <- "Costa Rica"
LB06$country[LB06$idenpa == 6] <- "Chile"
LB06$country[LB06$idenpa == 7] <- "Ecuador"
LB06$country[LB06$idenpa == 8] <- "El Salvador"
LB06$country[LB06$idenpa == 9] <- "Guatemala"
LB06$country[LB06$idenpa == 10] <-  "Honduras"
LB06$country[LB06$idenpa == 11] <- "Mexico"
LB06$country[LB06$idenpa == 12] <- "Nicaragua"
LB06$country[LB06$idenpa == 13] <- "Panama"
LB06$country[LB06$idenpa == 14] <- "Paraguay"
LB06$country[LB06$idenpa == 15] <- "Peru"
LB06$country[LB06$idenpa == 16] <- "Uruguay"
LB06$country[LB06$idenpa == 17] <- "Venezuela"
LB06$country[LB06$idenpa == 18] <- "Spain"
LB06$country[LB06$idenpa == 19] <- "Dominican Republic"

LB06$ccode <- countrycode(LB06$country,"country.name","cown")

## Clean response variable of interest (p22na_e)
################################################
# "People should be prepared to sacrifice personal interest if best for country.
# 1 = strongly agree, 2 = agree, 3 = disagree, 4 = strongly disagree, 0 = DK/NA

LB06$sacrifice <- LB06$p22na_e

# Recode 0s to = NA
LB06$sacrifice <- with(LB06, recode(sacrifice, "0 = NA"))

# Create a dummy. 1 or 2 = 1, 3 or 4 = 0.

LB06$sacrificedummy <- with(LB06, recode(p22na_e, "1=1; 2=1; 3=0; 4=0; 0 = NA"))
with(LB06, table(sacrificedummy, sacrifice)) # Yep, we did it right.


## Get the other stuff.

LB06$age <- with(LB06, recode(s7, "0=NA"))
LB06$female <- with(LB06, recode(s6, "-2=NA; 1=0; 2=1"))
LB06$hsed <- with(LB06, recode(s11, "1:12=0; 13:17=1")) # This changed, it seems, in 2000.
LB06$ses <- with(LB06, recode(s24, "0=NA; 1=2; 2=1; 3=0; 4=-1; 5=-2"))
LB06$unemployed <- with(LB06, recode(s13a, "0=NA; 1:3=0; 4=1; 5:7=0"))
LB06$ideology <- with(LB06, recode(p47st, "96:99 = NA"))

LB06$satisdem <- with(LB06, recode(p21st, "0=NA; 8=NA; 1=4; 2=3; 3=2; 4=1")) # We just inverted the scale here.
LB06$preseconc <- with(LB06, recode(p2st, "0=NA; 8=NA; 1=2; 2=1; 3=0; 4=-1; 5=-2")) # Higher = country doing good.

LB06 <- with(LB06, data.frame(year, country, ccode, sacrifice, sacrificedummy, age, female, hsed, ses, unemployed, ideology, satisdem, preseconc))
 
#############################
## Rbind the two together. ##
#############################

Data <- rbind(LB03, LB06)

## Standardize variables by two standard deviations, as I like to do.

Data$z.age <- with(Data, (age - mean(age, na.rm = TRUE))/(2*sd(age, na.rm = TRUE)))
Data$z.ses <- with(Data, (ses - mean(ses, na.rm = TRUE))/(2*sd(ses, na.rm = TRUE)))
Data$z.ideology <- with(Data, (ideology - mean(ideology, na.rm = TRUE))/(2*sd(ideology, na.rm = TRUE)))
Data$z.satisdem <- with(Data, (satisdem - mean(satisdem, na.rm = TRUE))/(2*sd(satisdem, na.rm = TRUE)))
Data$z.preseconc <- with(Data, (preseconc - mean(preseconc, na.rm = TRUE))/(2*sd(preseconc, na.rm = TRUE)))

write.table(Data,file="data.csv",sep=",",row.names=F,na="")
