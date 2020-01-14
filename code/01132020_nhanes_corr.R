#######################################################
# Author: Sarah Van Alsten                            #
# Date Created: January 13, 2020                      #
# Purpose: Correlations and individual models for wt  #
# perception in NHANES                                #
# Data Used: NHANES 07, 09, 11                        #
# Packages: tidyverse, MASS, readr, corrplot, descr,  #
#           lsr, polycor, survey                      #
# Last Update: January 13, 2020                       #
#######################################################
library(descr)
library(tidyverse)
library(MASS)
library(corrplot)
library(polycor)
library(survey)

# Data Management ---------------------------------------------------------

#read in the data
dat <- readr::read_csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019nhanes.csv")

#check that appropriate years used
table(dat$cycle)

#reminder for doing abt wt variable
#1: lost weight intentionally
#2: lost weight unintentionally
#3: tried to lose weight but did not
#4: tried to not gain weight (not included in 2013 cycle)
#5: all others (not trying to do anything)


# Correlations/Crosstabs ------------------------------------------------------------
#First, just do the crosstabs (without survey adjustment)
CrossTable(dat$BMIcat, dat$LikeToWeigh, chisq = T)
CrossTable(dat$BMIcat, dat$doingAbtWt, chisq = T)
CrossTable(dat$BMIcat, dat$ConsiderWt, chisq = T)
CrossTable(dat$LikeToWeigh, dat$doingAbtWt, chisq = T)
CrossTable(dat$LikeToWeigh, dat$ConsiderWt, chisq = T)
CrossTable(dat$doingAbtWt, dat$ConsiderWt, chisq = T)


#polychoric correlation can be used for ordinal vars
#or Kendall's tau
cor.test(dat$BMIcat, dat$LikeToWeigh, method = "kendall")
polychor(dat$BMIcat, dat$LikeToWeigh, std.err = T) #-0.707

cor.test(dat$BMIcat, dat$ConsiderWt, method = "kendall")
polychor(dat$BMIcat, dat$ConsiderWt, std.err = T) #0.7403

cor.test(dat$ConsiderWt, dat$LikeToWeigh, method = "kendall")
polychor(dat$ConsiderWt, dat$LikeToWeigh, std.err = T) #-0.8913

#for nominal... already had chisq, get the cramer's v for strength of assoc
lsr::cramersV(dat$BMIcat, dat$doingAbtWt) #0.175
lsr::cramersV(dat$LikeToWeigh, dat$doingAbtWt) #0.355
lsr::cramersV(dat$ConsiderWt, dat$doingAbtWt) #0.312



# Crosstabs with Survey Adjustment ----------------------------------------
svy <- svydesign(id = ~SDMVPSU, 
                 strata = ~SDMVSTRA, 
                 weights = ~WTMEC6YR, 
                 nest = TRUE, 
                 data = dat)


svychisq(~ConsiderWt + BMIcat, design = svy)
svychisq(~ConsiderWt + LikeToWeigh, design = svy)
svychisq(~ConsiderWt + doingAbtWt, design = svy)
svychisq(~LikeToWeigh + BMIcat, design = svy)
svychisq(~doingAbtWt + LikeToWeigh, design = svy)
svychisq(~BMIcat + doingAbtWt, design = svy)



# Regressions in Full Sample ----------------------------------------------
#unadjusted odds for depression
dep1.ltw <- glm(depressionBinary ~ relevel(factor(LikeToWeigh), ref = "0"), #ref = same
                data = dat,
                family = binomial(link = "logit"))
summary(dep1.ltw)

dep1.cow <- glm(depressionBinary ~ relevel(factor(ConsiderWt), ref = "0"), #ref = about right
                data = dat,
                family = binomial(link = "logit"))
summary(dep1.cow)

dep1.daw <- glm(depressionBinary ~ relevel(factor(doingAbtWt), ref = "5"), #ref = not doing anything
                data = dat,
                family = binomial(link = "logit"))
summary(dep1.daw)
################################################################################################
#add in BMI category
dep2.ltw <- glm(depressionBinary ~ relevel(factor(LikeToWeigh), ref = "0") + factor(BMIcat), 
                data = dat,
                family = binomial(link = "logit"))
summary(dep2.ltw)

dep2.cow <- glm(depressionBinary ~ relevel(factor(ConsiderWt), ref = "0") + factor(BMIcat), 
                data = dat,
                family = binomial(link = "logit"))
summary(dep2.cow)

dep2.daw <- glm(depressionBinary ~ relevel(factor(doingAbtWt), ref = "5") + factor(BMIcat), 
                data = dat,
                family = binomial(link = "logit"))
summary(dep2.daw)
##################################################################################################
#add in other potential confounders
dep3.ltw <- glm(depressionBinary ~ relevel(factor(LikeToWeigh), ref = "0") + factor(BMIcat) + factor(age4) +
                  factor(Male) + factor(maritalstatus) + factor(edu) + factor(Income) + factor(Race),
                data = dat,
                family = binomial(link = "logit"))
summary(dep3.ltw)

dep3.cow <- glm(depressionBinary ~ relevel(factor(ConsiderWt), ref = "0") + factor(BMIcat) + factor(age4) +
                  factor(Male) + factor(maritalstatus) + factor(edu) + factor(Income) + factor(Race), 
                data = dat,
                family = binomial(link = "logit"))
summary(dep3.cow)

dep3.daw <- glm(depressionBinary ~ relevel(factor(doingAbtWt), ref = "5") + factor(BMIcat) + factor(age4) +
                  factor(Male) + factor(maritalstatus) + factor(edu) + factor(Income) + factor(Race),
                data = dat,
                family = binomial(link = "logit"))
summary(dep3.daw)
######################################################################################################
#Interaction models w/ sex
dep4.ltw <- glm(depressionBinary ~ relevel(factor(LikeToWeigh), ref = "0")*factor(Male) +
                  factor(BMIcat) + factor(age4) +
                  factor(maritalstatus) + factor(edu) + factor(Income) + factor(Race),
                data = dat,
                family = binomial(link = "logit"))
summary(dep4.ltw)

dep4.cow <- glm(depressionBinary ~ relevel(factor(ConsiderWt), ref = "0")*factor(Male) +
                  factor(BMIcat) + factor(age4) +
                  factor(maritalstatus) + factor(edu) + factor(Income) + factor(Race), 
                data = dat,
                family = binomial(link = "logit"))
summary(dep4.cow)

dep4.daw <- glm(depressionBinary ~ relevel(factor(doingAbtWt), ref = "5")*factor(Male) + 
                  factor(BMIcat) + factor(age4) +
                  factor(maritalstatus) + factor(edu) + factor(Income) + factor(Race),
                data = dat,
                family = binomial(link = "logit"))
summary(dep4.daw)
##################################################################################3#
#Models with all 3 of the components


#no evidence of interaction with sex for any of them
#####################################################################################
#use odds.n.ends to get ORs/95% CI
odds.n.ends::odds.n.ends(dep1.ltw)
odds.n.ends::odds.n.ends(dep2.ltw)
odds.n.ends::odds.n.ends(dep3.ltw)

odds.n.ends::odds.n.ends(dep1.cow)
odds.n.ends::odds.n.ends(dep2.cow)
odds.n.ends::odds.n.ends(dep3.cow)

odds.n.ends::odds.n.ends(dep1.daw)
odds.n.ends::odds.n.ends(dep2.daw)
odds.n.ends::odds.n.ends(dep3.daw)
