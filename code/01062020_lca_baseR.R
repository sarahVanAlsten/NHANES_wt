#######################################################
# Author: Sarah Van Alsten                            #
# Date Created: December 09, 2019                     #
# Purpose: Code to run Latent Class Causal Analysis   #
# Data Used: NHANES 07, 09, 11                        #
# Packages Used: lcca                                 #
# Last Update: December 09, 2019                      #
#######################################################
#run this in R.3.0.0!
#open up the needed library
library(lcca)

#read in the data
lcaDat <- read.csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019lca.csv")

#first write a function to test LCA classes and get AIC/BIC to select
get_AIC_BIC <- function(data, maxclass = 10){
  aicVec <- c(rep(0,maxclass-1))
  bicVec <- c(rep(0,maxclass-1))
  
  set.seed(50)
  for (i in 2:maxclass){
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
              iter.max = 20000)
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC(lcaDat, 7)#5 classes optimal

#see what those classes are, including popl survey adjustment
class5 <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
summary(class5)

#are classes still invariant by sex?
set.seed(87)
class5.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                 nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                 flatten.gammas =1, iter.max = 20000, group = factor(Male),
                 constrain.rhos = T, 
                 weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class5.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Male),
                  constrain.rhos = F, 
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class5.con, class5.unc) #yes there is invariance: ChiSq = 1953.609, df = 65, p < 0.001

summary(class5.unc)

#do separately for men and women to get desired # classes in each
male <- lcaDat[lcaDat$Male == 2, ]
fem <- lcaDat[lcaDat$Male == 1,]

get_AIC_BIC(male, 7) #4/5 classes
get_AIC_BIC(fem, 7) #5/6/7 classes

#see what classes are to help decide
class5m <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
		subpop = (Male ==2))
class4m <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = 4, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
		subpop = (Male ==2))
summary(class5m)
summary(class4m) #this looks more plausible: other just distinguished ow vs ob who want to lose  wt

class5f <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
		subpop = (Male ==1))
class6f <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = 6, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
		subpop = (Male ==1))
class7f <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              nclass = 7, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
		subpop = (Male ==1))

summary(class5f) #while one class is v small, it's the too thin and I expect it to be rare anyway. Adding more
#categories would likely overcomplicate. stick with 5 class soln

fem.nona <- fem[!is.na(fem$orient),]
male.nona <- male[!is.na(male$orient),]

#are classes invariant by orientation?
class5.con.f <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                 nclass = 5, data = fem.nona, tol = 1e-06, flatten.rhos = 1, 
                 flatten.gammas =1, iter.max = 20000, group = factor(orient),
                 constrain.rhos = T, 
                 weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class5.unc.f <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 5, data = fem.nona, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(orient),
                  constrain.rhos = F, 
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class5.con.f, class5.unc.f) #yes there is invariance: ChiSq = 505, df = 195, p < 0.001

summary(class5.unc.f)


table(lcaDat$orient)
table(lcaDat$FSDHH)
table(lcaDat$edu)
table(lcaDat$maritalstatus)
table(lcaDat$age4)
table(lcaDat$Race)
table(lcaDat$Income)

lcaDat$orient <- lcaDat$orient - 1
lcaDat$age4 <- lcaDat$age4 - 1

#try doing this as a lcacov (essentially a regression to predict determinants of LC)

lcaCov <- lcacov(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~Male + factor(Race) +
			factor(Income) + factor(age4) + factor(orient) +factor(edu),
			 nclass = 5, data = lcaDat, tol = 1e-06,  
		    stabilize.alphas =1, flatten.rhos = 1, weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(lcaCov)


#stratify by sex and run individual lcacov for males vs females
lcaCovMale <- lcacov(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~ factor(Race) +
			 factor(Income) + factor(age4) + factor(orient) +factor(edu),
			 nclass = 4, data = male, tol = 1e-06,  
		    stabilize.alphas =1, flatten.rhos = 1, weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(lcaCovMale)


#stratify by sex and run individual lcacov for males vs females
lcaCovFem <- lcacov(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~ factor(Race) +
			 factor(Income) + factor(age4) + factor(orient) +factor(edu),
			 nclass = 5, data = fem, tol = 1e-06,  
		    stabilize.alphas =1, flatten.rhos = 1, weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(lcaCovFem)

lcaDat$depressionBinary <- lcaDat$depressionBinary - 1
fem$depressionBinary <- factor(fem$depressionBinary -1)
fem <- lcaDat[lcaDat$Male == 1,]
table(lcaDat$orient)
lcaDat$hetero <- ifelse(is.na(lcaDat$orient), NA,ifelse(lcaDat$orient == 2, 1, 0))
lcaDat$BMI2 <- ifelse(is.na(lcaDat$BMIcat), NA, 
			ifelse(lcaDat$BMIcat <=3, lcaDat$BMIcat,4))


xtabs(~lcaDat$Race + lcaDat$Male + lcaDat$depressionBinary)
noAsian <- lcaDat[lcaDat$Race != 4,]
lcaDat$minority <- ifelse(lcaDat$Race == 1, 0, 1)

table(lcaDat$minority, lcaDat$depressionBinary)

#now, use this info to see how classes related to depression
depLCCA <- lcca(formula.treatment = cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMI2)~factor(minority),
			formula.outcome = depressionBinary ~factor(minority) ,
			nclass = 2, data = lcaDat, tol = 1e-06, iter.max = 30000, 
		      stabilize.alphas =1, flatten.rhos = 1)

summary(depLCCA)


##########################################################################
#create a survey sample design by the primary sampling unit, the stratum, and the revised cycle weight
#(for combining surveys from 5, 4, 3, 2, and 1) cycles

dclus3 <-survey::svydesign(id=~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights=~WTMEC6YR, 
                           nest = TRUE, 
                           data=dat)


library(poLCA)

#lca.dat <- yw2.lose %>%
#  select(SEQN, RIAGENDR, BMIcat, considerWt, doAboutWt, contains("whyLose"))

#lca.dat$BMIcat <- ifelse(is.na(lca.dat$BMIcat), 7, lca.dat$BMIcat)
#lca.dat$considerWt <- ifelse(is.na(lca.dat$considerWt), 7, lca.dat$considerWt)
#lca.dat$doAboutWt <- ifelse(is.na(lca.dat$doAboutWt), 7, lca.dat$doAboutWt)

#lca.dat <- lca.dat %>%
#  mutate_all(.funs = replace_na, replace = 2)

#lca.dat$BMIcat <- ifelse(lca.dat$BMIcat == 7, NA, lca.dat$BMIcat)
#lca.dat$considerWt <- ifelse(lca.dat$considerWt == 7, NA, lca.dat$considerWt)
#lca.dat$doAboutWt <- ifelse(lca.dat$doAboutWt == 7,NA, lca.dat$doAboutWt)
yw2.lose <- read.csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\yw2lose.csv")
#split by gender
fem.youth <- subset(yw2.lose, RIAGENDR ==2)
male.youth <- subset(yw2.lose, RIAGENDR !=2)


lc.f <- lca(cbind(whyLose.look,
                  whyLose.health,
                  whyLose.sports,
                  whyLose.tease,
                  whyLose.clothes,
                  whyLose.boyslike,
                  whyLose.girlslike,
                  whyLose.friendare,
                  whyLose.familyare,
                  whyLose.parentWants,
                  whyLose.coachWants,
                  whyLose.docWants,
                  whyLose.amFat,
                  whyLose.feelBetter,
                  whyLose.other)~1,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
            data = fem.youth,
            nclass = 3,
            iter.max = 20000,
            flatten.rhos = 1)
summary(lc.f)

lc.m <- lca(cbind(whyLose.look,
                  whyLose.health,
                  whyLose.sports,
                  whyLose.tease,
                  whyLose.clothes,
                  whyLose.boyslike,
                  whyLose.girlslike,
                  whyLose.friendare,
                  whyLose.familyare,
                  whyLose.parentWants,
                  whyLose.coachWants,
                  whyLose.docWants,
                  whyLose.amFat,
                  whyLose.feelBetter,
                  whyLose.other)~1,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
            data = male.youth,
            nclass = 3,
            iter.max = 20000,
            flatten.rhos = 1)
summary(lc.m)

lc.f2 <- lcacov(cbind(whyLose.look,
                  whyLose.health,
                  whyLose.sports,
                  whyLose.tease,
                  whyLose.clothes,
                  whyLose.boyslike,
                  whyLose.girlslike,
                  whyLose.friendare,
                  whyLose.familyare,
                  whyLose.parentWants,
                  whyLose.coachWants,
                  whyLose.docWants,
                  whyLose.amFat,
                  whyLose.feelBetter,
                  whyLose.other)~factor(RIAGENDR) + factor(BMIcat)+ RIDAGEYR,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA,
            data = yw2.lose,
            nclass = 3,
            iter.max = 20000,
            flatten.rhos = 1)
summary(lc.f2)