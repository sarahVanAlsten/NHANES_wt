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
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
              nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
              iter.max = 20000)
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC(lcaDat, maxclass = 8) #3 classes

#now run with survey weights to see what those 3 calsses are
class3 <-lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
              nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
		flatten.gammas =1, iter.max = 20000,
		weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
summary(class3)


#now see if there is class invariance by gender
set.seed(87)
class3.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                 nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                 flatten.gammas =1, iter.max = 20000, group = factor(Male),
                 constrain.rhos = T, constrain.gammas = T,
                 weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class3.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Male),
                  constrain.rhos = F, constrain.gammas = T,
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class3.con, class3.unc) #yes there is invariance: ChiSq = 1084.811, df = 21, p < 0.001

lcaDat$BMIcat <- lcaDat$BMIcat - 1
#redo but also using BMI in the LCA
#first write a function to test LCA classes and get AIC/BIC to select
get_AIC_BIC_BMI <- function(data, maxclass = 10){
  aicVec <- c(rep(0,maxclass-1))
  bicVec <- c(rep(0,maxclass-1))
  
  set.seed(50)
  for (i in 2:maxclass){
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                    nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
                    iter.max = 15000)
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC_BMI(lcaDat, maxclass = 8) #now 5 classes is best

#print out what the classes are
class5.bmi <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, 
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
summary(class5.bmi)


#now see if there is class invariance by gender with BMI
set.seed(87)
class5.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Male),
                  constrain.rhos = T, constrain.gammas = T,
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class5.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 5, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Male),
                  constrain.rhos = F, constrain.gammas = T,
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class5.con, class5.unc) #yes there is invariance: ChiSq = 2751.409, df = 65, p < 0.001

#############################################################################################

#first write a function to test LCA classes and get AIC/BIC to select with Male option
get_AIC_BIC_male <- function(data, Male, maxclass = 10){
  aicVec <- c(rep(0,maxclass-1))
  bicVec <- c(rep(0,maxclass-1))
  data <- data[data$Male == Male,]
  set.seed(50)
  for (i in 2:maxclass){
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                    nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
                    iter.max = 15000)
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}


#first write a function to test LCA classes and get AIC/BIC to select with Male option
get_AIC_BIC_BMI_male <- function(data, male, maxclass = 10){
  aicVec <- c(rep(0,maxclass-1))
  bicVec <- c(rep(0,maxclass-1))
  
  data <- data[data$Male == male,]
  set.seed(50)
  for (i in 2:maxclass){
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                    nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
                    iter.max = 15000)
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC_male(lcaDat, Male = 1, maxclass = 8) #3 classes for females
get_AIC_BIC_male(lcaDat, Male = 2, maxclass = 8) #3 classes for males

#do same but for BMI included
get_AIC_BIC_BMI_male(lcaDat, male = 1, maxclass = 8) #3 classes for females
get_AIC_BIC_BMI_male(lcaDat, male = 2, maxclass = 8) #4 classes for males

#go back to included sampling strategy and see the classes
set.seed(100)
male3 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, subpop = (Male == 2),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(male3)

set.seed(100)
female3 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, subpop = (Male == 1),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(female3)

set.seed(100)
maleBMI4 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 4, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, subpop = (Male == 2),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(maleBMI4)

set.seed(100)
femaleBMI3 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, subpop = (Male == 1),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(femaleBMI3)
########################################################################
#now test class invariance by Race
wm <- lcaDat[lcaDat$Male == 2 & lcaDat$Race == 1,]
wf <- lcaDat[lcaDat$Male == 1 & lcaDat$Race == 1,]
bm <- lcaDat[lcaDat$Male == 2 & lcaDat$Race == 2,]
bf <- lcaDat[lcaDat$Male == 1 & lcaDat$Race == 2,]
hm <- lcaDat[lcaDat$Male == 2 & lcaDat$Race == 3,]
hf <- lcaDat[lcaDat$Male == 1 & lcaDat$Race == 3,]
om <- lcaDat[lcaDat$Male == 2 & lcaDat$Race == 4,]
of <- lcaDat[lcaDat$Male == 1 & lcaDat$Race == 4,]

#Females first: 3 classes with BMI
set.seed(87)
class3bmif.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = T, constrain.gammas = T, subpop = (Male == 1),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class3bmif.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = F, constrain.gammas = T, subpop = (Male == 1),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class3bmif.con, class3bmif.unc) #yes there is invariance: ChiSq = 884.3144, df = 117, p < 0.001

#the model without BMI included
set.seed(87)
class3f.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = T, constrain.gammas = T, subpop = (Male == 1),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class3f.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = F, constrain.gammas = T, subpop = (Male == 1),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class3f.con, class3f.unc) #yes there is invariance: ChiSq = 599.666, df = 72, p < 0.001

####################################################################################

#Males: 3 classes with BMI
set.seed(87)
class4bmim.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 4, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = T, constrain.gammas = T, subpop = (Male == 2),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class4bmim.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 4, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = F, constrain.gammas = T, subpop = (Male == 2),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class4bmim.con, class4bmim.unc) #yes there is invariance: ChiSq = 1027.175, df = 156, p < 0.001

#the model without BMI included
set.seed(87)
class3m.con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = T, constrain.gammas = T, subpop = (Male == 2),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

set.seed(99)
class3m.unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, group = factor(Race),
                  constrain.rhos = F, constrain.gammas = T, subpop = (Male == 2),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)
compare.fit(class3m.con, class3m.unc) #yes there is invariance: ChiSq = 495.2879, df = 72, p < 0.001

#####################################################################
#now need to choose optimal class number for each race/gender subgroup
get_AIC_BIC(wm, maxclass = 8) #3 classes
get_AIC_BIC(wf, maxclass = 8) #3 classes
get_AIC_BIC(bm, maxclass = 8) #3 classes
get_AIC_BIC(bf, maxclass = 8) #3 classes
get_AIC_BIC(hm, maxclass = 8) #3 classes
get_AIC_BIC(hf, maxclass = 8) #3 classes
get_AIC_BIC(om, maxclass = 8) #3 classes
get_AIC_BIC(of, maxclass = 8) #3 classes

get_AIC_BIC_BMI(wm, maxclass = 8) #3 classes
get_AIC_BIC_BMI(wf, maxclass = 8) #4 classes
get_AIC_BIC_BMI(bm, maxclass = 8) #3 classes
get_AIC_BIC_BMI(bf, maxclass = 8) #3 classes
get_AIC_BIC_BMI(hm, maxclass = 8) #3 classes
get_AIC_BIC_BMI(hf, maxclass = 8) #3 classes
get_AIC_BIC_BMI(om, maxclass = 8) #3 classes
get_AIC_BIC_BMI(of, maxclass = 8) #3 classes

#print out the appropriate classifications for each group
maleAndRaceSummary <- function(data, Race, Male, classNum){

	return(lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000,
                  subpop = (Male == Male & Race == Race),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA))
}

wf3 <- maleAndRaceSummary(lcaDat, 1, 1, 3)
wm3 <- maleAndRaceSummary(lcaDat, 1, 2, 3)
bf3 <- maleAndRaceSummary(lcaDat, 2, 1, 3)
bm3 <- maleAndRaceSummary(lcaDat, 2, 2, 3)
hf3 <- maleAndRaceSummary(lcaDat, 3, 1, 3)
hm3 <- maleAndRaceSummary(lcaDat, 3, 2, 3)
of3 <- maleAndRaceSummary(lcaDat, 3, 1, 3)
om3 <- maleAndRaceSummary(lcaDat, 3, 2, 3)

summary(wf3)
summary(wm3)
summary(bf3)
summary(bm3)
summary(hf3)
summary(hm3)
summary(of3)
summary(om3)
##########################################
#repeat for BMI

maleAndRaceSummaryB <- function(data, Race, Male, classNum){

	return(lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000,
                  subpop = (Male == Male & Race == Race),
                  weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA))
}

wf4b <- maleAndRaceSummary(lcaDat, 1, 1, 4)
wm3b <- maleAndRaceSummary(lcaDat, 1, 2, 3)
bf3b <- maleAndRaceSummary(lcaDat, 2, 1, 3)
bm3b <- maleAndRaceSummary(lcaDat, 2, 2, 3)
hf3b <- maleAndRaceSummary(lcaDat, 3, 1, 3)
hm3b <- maleAndRaceSummary(lcaDat, 3, 2, 3)
of3b <- maleAndRaceSummary(lcaDat, 3, 1, 3)
om3b <- maleAndRaceSummary(lcaDat, 3, 2, 3)

summary(wf4b)
summary(wm3b)
summary(bf3b)
summary(bm3b)
summary(hf3b)
summary(hm3b)
summary(of3b)
summary(om3b)

######################################################################
xtabs(~Race+FSDHH+Male, data = lcaDat)
#now need to see if also invariance by food security status
#will have to do individually for each Race/Sex pairing
fsInvariance <- function(data, Race, Male, classNum){
	set.seed(25)

	dat <- data[data$Race == Race & data$Male == Male & !is.na(data$FSDHH),]
	unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = F, constrain.gammas = T,group = factor(FSDHH))
	set.seed(25)
	con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = T, constrain.gammas = T,group = factor(FSDHH))

	return(compare.fit(unc,con))

}

#test the invariance
fsInvariance(lcaDat, 1, 1, 4) #white female: yes chisq = 130.8165, df = 96, p = 0.0105
fsInvariance(lcaDat, 1, 2, 3) #white male: NO
fsInvariance(lcaDat, 2, 1, 3) #black female: yes chisq = 156.5142, df = 72, p < 0.001
fsInvariance(lcaDat, 2, 2, 3) #black male: NO
fsInvariance(lcaDat, 3, 1, 3) #hispanic female: NO
fsInvariance(lcaDat, 3, 2, 3) #hispanic male: NO
fsInvariance(lcaDat, 4, 1, 3) #other female: NO
fsInvariance(lcaDat, 4, 2, 3) #other male: NO

#Same but including BMI
fsInvarianceBMI <- function(data, Race, Male, classNum){
	set.seed(25)

	dat <- data[data$Race == Race & data$Male == Male & !is.na(data$FSDHH),]
	unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = F, constrain.gammas = T,group = factor(FSDHH))
	set.seed(25)
	con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = T, constrain.gammas = T,group = factor(FSDHH))

	return(compare.fit(unc,con))

}

#test the invariance
fsInvarianceBMI(lcaDat, 1, 1, 4) #white female: yes chisq = 224.0589, df = 156, p < 0.001
fsInvarianceBMI(lcaDat, 1, 2, 3) #white male: NO
fsInvarianceBMI(lcaDat, 2, 1, 3) #black female: NO
fsInvarianceBMI(lcaDat, 2, 2, 3) #black male: NO
fsInvarianceBMI(lcaDat, 3, 1, 3) #hispanic female: NO
fsInvarianceBMI(lcaDat, 3, 2, 3) #hispanic male: NO
fsInvarianceBMI(lcaDat, 4, 1, 3) #other female: NO
fsInvarianceBMI(lcaDat, 4, 2, 3) #other male: NO

#in general, FS doesnt seem measurement invariant by race*sex
#does class prev differ by FS?

fsInvariancegam <- function(data, Race, Male, classNum){
	set.seed(25)

	dat <- data[data$Race == Race & data$Male == Male & !is.na(data$FSDHH),]
	unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = T, constrain.gammas = F,group = factor(FSDHH))
	set.seed(25)
	con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = T, constrain.gammas = T,group = factor(FSDHH))

	return(compare.fit(unc,con))

}
#test the invariance
fsInvariancegam(lcaDat, 1, 1, 4) #white female: yes chisq = 53.10903, df = 9, p < 0.001
fsInvariancegam(lcaDat, 1, 2, 3) #white male:NO
fsInvariancegam(lcaDat, 2, 1, 3) #black female: YES chisq = 14.14369, df = 6, p = 0.02807166
fsInvariancegam(lcaDat, 2, 2, 3) #black male: YES chisq = 22.41037, df = 6, p = 0.001020034
fsInvariancegam(lcaDat, 3, 1, 3) #hispanic female: NO, chisq = 11.8716, df = 6, p = 0.0648953
fsInvariancegam(lcaDat, 3, 2, 3) #hispanic male: YES, chisq = 16.3488, df = 6, p = 0.01199934
fsInvariancegam(lcaDat, 4, 1, 3) #other female: NO
fsInvariancegam(lcaDat, 4, 2, 3) #other male: NO


#Same but including BMI
fsInvarianceBMIgam <- function(data, Race, Male, classNum){
	set.seed(25)

	dat <- data[data$Race == Race & data$Male == Male & !is.na(data$FSDHH),]
	unc <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = T, constrain.gammas = F,group = factor(FSDHH))
	set.seed(25)
	con <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = classNum, data = dat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 40000,
			constrain.rhos = T, constrain.gammas = T,group = factor(FSDHH))

	return(compare.fit(unc,con))

}
#test the invariance
fsInvarianceBMIgam(lcaDat, 1, 1, 4) #white female: yes chisq = 201.2054, df = 9, p < 0.001
fsInvarianceBMIgam(lcaDat, 1, 2, 3) #white male: YES chisq = 50.36528, df = 6, p < 0.001
fsInvarianceBMIgam(lcaDat, 2, 1, 3) #black female: NO
fsInvarianceBMIgam(lcaDat, 2, 2, 3) #black male: YES chisq = 23.55243, df = 6, p < 0.001
fsInvarianceBMIgam(lcaDat, 3, 1, 3) #hispanic female: NO
fsInvarianceBMIgam(lcaDat, 3, 2, 3) #hispanic male: NO, chisq = 11.77494, df = 6, p = 0.06718
fsInvarianceBMIgam(lcaDat, 4, 1, 3) #other female: NO
fsInvarianceBMIgam(lcaDat, 4, 2, 3) #other male: NO
