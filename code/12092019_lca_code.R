#######################################################
# Author: Sarah Van Alsten                            #
# Date Created: December 09, 2019                     #
# Purpose: Code to run Latent Class Causal Analysis   #
# Data Used: NHANES 07, 09, 11                        #
# Packages Used: lcca                                 #
# Last Update: December 09, 2019                      #
#######################################################
#note this should be run in R 3.0.0!!!

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
              iter.max = 15000)
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC(lcaDat, maxclass = 8) #4 classes

#now run with survey weights to see what those 3 classes are
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
###############################################################################################
#now run separately by gender in both cases to see what the optimal number classes are

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

get_AIC_BIC_male(lcaDat, Male = 1, maxclass = 8) #3 classes for males
get_AIC_BIC_male(lcaDat, Male = 2, maxclass = 8) #3 classes for females

#do same but for BMI included
get_AIC_BIC_BMI_male(lcaDat, male = 1, maxclass = 8) #3 classes for females
get_AIC_BIC_BMI_male(lcaDat, male = 2, maxclass = 8) #4 classes for males

#go back to included sampling strategy and see the classes
set.seed(100)
male3 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
             nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
             flatten.gammas =1, iter.max = 20000, subpop = (Male == 2),
             constrain.rhos = T, constrain.gammas = T,
             weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(male3)

set.seed(100)
female3 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh)~1,
               nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
               flatten.gammas =1, iter.max = 20000, subpop = (Male == 1),
               constrain.rhos = T, constrain.gammas = T,
               weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(female3)

set.seed(100)
maleBMI4 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                nclass = 4, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                flatten.gammas =1, iter.max = 20000, subpop = (Male == 2),
                constrain.rhos = T, constrain.gammas = T,
                weights = WTMEC6YR, clusters = SDMVPSU, strata = SDMVSTRA)

summary(maleBMI4)

set.seed(100)
femaleBMI3 <- lca(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                  nclass = 3, data = lcaDat, tol = 1e-06, flatten.rhos = 1, 
                  flatten.gammas =1, iter.max = 20000, subpop = (Male == 1),
                  constrain.rhos = T, constrain.gammas = T,
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






