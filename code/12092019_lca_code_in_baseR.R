#######################################################
# Author: Sarah Van Alsten                            #
# Date Created: December 09, 2019                     #
# Purpose: Code to run Latent Class Causal Analysis   #
# Data Used: NHANES 07, 09, 11                        #
# Packages Used: lcca                                 #
# Last Update: December 09, 2019                      #
#######################################################

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
  
  set.seed(50)
  for (i in 2:maxclass){
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, likeToWeigh)~1,
                    nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
                    iter.max = 15000, subpop = (Male == Male))
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}


#first write a function to test LCA classes and get AIC/BIC to select with Male option
get_AIC_BIC_BMI_male <- function(data, Male, maxclass = 10){
  aicVec <- c(rep(0,maxclass-1))
  bicVec <- c(rep(0,maxclass-1))
  
  set.seed(50)
  for (i in 2:maxclass){
    lc <- lcca::lca(cbind(doingAbtWt, ConsiderWt, likeToWeigh, BMIcat)~1,
                    nclass = i, data = data, tol = 1e-06, flatten.rhos = 1, flatten.gammas =1,
                    iter.max = 15000, subpop = (Male == Male))
    
    aicVec[i] <-  lc$AIC
    bicVec[i] <-  lc$BIC
  }
  return(cbind(aicVec, bicVec))
}




