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


