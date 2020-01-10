
# Latent class ------------------------------------------------------------
library(poLCA)

#build latent class analysis
summary(lcaSub)

get_AIC_BIC <- function(maxclass = 10, data = lcaSub){
  aicVec <- c(rep(0, maxclass))
  bicVec <- aicVec
  for (i in 2:maxclass){
    lc <- poLCA(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
                data = data,
                nclass = i,
                maxiter = 20000)
    aicVec[i] <- lc$aic
    bicVec[i] <- lc$bic
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC(8) # 4 class solution is shown as the best one

#see how classes differ by gender
#Female: 4 classes
get_AIC_BIC(data = lcaSub[lcaSub$Male == 1,], maxclass = 7)
#Male : 4 classes
get_AIC_BIC(data = lcaSub[lcaSub$Male == 2,], maxclass = 7)

lc4f <- poLCA(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
            data = lcaSub[lcaSub$Male == 1,],
            nclass = 4,
            maxiter = 20000)

lc4m <- poLCA(cbind(doingAbtWt, ConsiderWt, LikeToWeigh, BMIcat)~1,
              data = lcaSub[lcaSub$Male == 2,],
              nclass = 4,
              maxiter = 20000)


#output everyone's predicted class
sum(table(lc4f$predclass))

fem <- lcaSub[lcaSub$Male == 1,]
male <- lcaSub[lcaSub$Male == 2,]

#exclude missings bc they have no predicted class
fem.comp <- fem[!is.na(lcaSub[lcaSub$Male == 1,"BMIcat"]) &
              !is.na(lcaSub[lcaSub$Male == 1,"LikeToWeigh"]) &
              !is.na(lcaSub[lcaSub$Male == 1,"ConsiderWt"]) &
              !is.na(lcaSub[lcaSub$Male == 1,"doingAbtWt"]),]

male.comp <- male[!is.na(lcaSub[lcaSub$Male == 2,"BMIcat"]) &
                  !is.na(lcaSub[lcaSub$Male == 2,"LikeToWeigh"]) &
                  !is.na(lcaSub[lcaSub$Male == 2,"ConsiderWt"]) &
                  !is.na(lcaSub[lcaSub$Male == 2,"doingAbtWt"]),]

fem.comp$predClass <- lc4f$predclass
male.comp$predClass <- lc4m$predclass

fem.comp$depressionBinary <- fem.comp$depressionBinary -1 
fem.dep.mod <- glm(depressionBinary ~ factor(predClass) + factor(Race) + factor(fsWithHunger) +
                     factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                     factor(age4),
                   data = fem.comp, family = binomial(link = "logit"))
summary(fem.dep.mod)

library(odds.n.ends)
odds.n.ends(fem.dep.mod) #CLASS 1: REF = WEIGH SAME;
#2 = WEIGH MORE, 3=NORM TO MOD BMI, WANT TO LOSE, 4 = HIGH BMI, WANT TO LOSE


#4= weigh same, 1 = weigh less, moderate bmi, 2 = weigh more, 3 = weigh less high bmi
male.comp$depressionBinary <- male.comp$depressionBinary -1 
male.dep.mod <- glm(depressionBinary ~ relevel(factor(predClass), ref = '4') + factor(Race) + factor(fsWithHunger) +
                     factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                     factor(age4),
                   data = male.comp, family = binomial(link = "logit"))
summary(male.dep.mod)
odds.n.ends(male.dep.mod)

###########################################################################################
#do same, taking into account the complex sampling of NHANES
femClus <-survey::svydesign(id=~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights=~WTMEC6YR, 
                           nest = TRUE, 
                           data=fem.comp)

maleClus <-survey::svydesign(id=~SDMVPSU, 
                            strata = ~SDMVSTRA, 
                            weights=~WTMEC6YR, 
                            nest = TRUE, 
                            data=male.comp)

maleSvy.dep <- survey::svyglm(depressionBinary ~ relevel(factor(predClass), ref = '4') + 
                              factor(Race) + factor(fsWithHunger) +
                              factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                              factor(age4), design = maleClus, family = binomial(link = "logit"))
summary(maleSvy.dep)


femSvy.dep <- survey::svyglm(depressionBinary ~ factor(predClass) + 
                                factor(Race) + factor(fsWithHunger) +
                                factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                                factor(age4), design = femClus, family = binomial(link = "logit"))
summary(femSvy.dep)
################################################################################
#now, see if classes are better at predicting than the variables that make it up
femSvy.dep2 <- survey::svyglm(depressionBinary ~ factor(doingAbtWt) + factor(LikeToWeigh)+ factor(ConsiderWt)+
                              factor(BMIcat)+
                               factor(Race) + factor(fsWithHunger) +
                               factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                               factor(age4), design = femClus, family = binomial(link = "logit"))
summary(femSvy.dep2)

#########################################################################################
fem.dep2 <- glm(depressionBinary ~ factor(doingAbtWt) + factor(LikeToWeigh)+ factor(ConsiderWt)+
                                factor(BMIcat)+
                                factor(Race) + factor(fsWithHunger) +
                                factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                                factor(age4), data = femClus$variables, family = binomial(link = "logit"))

odds.n.ends(fem.dep2) #5.5% s3ns and 99.4% spec... latent class was 2.75% sens and 99.4% spec

male.dep2 <- glm(depressionBinary ~ factor(doingAbtWt) + factor(LikeToWeigh)+ factor(ConsiderWt)+
                  factor(BMIcat)+
                  factor(Race) + factor(fsWithHunger) +
                  factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                  factor(age4), data = maleClus$variables, family = binomial(link = "logit"))
odds.n.ends(male.dep2) #3.76% sens and 99.8% spec... latent class was 2.19% sens and 99.9% spec


#together, this suggests that the latent classes aren't really all that helpful: at least in
#comparison to the variables that make them up, in predicting depression

##########################################################################
#What about predicting marijuana use?
fem.mj2 <- MASS::polr(factor(mjuse4) ~ factor(doingAbtWt) + factor(LikeToWeigh)+ factor(ConsiderWt)+
                  factor(BMIcat)+
                  factor(Race) + factor(fsWithHunger) +
                  factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                  factor(age4), data = femClus$variables)

odds.n.ends(fem.dep2) #5.5% s3ns and 99.4% spec... latent class was 2.75% sens and 99.4% spec

male.dep2 <- glm(depressionBinary ~ factor(doingAbtWt) + factor(LikeToWeigh)+ factor(ConsiderWt)+
                   factor(BMIcat)+
                   factor(Race) + factor(fsWithHunger) +
                   factor(Income) + factor(edu) + factor(orient) + factor(maritalstatus) +
                   factor(age4), data = maleClus$variables, family = binomial(link = "logit"))
odds.n.ends(male.dep2) #3.76% sens and 99.8% spec... latent class was 2.19% sens and 99.9% spec

