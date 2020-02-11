## Sarah Van Alsten
## Feburay 10, 2020
## Food Insecurity Analyses in NHANES: with weight perception
## Packages: tidyverse, survey, nnet

#open libraries
library(RNHANES)
library(tidyverse)
library(survey)
library(nnet) #for multinomial logit
library(broom)
library(multcomp) #for posthoc testing


# Data Management ---------------------------------------------------------

dat <- read_csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\01282020nhanes.csv")

#relevel factors for analyses
dat$consid <- relevel(factor(dat$ConsiderWt,
                             labels = c("too thin", "about right", "too big")),
                      ref = "about right")

dat$likeTo = relevel(factor(dat$LikeToWeigh, 
                            labels = c("like to weigh less", "like to weigh same", "like to weigh more")),
                     ref = "like to weigh same")

dat$doingWt = relevel(factor(dat$doingAbtWt,
                             labels = c("lost weight: intentional",
                                        "lost weight: unintented",
                                        "tried to lose weight (but didnt)",
                                        "tried to not gain",
                                        "doing nothing")),
                      ref = "doing nothing")


#calculate Self report bmi and BMI category
dat <- dat %>% 
  mutate(SRbmi = ifelse(is.na(WHD010) | is.na(WHD020), NA,
                        ifelse(WHD010 > 84 | WHD020 > 1000, NA,
                               (WHD020/2.2046)/ (WHD010*0.0254)^2)))

dat <- dat %>%
  mutate(SRbmicat = ifelse(SRbmi < 18.5, 1,
                           ifelse(SRbmi < 25, 2,
                                  ifelse(SRbmi < 30, 3,
                                         ifelse(SRbmi < 35, 4,
                                                ifelse(SRbmi < 40, 5, 6))))))

dat$fsWithHunger <- as.factor(dat$fsWithHunger)


#set up survey design
svy <- svydesign(id = ~SDMVPSU.x, 
                 strata = ~SDMVSTRA.x, 
                 weights = ~WTMEC6YR, 
                 nest = TRUE, 
                 data = dat)



#for subpop (male and female)
male.svy <- subset(svy, Male == 1)
fema.svy <- subset(svy, Male == 0)

male <- dat[dat$Male == 1,]
fema <- dat[dat$Male == 0,]
# Set Up Empty Frames for Results -----------------------------------------


#data frames to hold the analytic results
female.fs.with.hunger <- data.frame(var = c(rep(c("consider too thin",
                                                  "consider too big",
                                                  "like to weigh less",
                                                  "like to weigh more",
                                                  "lost weight intentional",
                                                  "lost weight unintentional",
                                                  "tried to lose",
                                                  "tried to not gain"), 3)),
                                    type = c(rep("unadj", 8),
                                             rep("bmi.adj", 8),
                                             rep("full.adj", 8)),
                                    est = c(rep("", 24)))


#initialize remaining needed data frames as exactly the same as one just created
female.fs.with.hunger$est <- as.character(female.fs.with.hunger$est)
male.fs.with.hunger <- female.fs.with.hunger
female.fs.no.hunger <- female.fs.with.hunger
male.fs.no.hunger <- female.fs.with.hunger

#function that will take the model and desired level to get OR + 95% CI
getEst <- function(mod, term1, y.level1){
  v <- 
    broom::tidy(mod, conf.int = T, exponentiate = T) %>%
    dplyr::select(y.level, term, estimate, conf.low, conf.high) %>%
    filter(term == term1 & y.level == y.level1) %>%
    as.vector() %>%
    paste0(round(as.numeric(.[3]),2), " (",
           round(as.numeric(.[4]),2), " - ",
           round(as.numeric(.[5]),2), ")") %>%
    str_remove(pattern = y.level1)
  
  return(v[1])
  
}

# Analysis ----------------------------------------------------------------

#like to weigh
fs.ltw.m <- multinom(likeTo ~ fsWithHunger, data = male)
#place results into data frames where needed
male.fs.no.hunger[3, "est"] <- getEst(fs.ltw.m, "fsWithHunger1", "like to weigh less")
male.fs.with.hunger[3, "est"] <- getEst(fs.ltw.m, "fsWithHunger2", "like to weigh less")

male.fs.no.hunger[4, "est"] <- getEst(fs.ltw.m, "fsWithHunger1", "like to weigh more")
male.fs.with.hunger[4, "est"] <- getEst(fs.ltw.m, "fsWithHunger2", "like to weigh more")


fs.ltw.f <- multinom(likeTo ~ fsWithHunger, data = fema)
#place results into data frames where needed
female.fs.no.hunger[3, "est"] <- getEst(fs.ltw.f, "fsWithHunger1", "like to weigh less")
female.fs.with.hunger[3, "est"] <- getEst(fs.ltw.f, "fsWithHunger2", "like to weigh less")

female.fs.no.hunger[4, "est"] <- getEst(fs.ltw.f, "fsWithHunger1", "like to weigh more")
female.fs.with.hunger[4, "est"] <- getEst(fs.ltw.f, "fsWithHunger2", "like to weigh more")
############################################################################
#consider weight

fs.cow.m <- multinom(consid ~ fsWithHunger, data = male)
male.fs.no.hunger[1, "est"] <- getEst(fs.cow.m, "fsWithHunger1", "too thin")
male.fs.with.hunger[1, "est"] <- getEst(fs.cow.m, "fsWithHunger2", "too thin")
male.fs.with.hunger[2, "est"] <- getEst(fs.cow.m, "fsWithHunger2", "too big")
male.fs.no.hunger[2, "est"] <- getEst(fs.cow.m, "fsWithHunger1", "too big")

fs.cow.f <- multinom(consid ~ fsWithHunger, data = fema)
female.fs.no.hunger[1, "est"] <- getEst(fs.cow.f, "fsWithHunger1", "too thin")
female.fs.with.hunger[1, "est"] <- getEst(fs.cow.f, "fsWithHunger2", "too thin")
female.fs.no.hunger[2, "est"] <- getEst(fs.cow.f, "fsWithHunger1", "too big")
female.fs.with.hunger[2, "est"] <- getEst(fs.cow.f, "fsWithHunger2", "too big")
#######################################################################################

#doing about weight
fs.daw.m <- multinom(doingWt ~ fsWithHunger, data = male)
tidy(fs.daw.m)
male.fs.no.hunger[5, "est"] <- getEst(fs.daw.m, "fsWithHunger1", "lost weight: intentional")
male.fs.with.hunger[5, "est"] <- getEst(fs.daw.m, "fsWithHunger2", "lost weight: intentional")
male.fs.with.hunger[6, "est"] <- getEst(fs.daw.m, "fsWithHunger2", "lost weight: unintented")
male.fs.no.hunger[6, "est"] <- getEst(fs.daw.m, "fsWithHunger1", "lost weight: unintented")
male.fs.with.hunger[7, "est"] <- getEst(fs.daw.m, "fsWithHunger2", "tried to lose weight (but didnt)")
male.fs.no.hunger[7, "est"] <- getEst(fs.daw.m, "fsWithHunger1", "tried to lose weight (but didnt)")
male.fs.with.hunger[8, "est"] <- getEst(fs.daw.m, "fsWithHunger2", "tried to not gain")
male.fs.no.hunger[8, "est"] <- getEst(fs.daw.m, "fsWithHunger1", "tried to not gain")


fs.daw.f <- multinom(doingWt ~ fsWithHunger, data = fema)
female.fs.no.hunger[5, "est"] <- getEst(fs.daw.f, "fsWithHunger1", "lost weight: intentional")
female.fs.with.hunger[5, "est"] <- getEst(fs.daw.f, "fsWithHunger2", "lost weight: intentional")
female.fs.with.hunger[6, "est"] <- getEst(fs.daw.f, "fsWithHunger2", "lost weight: unintented")
female.fs.no.hunger[6, "est"] <- getEst(fs.daw.f, "fsWithHunger1", "lost weight: unintented")
female.fs.with.hunger[7, "est"] <- getEst(fs.daw.f, "fsWithHunger2", "tried to lose weight (but didnt)")
female.fs.no.hunger[7, "est"] <- getEst(fs.daw.f, "fsWithHunger1", "tried to lose weight (but didnt)")
female.fs.with.hunger[8, "est"] <- getEst(fs.daw.f, "fsWithHunger2", "tried to not gain")
female.fs.no.hunger[8, "est"] <- getEst(fs.daw.f, "fsWithHunger1", "tried to not gain")



# BMI adjusted model ------------------------------------------------------

#like to weigh models

fs.ltw.bmi.m <- multinom(likeTo ~ fsWithHunger + factor(BMIcat), data = male)
male.fs.no.hunger[11, "est"] <- getEst(fs.ltw.bmi.m, "fsWithHunger1", "like to weigh less")
male.fs.with.hunger[11, "est"] <- getEst(fs.ltw.bmi.m, "fsWithHunger2", "like to weigh less")
male.fs.no.hunger[12, "est"] <- getEst(fs.ltw.bmi.m, "fsWithHunger1", "like to weigh more")
male.fs.with.hunger[12, "est"] <- getEst(fs.ltw.bmi.m, "fsWithHunger2", "like to weigh more")

fs.ltw.bmi.f <- multinom(likeTo ~ fsWithHunger+ factor(BMIcat), data = fema)
female.fs.no.hunger[11, "est"] <- getEst(fs.ltw.bmi.f, "fsWithHunger1", "like to weigh less")
female.fs.with.hunger[11, "est"] <- getEst(fs.ltw.bmi.f, "fsWithHunger2", "like to weigh less")
female.fs.no.hunger[12, "est"] <- getEst(fs.ltw.bmi.f, "fsWithHunger1", "like to weigh more")
female.fs.with.hunger[12, "est"] <- getEst(fs.ltw.bmi.f, "fsWithHunger2", "like to weigh more")
#########################################################################################

#consider weight models
fs.cow.bmi.m <- multinom(consid ~ fsWithHunger + factor(BMIcat), data = male)
fs.cow.bmi.f <- multinom(consid ~ fsWithHunger + factor(BMIcat), data = fema)

#fill in table male
male.fs.no.hunger[9, "est"] <- getEst(fs.cow.bmi.m, "fsWithHunger1", "too thin")
male.fs.with.hunger[9, "est"] <- getEst(fs.cow.bmi.m, "fsWithHunger2", "too thin")
male.fs.no.hunger[10, "est"] <- getEst(fs.cow.bmi.m, "fsWithHunger1", "too big")
male.fs.with.hunger[10, "est"] <- getEst(fs.cow.bmi.m, "fsWithHunger2", "too big")

#fill in table female
female.fs.no.hunger[9, "est"] <- getEst(fs.cow.bmi.f, "fsWithHunger1", "too thin")
female.fs.with.hunger[9, "est"] <- getEst(fs.cow.bmi.f, "fsWithHunger2", "too thin")
female.fs.no.hunger[10, "est"] <- getEst(fs.cow.bmi.f, "fsWithHunger1", "too big")
female.fs.with.hunger[10, "est"] <- getEst(fs.cow.bmi.f, "fsWithHunger2", "too big")


#################################################################################
#doing about weight adjusted for BMI
fs.daw.bmi.m <- multinom(doingWt ~ fsWithHunger + factor(BMIcat), data = male)
fs.daw.bmi.f <- multinom(doingWt ~ fsWithHunger + factor(BMIcat), data = fema)


male.fs.no.hunger[13, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger1", "lost weight: intentional")
male.fs.with.hunger[13, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger2", "lost weight: intentional")
male.fs.with.hunger[14, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger2", "lost weight: unintented")
male.fs.no.hunger[14, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger1", "lost weight: unintented")
male.fs.with.hunger[15, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger2", "tried to lose weight (but didnt)")
male.fs.no.hunger[15, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger1", "tried to lose weight (but didnt)")
male.fs.with.hunger[16, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger2", "tried to not gain")
male.fs.no.hunger[16, "est"] <- getEst(fs.daw.bmi.m, "fsWithHunger1", "tried to not gain")

female.fs.no.hunger[13, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger1", "lost weight: intentional")
female.fs.with.hunger[13, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger2", "lost weight: intentional")
female.fs.with.hunger[14, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger2", "lost weight: unintented")
female.fs.no.hunger[14, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger1", "lost weight: unintented")
female.fs.with.hunger[15, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger2", "tried to lose weight (but didnt)")
female.fs.no.hunger[15, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger1", "tried to lose weight (but didnt)")
female.fs.with.hunger[16, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger2", "tried to not gain")
female.fs.no.hunger[16, "est"] <- getEst(fs.daw.bmi.f, "fsWithHunger1", "tried to not gain")
########################################################################################


# Fully Adjusted Models ---------------------------------------------------

#like to weigh
fs.ltw.adj.m <- multinom(likeTo ~ fsWithHunger + factor(BMIcat) + factor(Income) +
                           factor(edu) + factor(Race) + factor(age4), data = male)

fs.ltw.adj.f <- multinom(likeTo ~ fsWithHunger+ factor(BMIcat) + factor(Income) +
                           factor(edu) + factor(Race) + factor(age4), data = fema)

male.fs.no.hunger[19, "est"] <- getEst(fs.ltw.adj.m, "fsWithHunger1", "like to weigh less")
male.fs.with.hunger[19, "est"] <- getEst(fs.ltw.adj.m, "fsWithHunger2", "like to weigh less")
male.fs.no.hunger[20, "est"] <- getEst(fs.ltw.adj.m, "fsWithHunger1", "like to weigh more")
male.fs.with.hunger[20, "est"] <- getEst(fs.ltw.adj.m, "fsWithHunger2", "like to weigh more")

female.fs.no.hunger[19, "est"] <- getEst(fs.ltw.adj.f, "fsWithHunger1", "like to weigh less")
female.fs.with.hunger[19, "est"] <- getEst(fs.ltw.adj.f, "fsWithHunger2", "like to weigh less")
female.fs.no.hunger[20, "est"] <- getEst(fs.ltw.adj.f, "fsWithHunger1", "like to weigh more")
female.fs.with.hunger[20, "est"] <- getEst(fs.ltw.adj.f, "fsWithHunger2", "like to weigh more")
###########################################################################################

#consider weight
fs.cow.adj.m <- multinom(consid ~ fsWithHunger + factor(BMIcat) + factor(Income) +
                           factor(edu) + factor(Race) + factor(age4), data = male)

fs.cow.adj.f <- multinom(consid ~ fsWithHunger+ factor(BMIcat) + factor(Income) +
                           factor(edu) + factor(Race) + factor(age4), data = fema)

male.fs.no.hunger[17, "est"] <- getEst(fs.cow.adj.m, "fsWithHunger1", "too thin")
male.fs.with.hunger[17, "est"] <- getEst(fs.cow.adj.m, "fsWithHunger2", "too thin")
male.fs.no.hunger[18, "est"] <- getEst(fs.cow.adj.m, "fsWithHunger1", "too big")
male.fs.with.hunger[18, "est"] <- getEst(fs.cow.adj.m, "fsWithHunger2", "too big")

female.fs.no.hunger[17, "est"] <- getEst(fs.cow.adj.f, "fsWithHunger1", "too thin")
female.fs.with.hunger[17, "est"] <- getEst(fs.cow.adj.f, "fsWithHunger2", "too thin")
female.fs.no.hunger[18, "est"] <- getEst(fs.cow.adj.f, "fsWithHunger1", "too big")
female.fs.with.hunger[18, "est"] <- getEst(fs.cow.adj.f, "fsWithHunger2", "too big")
###################################################################################################
#doing about weight

fs.daw.adj.m <- multinom(doingWt ~ fsWithHunger + factor(BMIcat) + factor(Income) +
                           factor(edu) + factor(Race) + factor(age4), data = male)

fs.daw.adj.f <- multinom(doingWt ~ fsWithHunger+ factor(BMIcat) + factor(Income) +
                           factor(edu) + factor(Race) + factor(age4), data = fema)

male.fs.no.hunger[21, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger1", "lost weight: intentional")
male.fs.with.hunger[21, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger2", "lost weight: intentional")
male.fs.with.hunger[22, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger2", "lost weight: unintented")
male.fs.no.hunger[22, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger1", "lost weight: unintented")
male.fs.with.hunger[23, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger2", "tried to lose weight (but didnt)")
male.fs.no.hunger[23, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger1", "tried to lose weight (but didnt)")
male.fs.with.hunger[24, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger2", "tried to not gain")
male.fs.no.hunger[24, "est"] <- getEst(fs.daw.adj.m, "fsWithHunger1", "tried to not gain")

female.fs.no.hunger[21, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger1", "lost weight: intentional")
female.fs.with.hunger[21, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger2", "lost weight: intentional")
female.fs.with.hunger[22, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger2", "lost weight: unintented")
female.fs.no.hunger[22, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger1", "lost weight: unintented")
female.fs.with.hunger[23, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger2", "tried to lose weight (but didnt)")
female.fs.no.hunger[23, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger1", "tried to lose weight (but didnt)")
female.fs.with.hunger[24, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger2", "tried to not gain")
female.fs.no.hunger[24, "est"] <- getEst(fs.daw.adj.f, "fsWithHunger1", "tried to not gain")



# Print Results -----------------------------------------------------------

female.fs.no.hunger$est[1:8]
male.fs.no.hunger$est[1:8]
female.fs.with.hunger$est[1:8]
male.fs.with.hunger$est[1:8]

female.fs.no.hunger$est[9:16]
male.fs.no.hunger$est[9:16]
female.fs.with.hunger$est[9:16]
male.fs.with.hunger$est[9:16]

female.fs.no.hunger$est[17:24]
male.fs.no.hunger$est[17:24]
female.fs.with.hunger$est[17:24]
male.fs.with.hunger$est[17:24]


# Survey Adjusted Models --------------------------------------------------

#create a bootstrap replicate weight set
bootMale <-as.svrepdesign(male.svy, type="bootstrap", replicates=10)
bootFemale <-as.svrepdesign(fema.svy, type="bootstrap", replicates=10)
########################################################################################
#function to get ORs and 95% confidence intervals for the models
#note that the food security variable MUST be the first coefficent to get the right result
svymres <- function(mod){
  #get the betas: nrow = # outcomes -1, ncol = #predictors (here 2 bc fswithhunger is 3 level dummy)
  mfitcoef <- data.frame(matrix(attr(attr(mod, "var"), "means")[-1:-4],
                                nrow=nrow(coef(mod)),
                                ncol=ncol(coef(mod))-1,
                                byrow=F))
  #names(mfitcoef) <- c("fs_no_hunger", "fs_hunger")
  rownames(mfitcoef) <- rownames(coef(mod))
  
  #odds ratios
  round(exp(mfitcoef), 3)
  
  #get the covariance matrix for the betas : nrow and ncol = mfitcoef's nrow*(ncol +1)
  vcov <- matrix(attr(mod, "var"), nrow=length(coef(mod)), length(coef(mod)))
  
  #get the z tests by using the coefficients and the standard errors (diag(vcov))
  z <- as.vector(mfitcoef/sqrt(diag(vcov)[-1:-4]))
  round(z, 2) #show Z values
  
  #p values
  pvals <- round((1-pnorm(abs(as.matrix(z)),0,1))*2,4)
  round(pvals, 3)
  
  #95% confidence intervals
  se <- data.frame(matrix(sqrt(diag(vcov))[-1:-4], 
                          nrow = nrow(coef(mod)), 
                          ncol = ncol(coef(mod))-1, 
                          byrow = F))
  
  mfitcoef <- cbind(mfitcoef, se)
  
  #at index of ncol is the first se we want
  mfitcoef$fs_no_hunger_lower <- mfitcoef[,1] - (1.96*mfitcoef[, ncol(coef(mod))])
  mfitcoef$fs_no_hunger_upper <- mfitcoef[,1] + (1.96*mfitcoef[,ncol(coef(mod))])
  
  #next is at ncol + 1
  mfitcoef$fs_hunger_lower <- mfitcoef[,2] - (1.96*mfitcoef[,ncol(coef(mod))+1])
  mfitcoef$fs_hunger_upper <- mfitcoef[,2] + (1.96*mfitcoef[,ncol(coef(mod))+1])
  
  #print out the OR and 95% CI
  cat(noquote(paste0(sprintf(exp(mfitcoef[,1]), fmt = "%.3f"), " (",
                     sprintf(exp(mfitcoef$fs_no_hunger_lower), fmt = "%.3f"), " - ",
                     sprintf(exp(mfitcoef$fs_no_hunger_upper), fmt = "%.3f"), ")")), sep = "\n")
  
  cat(noquote(paste0(sprintf(exp(mfitcoef[,2]), fmt = "%.3f"), " (",
                     sprintf(exp(mfitcoef$fs_hunger_lower), fmt = "%.3f"), " - ",
                     sprintf(exp(mfitcoef$fs_hunger_upper), fmt = "%.3f"), ")")), sep = "\n")
}

#same thing when var has 3 levels (1 ref + 2 comparison)
svymres2 <- function(mod){
  #get the betas: nrow = # outcomes -1, ncol = #predictors (here 2 bc fswithhunger is 3 level dummy)
  mfitcoef <- data.frame(matrix(attr(attr(mod, "var"), "means")[-1:-2],
                                nrow=nrow(coef(mod)),
                                ncol=ncol(coef(mod))-1,
                                byrow=F))
  #names(mfitcoef) <- c("fs_no_hunger", "fs_hunger")
  rownames(mfitcoef) <- rownames(coef(mod))
  
  #odds ratios
  round(exp(mfitcoef), 3)
  
  #get the covariance matrix for the betas : nrow and ncol = mfitcoef's nrow*(ncol +1)
  vcov <- matrix(attr(mod, "var"), nrow=length(coef(mod)), length(coef(mod)))
  
  #get the z tests by using the coefficients and the standard errors (diag(vcov))
  z <- as.vector(mfitcoef/sqrt(diag(vcov)[-1:-2]))
  round(z, 2) #show Z values
  
  #p values
  pvals <- round((1-pnorm(abs(as.matrix(z)),0,1))*2,4)
  round(pvals, 3)
  
  #95% confidence intervals
  se <- data.frame(matrix(sqrt(diag(vcov))[-1:-2], 
                          nrow = nrow(coef(mod)), 
                          ncol = ncol(coef(mod))-1, 
                          byrow = F))
  
  mfitcoef <- cbind(mfitcoef, se)
  
  #at index of ncol is the first se we want
  mfitcoef$fs_no_hunger_lower <- mfitcoef[,1] - (1.96*mfitcoef[, ncol(coef(mod))])
  mfitcoef$fs_no_hunger_upper <- mfitcoef[,1] + (1.96*mfitcoef[,ncol(coef(mod))])
  
  #next is at ncol + 1
  mfitcoef$fs_hunger_lower <- mfitcoef[,2] - (1.96*mfitcoef[,ncol(coef(mod))+1])
  mfitcoef$fs_hunger_upper <- mfitcoef[,2] + (1.96*mfitcoef[,ncol(coef(mod))+1])
  
  #print out the OR and 95% CI
  cat(noquote(paste0(sprintf(exp(mfitcoef[,1]), fmt = "%.3f"), " (",
                     sprintf(exp(mfitcoef$fs_no_hunger_lower), fmt = "%.3f"), " - ",
                     sprintf(exp(mfitcoef$fs_no_hunger_upper), fmt = "%.3f"), ")")), sep = "\n")
  
  cat(noquote(paste0(sprintf(exp(mfitcoef[,2]), fmt = "%.3f"), " (",
                     sprintf(exp(mfitcoef$fs_hunger_lower), fmt = "%.3f"), " - ",
                     sprintf(exp(mfitcoef$fs_hunger_upper), fmt = "%.3f"), ")")), sep = "\n")
}
#####################################################################
#Now I fit the models: warning this is screwy looking!!!
#male
mfit <- withReplicates(bootMale, 
                       quote(coef(multinom(doingWt ~ fsWithHunger,
                                           weights= .weights,
                                           trace=F))))

svymres(mfit)

#female
ffit <- withReplicates(bootFemale, 
                       quote(coef(multinom(doingWt ~ fsWithHunger,
                                           weights= .weights,
                                           trace=F))))
svymres(ffit)
#########################################################
#BMI adjusted
mfit <- withReplicates(bootMale, 
                       quote(coef(multinom(doingWt ~ fsWithHunger + factor(BMIcat),
                                           weights= .weights,
                                           trace=F))))

svymres(mfit)

#female
ffit <- withReplicates(bootFemale, 
                       quote(coef(multinom(doingWt ~ fsWithHunger + factor(BMIcat),
                                           weights= .weights,
                                           trace=F))))
svymres(ffit)
############################################
#Fully Adjusted
mfit <- withReplicates(bootMale, 
                       quote(coef(multinom(doingWt ~ fsWithHunger + factor(BMIcat) +
                                             factor(age4)+ factor(edu) + factor(Race),
                                           weights= .weights,
                                           trace=F))))

svymres(mfit)

#female
ffit <- withReplicates(bootFemale, 
                       quote(coef(multinom(doingWt ~ fsWithHunger + factor(BMIcat) +
                                             factor(age4)+ factor(edu) + factor(Race),
                                           weights= .weights,
                                           trace=F))))
svymres(ffit)
##################################################################
#what you would like to weigh
#male
mfit <- withReplicates(bootMale, 
                       quote(coef(multinom(likeTo ~ fsWithHunger,
                                           weights= .weights,
                                           trace=F))))

svymres2(mfit)

#female
ffit <- withReplicates(bootFemale, 
                       quote(coef(multinom(likeTo ~ fsWithHunger,
                                           weights= .weights,
                                           trace=F))))
svymres2(ffit)
#########################################################
#BMI adjusted
mfit <- withReplicates(bootMale, 
                       quote(coef(multinom(likeTo ~ fsWithHunger + factor(BMIcat),
                                           weights= .weights,
                                           trace=F))))

svymres2(mfit)

#female
ffit <- withReplicates(bootFemale, 
                       quote(coef(multinom(likeTo ~ fsWithHunger + factor(BMIcat),
                                           weights= .weights,
                                           trace=F))))
svymres2(ffit)
############################################
#Fully Adjusted
mfit <- withReplicates(bootMale, 
                       quote(coef(multinom(likeTo ~ fsWithHunger + factor(BMIcat) +
                                             factor(age4)+ factor(edu) + factor(Race),
                                           weights= .weights,
                                           trace=F))))

svymres(mfit)

#female
ffit <- withReplicates(bootFemale, 
                       quote(coef(multinom(likeTo ~ fsWithHunger + factor(BMIcat) +
                                             factor(age4)+ factor(edu) + factor(Race),
                                           weights= .weights,
                                           trace=F))))
svymres(ffit)
