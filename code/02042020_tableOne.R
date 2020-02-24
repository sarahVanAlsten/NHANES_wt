#####################################
# Sarah Van Alsten                  #
# February 4, 2020                  #
# Create Table 1 for NHANES data    #
# Package:tableone,survey, tidyverse#
#####################################

#open libraries
library(tidyverse)
library(tableone)
library(survey)

#read in the data
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
svy <- svydesign(id = ~SDMVPSU, 
                 strata = ~SDMVSTRA, 
                 weights = ~WTMEC6YR, 
                 nest = TRUE, 
                 data = dat)



#variables I will probably want for descriptives:
#Age, Sex, Race, BMI(cat), Current Smoking, Depression, MJ use
#############################################################################
svyCreateTableOne(vars = c("Race", "edu", "BMIcat", "Male", "age4",
                           "maritalstatus", "smkStat", "consid",
                           "likeTo", "doingWt"),
                  strata = "fsWithHunger",
                  factorVars = c("Race", "edu", "BMIcat", "Male", "age4",
                                 "maritalstatus", "smkStat", "consid",
                                 "likeTo", "doingWt"),
                  data = svy)
