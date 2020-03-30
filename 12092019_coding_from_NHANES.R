#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: December 5, 2019                                                #
# Purpose: Datamanagement to recode diet behavior and weight for food security  #
# and weight perception in NHANES participants                                  #
# Packages Used: tidyverse, RNHANES, survey, tableone                           #
# Data Used: NHANES 2005, 2007, 2009, 2011                                      #
# Last Update: Nov 21, 2019                                                     #
#################################################################################
#create the survey sample design
library(survey)
library(tidyverse)
library(RNHANES)

#weight info
whq09 <- nhanes_load_data("WHQ", "2009-2010", demographics = T)
whq11 <- nhanes_load_data("WHQ", "2011-2012", demographics = T)
whq13 <- nhanes_load_data("WHQ", "2013-2014", demographics = T)
whq07 <- nhanes_load_data("WHQ", "2007-2008", demographics = T)
whq15 <- nhanes_load_data("WHQ", "2005-2006", demographics = T)

whq <- plyr::rbind.fill(whq09, whq11, whq13, whq07, whq15)

#clean up workspce
rm(whq09)
rm(whq11)
rm(whq13)
rm(whq07)
#############################################################################################################
#food security screens
fsq09 <- nhanes_load_data("FSQ", "2009-2010", demographics = F)
fsq11 <- nhanes_load_data("FSQ", "2011-2012", demographics = F)
fsq13 <- nhanes_load_data("FSQ", "2013-2014", demographics = F)
fsq07 <- nhanes_load_data("FSQ", "2007-2008", demographics = F)
fsq15 <- nhanes_load_data("FSQ", "2005-2006", demographics = F)

#put all the data into one frame
fsq <- plyr::rbind.fill(fsq09, fsq11, fsq13,fsq07, fsq15)

#clean up workspce
rm(fsq09)
rm(fsq11)
rm(fsq13)
rm(fsq07)
#########################################################################################################
#diet behavior
dbq09 <- nhanes_load_data("dbq", "2009-2010", demographics = F)
dbq11 <- nhanes_load_data("dbq", "2011-2012", demographics = F)
dbq13 <- nhanes_load_data("dbq", "2013-2014", demographics = F)
dbq07 <- nhanes_load_data("dbq", "2007-2008", demographics = F)
dbq15 <- nhanes_load_data("dbq", "2005-2006", demographics = F)

#put all the data into one frame
dbq <- plyr::rbind.fill(dbq09, dbq11, dbq13, dbq07, dbq15)

#clean up workspce
rm(dbq09)
rm(dbq11)
rm(dbq13)
rm(dbq07)
####################################
#drug use
#########################################################################################################
duq09 <- nhanes_load_data("duq", "2009-2010", demographics = F)
duq11 <- nhanes_load_data("duq", "2011-2012", demographics = F)
duq13 <- nhanes_load_data("duq", "2013-2014", demographics = F)
duq07 <- nhanes_load_data("duq", "2007-2008", demographics = F)
duq15 <- nhanes_load_data("duq", "2005-2006", demographics = F)


#put all the data into one frame
duq <- plyr::rbind.fill(duq09, duq11, duq13, duq07, duq15)

#clean up workspce
rm(duq09)
rm(duq11)
rm(duq13)
rm(duq07)
###########################################################
##Depression Screen
dpq09 <- nhanes_load_data("dpq", "2009-2010", demographics = F)
dpq11 <- nhanes_load_data("dpq", "2011-2012", demographics = F)
dpq13 <- nhanes_load_data("dpq", "2013-2014", demographics = F)
dpq07 <- nhanes_load_data("dpq", "2007-2008", demographics = F)
dpq15 <- nhanes_load_data("dpq", "2005-2006", demographics = F)


#put all the data into one frame
dpq <- plyr::rbind.fill(dpq09, dpq11, dpq13, dpq07, dpq15)

#clean up workspce
rm(dpq09)
rm(dpq11)
rm(dpq13)
rm(dpq07)
#############################################################
#Smoking
#########################################################################################################
smq09 <- nhanes_load_data("smq", "2009-2010", demographics = F)
smq11 <- nhanes_load_data("smq", "2011-2012", demographics = F)
smq13 <- nhanes_load_data("smq", "2013-2014", demographics = F)
smq07 <- nhanes_load_data("smq", "2007-2008", demographics = F)
smq15 <- nhanes_load_data("smq", "2005-2006", demographics = F)

#put all the data into one frame
smq <- plyr::rbind.fill(smq09, smq11, smq13, smq07, smq15)

#clean up workspce
rm(smq09)
rm(smq11)
rm(smq13)
rm(smq07)
#################################################################
#ALCOHOL
#########################################################################################################r
alq09 <- nhanes_load_data("alq", "2009-2010", demographics = F)
alq11 <- nhanes_load_data("alq", "2011-2012", demographics = F)
alq13 <- nhanes_load_data("alq", "2013-2014", demographics = F)
alq07 <- nhanes_load_data("alq", "2007-2008", demographics = F)
alq15 <- nhanes_load_data("alq", "2005-2006", demographics = F)

#put all the data into one frame
alq <- plyr::rbind.fill(alq09, alq11, alq13, alq07, alq15)

#clean up workspce
rm(alq09)
rm(alq11)
rm(alq13)
rm(alq07)
##########################################
#ACCULTURATION
#########################################################################################################
acq09 <- nhanes_load_data("acq", "2009-2010", demographics = F)
acq11 <- nhanes_load_data("acq", "2011-2012", demographics = F)
acq13 <- nhanes_load_data("acq", "2013-2014", demographics = F)
acq07 <- nhanes_load_data("acq", "2007-2008", demographics = F)
acq15 <- nhanes_load_data("acq", "2005-2006", demographics = F)

#put all the data into one frame
acq <- plyr::rbind.fill(acq09, acq11, acq13, acq07, acq15)

#clean up workspce
rm(acq09)
rm(acq11)
rm(acq13)
rm(acq07)
##########################################
#BODY MEASURES
#########################################################################################################
bmx09 <- nhanes_load_data("bmx", "2009-2010", demographics = F)
bmx11 <- nhanes_load_data("bmx", "2011-2012", demographics = F)
bmx13 <- nhanes_load_data("bmx", "2013-2014", demographics = F)
bmx07 <- nhanes_load_data("bmx", "2007-2008", demographics = F)
bmx15 <- nhanes_load_data("bmx", "2005-2006", demographics = F)

#put all the data into one frame
bmx <- plyr::rbind.fill(bmx09, bmx11, bmx13, bmx07, bmx15)

#clean up workspce
rm(bmx09)
rm(bmx11)
rm(bmx13)
rm(bmx07)

#################################################################################################
#Sexual Behavior 
sxq09 <- nhanes_load_data("sxq", "2009-2010", demographics = F)
sxq11 <- nhanes_load_data("sxq", "2011-2012", demographics = F)
sxq13 <- nhanes_load_data("sxq", "2013-2014", demographics = F)
sxq07 <- nhanes_load_data("sxq", "2007-2008", demographics = F)
sxq05 <- nhanes_load_data("sxq", "2005-2006", demographics = F)


#put all the data into one frame
sxq <- plyr::rbind.fill(sxq09, sxq11, sxq13, sxq07, sxq05)

#clean up workspce
rm(sxq09)
rm(sxq11)
rm(sxq13)
rm(sxq07)
############################################################################
#General Health Status
hsq09 <- nhanes_load_data("hsq", "2009-2010", demographics = F)
hsq11 <- nhanes_load_data("hsq", "2011-2012", demographics = F)
hsq13 <- nhanes_load_data("hsq", "2013-2014", demographics = F)
hsq07 <- nhanes_load_data("hsq", "2007-2008", demographics = F)
hsq05 <- nhanes_load_data("hsq", "2005-2006", demographics = F)

#put all the data into one frame
hsq <- plyr::rbind.fill(hsq09, hsq11, hsq13, hsq07, hsq05)

#clean up workspce
rm(hsq09)
rm(hsq11)
rm(hsq13)
rm(hsq07)
###############################################################################
#Merge data by respondent ID to get one data frame
dat <- merge(
  merge(
  merge(
  merge(
    merge(
      merge(
        merge(
          merge(
            merge(
              whq, smq, by = "SEQN"),
            fsq, by = "SEQN"),
          duq, by = "SEQN"), 
        dpq, by = "SEQN"),
      bmx, by = "SEQN"),
    alq, by = "SEQN"),
  acq, by = "SEQN"),
hsq, by = "SEQN"),
sxq, by = "SEQN")

#get rid of unnecessary repeated columns
dat <- dat[,!names(dat)%in% c("file_name.x", "file_name.y", "end_year.y", "end_year.x", "begin_year.x", 
                              "begin_year.y", "begin_year.y.2", "begin_year.x.2", 
                              "begin_year.x.3", "begin_year.y.3", "begin_year.x.1", "begin_year.y.1",
                              "cycle.x.3", "cycle.y.3", "cycle.y.1", "cycle.x.2", "cycle.x.1",
                              "cycle.y"),]

#check the merging
names(dat)

#rename the cycle column
dat$cycle <- dat$cycle.x
dat <- dat[, !names(dat) %in% "cycle.x"]
#######################################################################################
#recode variables: Demographics
dat <- dat %>%
  mutate(Income = ifelse(INDFMPIR <=1, 1,
                         ifelse(INDFMPIR <=2,2,
                                ifelse(INDFMPIR <=3, 3, 
                                       ifelse(INDFMPIR  <=4, 4,
                                              ifelse(INDFMPIR < 5, 5,
                                                     ifelse(INDFMPIR ==5, 6, NA))))))) %>%
  mutate(Race = ifelse(RIDRETH1 %in% c(1,2), 2, #hispanic
                       ifelse(RIDRETH1 == 3,0, #white
                              ifelse(RIDRETH1 == 4, 1, #black
                                     ifelse(RIDRETH1 == 5, 3,NA)))))  %>% #other
  mutate(Male = ifelse(RIAGENDR == 1, 1,
                       ifelse(RIAGENDR == 2,0, NA)))


#####################################################
#weight related questions
dat <- dat %>%
  mutate(ConsiderWt = ifelse(WHQ030 == 3, 0, #right
                             ifelse(WHQ030 == 1, 1, #overweight
                                    ifelse(WHQ030 ==2 , -1, NA))),#underweight
         LikeToWeigh =ifelse(WHQ040 == 3, 0, #same
                             ifelse(WHQ040 == 1, 1, #gain
                                    ifelse(WHQ040 ==2 , -1, NA))))#less
#bmi categories
dat <- dat %>%
  mutate(BMIcat = ifelse(BMXBMI < 18.5, 1,
                         ifelse(BMXBMI < 25, 2,
                                ifelse(BMXBMI < 30, 3,
                                       ifelse(BMXBMI < 35, 4,
                                              ifelse(BMXBMI < 40, 5,
                                                     ifelse(is.na(BMXBMI), NA, 6)))))))

#if people's weight perception lines up with reality (eg you're underweight and think you weight too little)
dat <- dat %>%
  mutate(correctConsid = ifelse(BMIcat == 1 & ConsiderWt == -1, 1,
                                ifelse(BMIcat == 1 & !is.na(ConsiderWt), 0,
                                       ifelse(BMIcat == 2 & ConsiderWt == 0, 1,
                                              ifelse(BMIcat == 2 & !is.na(ConsiderWt), 0,
                                                     ifelse(BMIcat %in% c(3,4,5,6) & ConsiderWt == 1, 1,
                                                            ifelse(BMIcat %in% c(3,4,5,6) & ConsiderWt %in% c(0,-1), 0, NA))))))) %>%
  
  #if people's desired weight direction lines up (eg you're underweight and want to weigh more)
  mutate(correctLikeWeigh= ifelse(BMIcat == 1 & LikeToWeigh == 1, 1,
                                  ifelse(BMIcat == 1 & !is.na(LikeToWeigh), 0,
                                         ifelse(BMIcat == 2 & LikeToWeigh == 0, 1,
                                                ifelse(BMIcat == 2 & !is.na(LikeToWeigh), 0,
                                                       ifelse(BMIcat %in% c(3,4,5,6) & LikeToWeigh == -1, 1,
                                                              ifelse(BMIcat %in% c(3,4,5,6) & LikeToWeigh %in% c(0,1), 0, NA)))))))

#create category for what they're doing about weight
#1: lost weight intentionally
#2: lost weight unintentionally
#3: tried to lose weight but did not
#4: tried to not gain weight (not included in 2013 cycle)
#5: all others (not trying to do anything)
table(dat$WHQ060, dat$WHQ070, useNA = "ifany")
table(dat$WHQ060)

dat <- dat %>%
  mutate(doingAbtWt = ifelse(is.na(WHQ060) & WHQ070 == 1, 3,
                             ifelse(is.na(WHQ060) & WHQ070 == 2 & WHQ090 == 1, 4,
                                    ifelse(is.na(WHQ060) & WHQ070 == 2 & WHQ090 == 2, 5,
                                           ifelse(WHQ060 == 1, 1,
                                                  ifelse(WHQ060 == 2 & WHQ070 == 1, 1,
                                                         ifelse(WHQ060 == 2 & WHQ070 == 2, 2, NA)))))))

#check codings    
table(dat$doingAbtWt, useNA = "ifany")

#variables for if wt change intentional and if tried to lose weight past year
dat <- dat %>%
  mutate(lastYrLose = ifelse(WHQ070 ==2, 0, ifelse(WHQ070 == 1, 1, NA))) #have you tried to lose wt in last year

dat <- dat %>%
  mutate(intentional = ifelse(is.na(WHQ060),NA,
                              ifelse(WHQ060 == 2, 0, 
                                     ifelse(WHQ060 == 1, 1, NA)))) 

#all the diet bx questions: in last yr to lose weight did you...
dat <- dat %>%
  mutate(
    lastYrAteLess= ifelse(lastYrLose == 0, 0,
                          ifelse(WHD080A == 10, 1,
                                 ifelse(lastYrLose ==1, 0, NA))),
    lastYrSwitchFood= ifelse(lastYrLose == 0, 0,
                             ifelse(WHD080B == 11, 1,
                                    ifelse(lastYrLose ==1, 0, NA))),
    lastYrLessFat= ifelse(lastYrLose == 0, 0,
                          ifelse(WHD080C == 12, 1,
                                 ifelse(lastYrLose ==1, 0, NA))),
    lastYrExercise= ifelse(lastYrLose == 0, 0,
                           ifelse(WHD080D == 13, 1,
                                  ifelse(lastYrLose ==1, 0, NA))),
    lastYrSkipMeal= ifelse(lastYrLose == 0, 0,
                           ifelse(WHD080E == 14, 1,
                                  ifelse(lastYrLose ==1, 0, NA))),
    lastYrDietFood= ifelse(lastYrLose == 0, 0,
                           ifelse(WHD080F == 15, 1,
                                  ifelse(lastYrLose ==1, 0, NA))),
    lastYrLiquidDiet= ifelse(lastYrLose == 0, 0,
                             ifelse(WHD080G == 16, 1,
                                    ifelse(lastYrLose ==1, 0, NA))),
    lastYrJoinProgram= ifelse(lastYrLose == 0, 0,
                              ifelse(WHD080H == 17, 1,
                                     ifelse(lastYrLose ==1, 0, NA))),
    lastYrDietPill= ifelse(lastYrLose == 0, 0,
                           ifelse(WHD080I == 31, 1,
                                  ifelse(lastYrLose ==1, 0, NA))),
    lastYrOthRx= ifelse(lastYrLose == 0, 0,
                        ifelse(WHD080J == 32, 1,
                               ifelse(lastYrLose ==1, 0, NA))),
    lastYrLaxVom= ifelse(lastYrLose == 0, 0,
                         ifelse(WHD080K == 33, 1,
                                ifelse(lastYrLose ==1, 0, NA))),
    lastYrOther= ifelse(lastYrLose == 0, 0,
                        ifelse(WHD080L == 40, 1,
                               ifelse(lastYrLose ==1, 0, NA))),
    lastYrMoreH20= ifelse(lastYrLose == 0, 0,
                          ifelse(WHD080M == 34, 1,
                                 ifelse(lastYrLose ==1, 0, NA))),
    lastYrSpecDiet= ifelse(lastYrLose == 0, 0,
                           ifelse(WHD080N == 30, 1,
                                  ifelse(lastYrLose ==1, 0, NA))),
    lastYrLowCarb= ifelse(lastYrLose == 0, 0,
                          ifelse(WHD080O == 41, 1,
                                 ifelse(lastYrLose ==1, 0, NA))),
    lastYrRestartSmoke= ifelse(lastYrLose == 0, 0,
                               ifelse(WHD080P == 42, 1,
                                      ifelse(lastYrLose ==1, 0, NA))),
    lastYrFruitVeg= ifelse(lastYrLose == 0, 0,
                           ifelse(WHD080Q == 43, 1,
                                  ifelse(lastYrLose ==1, 0, NA))),
    lastYrChangeDiet= ifelse(lastYrLose == 0, 0,
                             ifelse(WHD080R == 44, 1,
                                    ifelse(lastYrLose ==1, 0, NA))),
    lastYrAteLessSweet= ifelse(lastYrLose == 0, 0,
                               ifelse(WHD080S == 45, 1,
                                      ifelse(lastYrLose ==1, 0, NA))),
    lastYrAteLessJunk= ifelse(lastYrLose == 0, 0,
                              ifelse(WHD080T == 46, 1,
                                     ifelse(lastYrLose ==1, 0, NA))))


#tried not to gain weight in last year and what bx did to prevent that (asked in 3/4 cycles)
dat <-
  dat %>%
  mutate(tryNotGain = ifelse(WHQ090 == 1, 1,
                             ifelse(WHQ090 == 2, 0, NA))) 

#currently these are interpreted as integers which is causing problems: convert to numeric
dat[ , c("WHD100A", "WHD100B", "WHD100C", "WHD100D", "WHD100E",
         "WHD100F", "WHD100G", "WHD100H", "WHD100I",
         "WHD100J", "WHD100K", "WHD100L", "WHD100M", "WHD100N",
         "WHD100O", "WHD100P", "WHD100Q", "WHD100R", "WHD100S",
         "WHD100T")] <- apply(dat[ , c("WHD100A", "WHD100B", "WHD100C", "WHD100D", "WHD100E",
                                       "WHD100F", "WHD100G", "WHD100H", "WHD100I",
                                       "WHD100J", "WHD100K", "WHD100L", "WHD100M", "WHD100N",
                                       "WHD100O", "WHD100P", "WHD100Q", "WHD100R", "WHD100S",
                                       "WHD100T")], FUN = as.numeric, MARGIN = 2)

#applied to 3/4 cycles
dat <- dat %>%
  mutate(ngAteLess = ifelse(is.na(WHD100A) & tryNotGain == 1, 0, 
                            ifelse(WHD100A == 10, 1, NA))) %>%
  mutate(ngLowCal = ifelse(is.na(WHD100B) & tryNotGain == 1, 0, 
                           ifelse(WHD100B == 11, 1, NA))) %>%
  mutate(ngLessFat = ifelse(is.na(WHD100C) & tryNotGain == 1, 0, 
                            ifelse(WHD100C == 12, 1, NA))) %>%
  mutate(ngExerc = ifelse(is.na(WHD100D) & tryNotGain == 1, 0, 
                          ifelse(WHD100D == 13, 1, NA))) %>%
  mutate(ngSkipMeal = ifelse(is.na(WHD100E) & tryNotGain == 1, 0, 
                             ifelse(WHD100E == 14, 1, NA))) %>%
  mutate(ngDietFood = ifelse(is.na(WHD100F) & tryNotGain == 1, 0, 
                             ifelse(WHD100F == 15, 1, NA))) %>%
  mutate(ngLiquidDiet = ifelse(is.na(WHD100G) & tryNotGain == 1, 0, 
                               ifelse(WHD100G == 16, 1, NA))) %>%
  mutate(ngJoinProgram = ifelse(is.na(WHD100H) & tryNotGain == 1, 0, 
                                ifelse(WHD100H == 17, 1, NA))) %>%
  mutate(ngDietPill = ifelse(is.na(WHD100I) & is.na(WHD100J) & tryNotGain == 1, 0, 
                             ifelse(WHD100I == 31 | WHD100J == 32, 1, NA))) %>%
  mutate(ngLaxVom = ifelse(is.na(WHD100K) & tryNotGain == 1, 0,  
                           ifelse(WHD100K == 33, 1, NA))) %>%
  mutate(ngMoreH20 = ifelse(is.na(WHD100L) & tryNotGain == 1, 0, 
                            ifelse(WHD100L == 34, 1, NA))) %>%
  mutate(ngSpecDiet = ifelse(is.na(WHD100M) & tryNotGain == 1, 0, 
                             ifelse(WHD100M == 30, 1, NA))) %>%
  mutate(ngLowCarb = ifelse(is.na(WHD100N) & tryNotGain == 1, 0, 
                            ifelse(WHD100N == 41, 1, NA))) %>%
  mutate(ngSmoke = ifelse(is.na(WHD100O) & tryNotGain == 1, 0, 
                          ifelse(WHD100O == 42, 1, NA))) %>%
  mutate(ngMoreProduce = ifelse(is.na(WHD100P) & tryNotGain == 1, 0, 
                                ifelse(WHD100P == 43, 1, NA))) %>%
  mutate(ngChangeDiet = ifelse(is.na(WHD100Q) & tryNotGain == 1, 0, 
                               ifelse(WHD100Q == 44, 1, NA))) %>%
  mutate(ngLessSweet = ifelse(is.na(WHD100R) & tryNotGain == 1, 0, 
                              ifelse(WHD100R == 45, 1, NA))) %>%
  mutate(ngLessFastFood = ifelse(is.na(WHD100S) & tryNotGain == 1, 0, 
                                 ifelse(WHD100S == 46, 1, NA))) %>%
  mutate(ngOther = ifelse(is.na(WHD100T) & tryNotGain == 1, 0, 
                          ifelse(WHD100T == 40, 1, NA)))


###################################################################################3
#food security questions
dat <-
  dat %>%
  mutate(hhFScat = FSDHH, #household score
         kidFScat = FSDCH, #child score
         adFScat = FSDAD) #adult score

#derive more informative food security scores (continuous)
# FSDAD (Adult food security category):
#Count affirmative responses in these 10 items: FSD032a, FSD032b, FSD032c, FSD041, FSD052, FSD061, FSD071, FSD081, FSD092, and FSD102. 
#For households without children under the age of 18, their household food security category (FSDHH) should be identical 
#to their adult food security category (FSDAD).
# 
#FSDCH (Child food security category):
#   
#This category is only generated for households with children under the age of 18. 
#Count affirmative responses in these 8 items: FSD032d, FSD032e, FSD032f, FSD111, FSD122, FSD132, FSD141, and FSD146.

#recode per NHANES guidelines: yes/ fairly often or often = YES
dat <-
  dat %>%
  mutate(FSD032Ar = ifelse(FSD032A %in% c(1,2), 1, ifelse(FSD032A == 3, 0, NA)),
         FSD032Br = ifelse(FSD032B %in% c(1,2), 1, ifelse(FSD032B == 3, 0, NA)),
         FSD032Cr = ifelse(FSD032C %in% c(1,2), 1, ifelse(FSD032C == 3, 0, NA)),
         FSD041r = ifelse(FSD041 ==1, 1, ifelse(FSD041 == 2, 0, NA)),
         FSD052r = ifelse(FSD052 %in% c(1,2), 1, ifelse(FSD052 == 3, 0, NA)),
         FSD061r = ifelse(FSD061 ==1, 1, ifelse(FSD061 == 2, 0, NA)),
         FSD071r = ifelse(FSD071 ==1, 1, ifelse(FSD071 == 2, 0, NA)),
         FSD081r = ifelse(FSD081 ==1, 1, ifelse(FSD081 == 2, 0, NA)),
         FSD092r = ifelse(FSD092 ==1, 1, ifelse(FSD092 == 2, 0, NA)),
         FSD102r = ifelse(FSD102 %in% c(1,2), 1, ifelse(FSD102 == 3, 0, NA)),
         FSD032Dr = ifelse(FSD032D %in% c(1,2), 1, ifelse(FSD032D == 3, 0, NA)),
         FSD032Er = ifelse(FSD032E %in% c(1,2), 1, ifelse(FSD032E == 3, 0, NA)),
         FSD032Fr = ifelse(FSD032F %in% c(1,2), 1, ifelse(FSD032F == 3, 0, NA)),
         FSD111r = ifelse(FSD111 ==1, 1, ifelse(FSD111 == 2, 0, NA)),
         FSD122r = ifelse(FSD122 ==1, 1, ifelse(FSD122 == 3, 0, NA)),
         FSD132r = ifelse(FSD132 %in% c(1,2), 1, ifelse(FSD132 == 3, 0, NA)),
         FSD141r = ifelse(FSD141 ==1, 1, ifelse(FSD141 == 2, 0, NA)),
         FSD146r = ifelse(FSD146 ==1, 1, ifelse(FSD146 == 3, 0, NA)))

#continuous score of adult food insecurity
dat <- dat %>% 
  #rowwise will make sure the sum operation will occur on each row
  rowwise() %>% 
  #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
  mutate(adFScont = sum(FSD032Ar ,FSD032Br,FSD032Cr ,FSD041r ,FSD052r , 
                        FSD061r ,FSD071r ,FSD081r, FSD092r ,FSD102r, na.rm=TRUE))

#Continuous score of childhood food insecurity
dat <- dat %>%
  rowwise() %>%
  mutate(kidFScont = sum(FSD032Dr, FSD032Er, FSD032Fr, FSD111r, FSD122r, FSD132r, FSD141r, FSD146r, na.rm = TRUE))

#household food insecurity total: continuous
dat <- dat %>%
  rowwise() %>%
  mutate(hhFScont = sum(adFScont, kidFScont, na.rm =T))

#make a male category so we can see labels
dat <- dat %>%
  mutate(maleFact = ifelse(Male == 1, "Male",
                           ifelse(Male == 0, "Female", NA))) %>%
  mutate(maleFact = as.factor(maleFact))

#make a what you like to weigh factor and a consider your weight factor
dat <- dat %>%
  mutate(likeToWeighFact = recode_factor(.x = LikeToWeigh,
                                         `-1` = "Less",
                                         `0` = "Same",
                                         `1` = "More",
                                         .ordered = F)) %>%
  mutate(considerWeightFact = recode_factor(.x = ConsiderWt,
                                            `-1` = "Underweight",
                                            `0` = "About Right",
                                            `1` = "Overweight",
                                            .ordered = F))

#set reference groups as 'same' or 'about right'
dat$likeToWeighFact <- relevel(factor(dat$likeToWeighFact, ordered = F), ref = "Same")
dat$considerWeightFact <- relevel(factor(dat$considerWeightFact, ordered = F), ref = "About Right")

dat <- dat %>%
  mutate(percepActCong = ifelse(ConsiderWt == -1 & LikeToWeigh == 1, 1,
                                ifelse(ConsiderWt == 0 & LikeToWeigh == 0, 1,
                                       ifelse(ConsiderWt == 1 & LikeToWeigh == -1, 1,
                                              ifelse(!is.na(ConsiderWt)& ! is.na(LikeToWeigh), 0, NA)))))

#table(dat$percepActCong)
#table(dat$hhFScont, dat$BMIcat)
#table(dat$hhFScont, dat$ConsiderWt)
#prop.table(table(dat$hhFScont, dat$ConsiderWt),1)
#table(dat$hhFS_recode, dat$LikeToWeigh)

dat <-
  dat %>%
  mutate(hhFS_recode = ifelse(hhFScont == 0, 0, 
                              ifelse(hhFScont < 10, hhFScont,
                                     ifelse(!is.na(hhFScont), 10, NA))))

#how do considerations of weight seem to differ by level of food insecurity
dat %>%
  drop_na(ConsiderWt) %>%
  group_by(hhFS_recode, ConsiderWt) %>%
  count() %>%
  group_by(hhFS_recode) %>%
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot() +
  geom_bar(aes(x = hhFS_recode, y = percent,
               fill = factor(ConsiderWt), color = factor(ConsiderWt)), 
           stat = "identity", position = "fill")

#how does what you like to weigh seem to differ by level of food insecurity
dat %>%
  drop_na(LikeToWeigh) %>%
  group_by(hhFS_recode, LikeToWeigh) %>%
  count() %>%
  group_by(hhFS_recode) %>%
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot() +
  geom_bar(aes(x = hhFS_recode, y = percent,
               fill = factor(LikeToWeigh), color = factor(LikeToWeigh)), 
           stat = "identity", position = "fill")

#how do what doing abt weight seem to differ by level of food insecurity
dat %>%
  drop_na(doingAbtWt) %>%
  group_by(hhFS_recode, doingAbtWt) %>%
  count() %>%
  group_by(hhFS_recode) %>%
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot() +
  geom_bar(aes(x = hhFS_recode, y = percent,
               fill = factor(doingAbtWt), color = factor(doingAbtWt)), 
           stat = "identity", position = "fill")


#use larger blocks of Food Insecurity (the categorical version)
dat %>%
  drop_na(ConsiderWt) %>%
  group_by(hhFScat, ConsiderWt) %>%
  count() %>%
  group_by(hhFScat) %>%
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot() +
  geom_bar(aes(x = hhFScat, y = percent,
               fill = factor(ConsiderWt), color = factor(ConsiderWt)), 
           stat = "identity", position = "fill")

#how do what doing abt weight seem to differ by level of food insecurity
dat %>%
  drop_na(doingAbtWt) %>%
  group_by(hhFScat, doingAbtWt) %>%
  count() %>%
  group_by(hhFScat) %>%
  mutate(percent = 100*(n/sum(n))) %>%
  ggplot() +
  geom_bar(aes(x = hhFScat, y = percent,
               fill = factor(doingAbtWt), color = factor(doingAbtWt)), 
           stat = "identity", position = "fill")

#per nhanes guidelines, to collapse accross year create sample weight that is original
#divided by number of collapsed cycles
dat <-
  dat %>%
  rowwise() %>%
  mutate(WTMEC8YR = WTMEC2YR / 4,
         WTMEC6YR = WTMEC2YR / 3,
         WTMEC4YR = WTMEC2YR /2,
         WTMEC10YR = WTMEC2YR/5)

#make a variable to represent food security with and without hunger
dat <- dat %>%
  mutate(fsWithHunger = ifelse(FSDHH %in% c(2,3,4) & FSD071 == 2, 2, #with hunger
                               ifelse(FSDHH %in% c(2,3,4), 1, #without hunger
                                      ifelse(FSDHH == 1, 0, NA)))) #not insecure

#Depression Screener
dat <- dat %>%
  mutate(dpq010r = ifelse(DPQ010 %in% c(7,9, NA), NA, DPQ010),
         dpq020r = ifelse(DPQ020 %in% c(7,9, NA), NA, DPQ020),
         dpq030r = ifelse(DPQ030 %in% c(7,9, NA), NA, DPQ030),
         dpq040r = ifelse(DPQ040 %in% c(7,9, NA), NA, DPQ040),
         dpq050r = ifelse(DPQ050 %in% c(7,9, NA), NA, DPQ050),
         dpq060r = ifelse(DPQ060 %in% c(7,9, NA), NA, DPQ060),
         dpq070r = ifelse(DPQ070 %in% c(7,9, NA), NA, DPQ070),
         dpq080r = ifelse(DPQ080 %in% c(7,9, NA), NA, DPQ080),
         dpq090r = ifelse(DPQ090 %in% c(7,9, NA), NA, DPQ090),
         dpq100r = ifelse(DPQ100 %in% c(7,9, NA), NA, DPQ100))

#phq9 = sum of dpq questions
dat <-
  dat %>%
  rowwise()%>%
  mutate(phq9 = sum(dpq010r, dpq020r, dpq030r, dpq040r, 
                    dpq050r, dpq060r, dpq070r, dpq080r, dpq090r, na.rm =T))

table(dat$phq9)

#use cutoff scores for PHQ9 to classify depressive sx: 
#<5 = no, 5-9 = minimal, 10 - 14 = moderate, 15 - 19 = severe, 20+ = very severe
dat <-
  dat %>%
  mutate(depression = ifelse(phq9 < 5, 0,
                             ifelse(phq9 < 10, 1,
                                    ifelse(phq9 < 15, 2,
                                           ifelse(phq9 < 20, 3,
                                                  ifelse(phq9 < 28, 4, NA)))))) %>% 
  mutate(depressionBinary = ifelse(depression %in% c(2,3,4), 1,
                                   ifelse(depression %in% c(0,1), 0, NA)))



table(dat$depressionBinary, dat$hhFScat)
prop.table(table(dat$depressionBinary, dat$hhFScat), margin = 2)
###################################################################
#Acculturation: language usually spoken 
dat <-
  dat %>%
  mutate(language = ifelse(is.na(ACD010A) & is.na(ACD010B) & is.na(ACD010C), NA,
                           ifelse(ACD010A == 1, "English",
                                  ifelse(ACD010B == 8, "Spanish",
                                         ifelse(ACD010C == 9, "Other")))))

######################################################################################
#Alcohol Use
dat <-
  dat %>%
  #12 drinks lifetime
  mutate(twelveDrinksLt = ifelse(ALQ101 %in% c(7, 9, NA), NA,
                                 ifelse(ALQ101 == 1, 1,
                                        ifelse(ALQ110 == 1, 1, 0)))) %>%
  #convert drinks to common unit (weekly)
  mutate(alcUnit = ifelse(ALQ120U == 1, 1,
                          ifelse(ALQ120U == 2, .25,
                                 ifelse(ALQ120U == 3, .25/52,
                                        ifelse(is.na(ALQ120U) | ALQ120U %in% c(7,9), NA, NA))))) %>%
  mutate(alcWeek = ifelse(is.na(alcUnit) | ALQ120Q %in% c(777,999, NA), NA,
                          ifelse(twelveDrinksLt == 0, 0, ALQ120Q * alcUnit))) %>%
  mutate(bingeDrk = ifelse(twelveDrinksLt == 0, 0,
                           ifelse(ALQ140Q >= 1 & ALQ140Q < 370, 1,NA))) #ever binge drank in last year

########################################################################
#Smoking
dat <-
  dat %>%
  mutate(smoke100 = ifelse(SMQ020 %in% c(9,7,NA), NA,
                           ifelse(SMQ020 == 1, 1, 0))) %>%
  mutate(nowSmoke = ifelse(SMQ040 %in% c(9,7, NA), NA,
                           ifelse(SMQ040 == 3, 0, 1))) %>%
  mutate(smkStat = ifelse(smoke100 == 0, 0, #never
                              ifelse(nowSmoke == 0 & smoke100 == 1, 1, 
                                     ifelse(nowSmoke == 1, 2, NA)))) #former

########################################################################
#Age category, marital status, and education
dat <-
  dat %>%
  mutate(age4 = ifelse(RIDAGEYR > 20 & RIDAGEYR <= 29, 1,
                       ifelse(RIDAGEYR > 29 & RIDAGEYR <= 39, 2,
                              ifelse(RIDAGEYR > 39 & RIDAGEYR <= 49, 3,
                                     ifelse(RIDAGEYR > 49 & RIDAGEYR <= 59, 4, NA)))))
#NOTE: I changed this so that all people who were out of age range for the sexual orientation questions
#were coded 2;
dat <-
  dat %>%
  mutate(age = ifelse(age4 %in% c(1,2), 0,
                      ifelse(age4 %in% c(3,4), 1, 2)))

dat <-
  dat %>%
  mutate(maritalstatus = ifelse(DMDMARTL == 1, 0,
                                ifelse(DMDMARTL == 5, 1,
                                       ifelse(DMDMARTL == 6, 2,
                                              ifelse(DMDMARTL %in% c(2,3,4), 3, NA))))) %>%
  mutate(maritalstatus2 = ifelse(maritalstatus %in% c(0,1), 0,
                                 maritalstatus - 2))
dat <-
  dat %>%
  mutate(edu = ifelse(DMDEDUC2 %in% c(1,2), 0,
                      ifelse(DMDEDUC2 == 3, 1,
                             ifelse(DMDEDUC2 %in% c(4,5), 2, NA))))

#######################################################################
#Sexual Orientation/ Sexual Behavior
#First, make a separte female and male data set to make it easier to work with
fem <- dat %>%
  filter(Male == 0)

male <- dat %>%
  filter(Male == 1)

#questions were asked differently in 07-08 vs 09-10 and 11-12:
#again separate out to make it easier with the ifelse statements
fem07 <- fem %>%
  filter(cycle.x == "2007-2008")
fem0911 <- fem %>%
  filter(!cycle.x == "2007-2008")
male07 <- male %>%
  filter(cycle.x == "2007-2008")
male0911 <- male %>%
  filter(!cycle.x == "2007-2008")

#for female 09-11: follow julia's code
#never had any sex: vaginal, anal, oral M or W
fem0911 <- fem0911 %>%
  mutate(neversex = ifelse(SXQ700 != 1 & SXQ703 !=1 & SXQ706 !=1 & SXQ709 != 1, 1, 0))

fem0911 <- fem0911 %>%
  mutate(hetero = ifelse(SXQ294 == 1, 1,
                         ifelse(SXQ294 %in% c(2,3,4,5,9), 0, NA))) %>%
  mutate(orient = ifelse(hetero == 1, 1, #hetero
                         ifelse(SXQ294 %in% c(2,3), 2, #lesbian/gay
                                ifelse(SXQ294 == 4, 3, #bisexual
                                       ifelse(SXQ294 %in% c(5,9), 4, NA)))))%>% #something else
  mutate(wsexw = ifelse(SXQ709 == 1, 1, #had sex with women
                        ifelse(SXQ709 == 2, 0, NA))) %>% #didn't have sex with women
  mutate(newwsw = ifelse(hetero == 1 | wsexw == 0, 0, #hetero or no women sex
                         ifelse(orient %in% c(2,3) | wsexw == 1, 1, NA))) #self labeled lesbian/gay/bi or sex with w

fem07 <- fem07 %>%
  mutate(hetero = ifelse(SXQ294 == 1, 1,
                         ifelse(SXQ294 %in% c(2,3,4,5,9), 0, NA))) %>%
  mutate(orient = ifelse(hetero == 1, 1, #hetero
                         ifelse(SXQ294 %in% c(2,3), 2, #lesbian/gay
                                ifelse(SXQ294 == 4, 3, #bisexual
                                       ifelse(SXQ294 %in% c(5,9), 4, NA)))))%>% #something else
  mutate(wsexw = ifelse(SXQ130 == 0, 0, #zero female partners
                        ifelse(SXQ130 > 0 & SXQ130 < 777, 1, NA))) %>%
  mutate(newwsw = ifelse(hetero == 1 | wsexw == 0, 0, #hetero or no women sex
                         ifelse(orient %in% c(2,3) | wsexw == 1, 1, NA)))%>% #self labeled lesbian/gay/bi or sex with w
  mutate(neversex = ifelse(SXQ021 == 2, 1, 0))

#put the two back together
fem <- rbind(fem07, fem0911)

#males
male0911 <- male0911 %>%
  mutate(neversex = ifelse(SXQ800 != 1 & SXQ803 !=1 & SXQ806 !=1 & SXQ809 != 1, 1, 0))

male07 <- male07 %>%
  mutate(hetero = ifelse(SXQ292 == 1, 1,
                         ifelse(SXQ292 %in% c(2,3,4,5,9), 0, NA))) %>%
  mutate(orient = ifelse(hetero == 1, 1, #hetero
                         ifelse(SXQ292 %in% c(2,3), 2, #lesbian/gay
                                ifelse(SXQ292 == 4, 3, #bisexual
                                       ifelse(SXQ292 %in% c(5,9), 4, NA)))))%>% #something else
  mutate(wsexw = ifelse(SXQ410 == 0, 0, #zero male partners
                        ifelse(SXQ410 > 0 & SXQ410 < 777, 1, NA))) %>% #here it's really msexm but that would mess up
  #when putting the columns back together
  mutate(newwsw = ifelse(hetero == 1 | wsexw == 0, 0, #hetero or no women sex
                         ifelse(orient %in% c(2,3) | wsexw == 1, 1, NA)))%>% #self labeled lesbian/gay/bi or sex with m
  mutate(neversex = ifelse(SXQ021 == 2, 1, 0))

male0911 <- male0911 %>%
  mutate(hetero = ifelse(SXQ292 == 1, 1,
                         ifelse(SXQ292 %in% c(2,3,4,5,9), 0, NA))) %>%
  mutate(orient = ifelse(hetero == 1, 1, #hetero
                         ifelse(SXQ292 %in% c(2,3), 2, #lesbian/gay
                                ifelse(SXQ292 == 4, 3, #bisexual
                                       ifelse(SXQ292 %in% c(5,9), 4, NA)))))%>% #something else
  mutate(wsexw = ifelse(SXQ410 == 0, 0, #had sex with men
                        ifelse(SXQ410 < 7777 & SXQ410 > 0, 1, NA))) %>% #didn't have sex with men
  mutate(newwsw = ifelse(hetero == 1 | wsexw == 0, 0, #hetero or no women sex
                         ifelse(orient %in% c(2,3) | wsexw == 1, 1, NA))) #self labeled lesbian/gay/bi or sex with m

#put back together
male <- rbind(male07, male0911)

#now put back together overall
dat <- rbind(male, fem)
rm(male)
rm(fem)
rm(fem07)
rm(fem0911)
rm(male07)
rm(male0911)
#########################################################################
#marijuana use
dat <- dat %>%
  mutate(mjever = ifelse(DUQ200 == 1, 1,
                         ifelse(DUQ200 == 2, 0, NA))) %>%
  mutate(momjuse = ifelse(DUQ211 == 1, 1,
                          ifelse(DUQ211 == 2 | mjever == 0, 0, NA))) %>%
  mutate(mjuse4 = ifelse(DUQ217 == 5, 3,
                         ifelse(DUQ217 %in% c(1,2,3,4), 2,
                                ifelse(mjever ==1 & momjuse != . & !DUQ217 %in% c(7,9), 1,
                                       ifelse(mjever == 0, 0, NA))))) %>%
  mutate(dailymjuse = ifelse(mjuse4 == 3, 1,
                             ifelse(mjuse4 %in% c(0,1,2), 0, NA)))

#illicit drugs
dat <- dat %>%
  mutate(illdruguse = ifelse(DUQ240 == 2, 0,
                             ifelse(DUQ240 == 1, 1, NA)))

#alcohol
dat <- dat %>%
  mutate(everdrink12 = ifelse(ALQ101 == 1 | ALQ110 == 1, 1,
                              ifelse(ALQ101 == 2 & ALQ110 == 2, 0, NA))) %>%
  mutate(drink12mo = ifelse(ALQ120Q == 0 | everdrink12 == 0, 0,
                            ifelse(ALQ120U %in% c(1,2,3), 1, NA))) %>%
  mutate(drinking = ifelse(ALQ141U == 1, ALQ141Q * 4.3,
                           ifelse(ALQ141U ==2, ALQ141Q,
                                  ifelse(ALQ141U == 3, ALQ141Q/12, NA)))) %>%
  mutate(alcohol = ifelse(drinking <5 & drinking >=1, 2,
                          ifelse(drinking >=5, 3,
                                 ifelse(drink12mo == 1, 1,
                                        ifelse(drinking < 1 | !is.na(everdrink12), 0, NA)))))

foreign::write.dta(dat, "data\\nhanes30.dta")
plot(dat$RIDAGEYR, dat$age4)
##########################################################################
#create a survey sample design by the primary sampling unit, the stratum, and the revised cycle weight
#(for combining surveys from 5, 4, 3, 2, and 1) cycles

dclus3 <-survey::svydesign(id=~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights=~WTMEC6YR, 
                           nest = TRUE, 
                           data=dat)


nhanes.2007.to.2012 <- dat[!dat$cycle %in% "2013-2014",]
########################################################
#now to see the xtabs showing rlship btwn weight perception bx

#first write the csv of this data
write.csv(dat, "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019nhanes.csv")
write.csv(nhanes.2007.to.2012, "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019no2013.csv")
dat <- readr::read_csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019nhanes.csv")

#########################################################################
#Cross tabs of each of the weight behavior variables, separated out by sex
male <- nhanes.2007.to.2012[nhanes.2007.to.2012$Male == 1,]
female <- nhanes.2007.to.2012[nhanes.2007.to.2012$Male == 0,]

allXtabs <- function(data){
  v1 <- data$LikeToWeigh
  v2 <- data$ConsiderWt
  v3 <- data$doingAbtWt
  listVec <- list(v1, v2, v3)
  nameVec <- c("likeToWeigh", "considerWt", "weightAction")
  for (i in 1:(length(listVec)-1)){
    for (j in (i+1):length(listVec)){
      if (i==j) break
      print(descr::CrossTable(x = listVec[[i]], y = listVec[[j]],
                              format = "SAS", chisq = T, 
                              dnn = c(nameVec[i], nameVec[j]),
                              missing.include = F, prop.chisq = F))
    }
  }
}


allXtabs(female)
allXtabs(male)

#to prepare this for LCA, will need to recode variables so they go
#from 1-> number of categories (ie 1,2,3 vs -1, 0, 1)

#to make data more manageable, select subset of desired variables
#which may be used in the LCA
lcaSub <- nhanes.2007.to.2012 %>%
  select(Male, BMIcat, depression, phq9, depressionBinary,
         smkStat, Race, FSDHH, fsWithHunger, kidFScont,
         adFScont, hhFScont, tryNotGain, ConsiderWt,
         LikeToWeigh, intentional, lastYrLose, lastYrLaxVom,
         lastYrAteLess, lastYrSwitchFood, lastYrChangeDiet,
         lastYrAteLessJunk, lastYrLiquidDiet, lastYrExercise,
         lastYrLessFat, lastYrAteLessSweet, lastYrDietFood,
         lastYrDietPill, lastYrSpecDiet, lastYrFruitVeg,
         lastYrSkipMeal, lastYrJoinProgram, lastYrOthRx,
         lastYrMoreH20, lastYrLowCarb, lastYrRestartSmoke,
         lastYrOther, RIDAGEYR, Income, bingeDrk, doingAbtWt,
         WTMEC2YR, WTMEC6YR, WTMEC4YR, SDMVPSU, SDMVSTRA,
         alcohol, orient, wsexw, mjuse4, newwsw, maritalstatus,
         maritalstatus2, age4, edu)

#for 3 categories: add1, then follow up with this add1 again
add1 <- function(x){
  return(x+1)
}

lcaSub <- lcaSub %>%
  mutate_if(.predicate = !(grepl(pattern = "FScont", names(lcaSub))|
                             grepl(pattern = "RIDAGEYR", names(lcaSub))|
                             grepl(pattern = "FSDHH", names(lcaSub)) |
                             grepl(pattern = "Income", names(lcaSub))|
                             grepl(pattern = "doingAbtWt", names(lcaSub))|
                             grepl(pattern = "SDM", names(lcaSub))|
                             grepl(pattern = "WTMEC", names(lcaSub))), 
            .funs = add1) 

#like to Weigh and Consider Wt need 1 more added
lcaSub <- lcaSub %>%
  mutate_if(.predicate = (grepl(pattern = "Consider", names(lcaSub)) |
                            grepl(pattern = "LikeToWeigh", names(lcaSub))), 
            .funs = add1)



lcaSub$BMIcat <- lcaSub$BMIcat -1

#write csv so that it's readable for LCA
write.csv(lcaSub,  "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019lca.csv")
lcaSub <- read.csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019lca.csv")
nhanes.2007.to.2012 <- read.csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019no2013.csv")


# Build Dag ---------------------------------------------------------------

library(ggdag)
library(dagitty)

depDag <- dagify(Dep ~ LC + Sex + Race + Orient + FS + Edu + Age + Marital + Income,
                      LC ~ Sex + Race + Orient + FS + Edu + Age + Income,
                      FS ~ Income,
                      Income ~ Marital + Edu + Race + Income,
                      Edu ~ Race,
                      exposure = "LC",
                      outcome = "Dep")

ggdag(depDag, text = T) + theme_dag()
ggdag_adjustment_set(depDag, text = T, use_labels = F, shadow = TRUE)

fs.diet.sum <-nhanes.2007.to.2012 %>%
  group_by(FSDHH) %>%
  select(contains("lastYr")) %>%
  summarise_all(.funs = ~(sum(. == 1, na.rm =T)/n()))

fs.mg.sum <-nhanes.2007.to.2012 %>%
  group_by(FSDHH) %>%
  select(contains("ng",ignore.case = FALSE)) %>%
  summarise_all(.funs = ~(sum(. == 1, na.rm =T)/n())) %>%
  select(c(1, 4:22))

#food insecurity with/without hunger
fs.hun.diet.sum <-nhanes.2007.to.2012 %>%
  group_by(fsWithHunger) %>%
  select(contains("lastYr")) %>%
  summarise_all(.funs = ~(sum(. == 1, na.rm =T)/n()))

fs.hun.ng.sum <-nhanes.2007.to.2012 %>%
  group_by(fsWithHunger) %>%
  select(contains("ng",ignore.case = FALSE)) %>%
  summarise_all(.funs = ~(sum(. == 1, na.rm =T)/n())) %>%
  select(c(1, 4:22))

#continuous measure of food insecurity
fs.cont.diet.sum <-nhanes.2007.to.2012 %>%
  group_by(hhFScont) %>%
  select(contains("lastYr")) %>%
  summarise_all(.funs = ~(sum(. == 1, na.rm =T)/n()))

fs.cont.ng.sum <-nhanes.2007.to.2012 %>%
  group_by(hhFScont) %>%
  select(contains("ng",ignore.case = FALSE)) %>%
  summarise_all(.funs = ~(sum(. == 1, na.rm =T)/n())) %>%
  select(c(1, 4:22))

#graph continuous results
ggplot(fs.cont.diet.sum,
       aes(y = lastYrLose, x= hhFScont))+
  geom_point()

ggplot(fs.cont.diet.sum,
       aes(y = lastYrAteLess, x= hhFScont))+
  geom_point()

ggplot(fs.cont.diet.sum,
       aes(y = lastYrDietPill, x= hhFScont))+
  geom_point()

ggplot(fs.cont.diet.sum,
       aes(y = lastYrOthRx, x= hhFScont))+
  geom_point()

ggplot(fs.cont.diet.sum,
       aes(y = lastYrLaxVom, x= hhFScont))+
  geom_point()

names(dat) <- str_remove_all(names(dat), pattern = "_")
dat$fsAny <- ifelse(dat$fsWithHunger %in% c(1,2),1,0)
dat$obese <- ifelse(dat$BMIcat %in% c(4,5,6),1,0)

names(dat)
used <- dat %>%
  select(fsAny, fsWithHunger, Race, RIDAGEYR, Male, doingAbtWt,
         ConsiderWt, LikeToWeigh, depression, depressionBinary, edu, BMXBMI,
         BMIcat, WTMEC10YR, SDMVPSU, SDMVSTRA, SEQN, Income, cycle,
         RIAGENDR, phq9, obese)

#write to dta for stata analysis
foreign::write.dta(used, "data\\nhanes2.dta")
used <- haven::read_dta("data\\nhanes2.dta")
table(used$cycle)

table(dat$BMIcat, dat$fsWithHunger)
prop.table(table(dat$BMIcat, dat$fsWithHunger),2)
xtabs(~dat$BMIcat+dat$fsWithHunger+ dat$Race)

#plot how obesity*food insecurity has changed over time
dat %>%
  ggplot(aes(x= cycle.x.1)) + geom_bar(stat = "count")+ facet_grid(BMIcat~fsAny)

dat %>%
  ggplot(aes(x= cycle.x.1, fill = factor(obese))) + 
  geom_bar(position = "fill")+
  facet_grid(~fsAny)

#redo polychoric correlations
polycor::polychor(x = dat$considerWeightFact, y = dat$likeToWeighFact, std.err = T)
polycor::polychor(x = dat$considerWeightFact, y = dat$doingAbtWt, std.err = T)
polycor::polychor(x = dat$considerWeightFact, y = dat$BMIcat, std.err = T)
polycor::polychor(x = dat$considerWeightFact, y = dat$fsAny, std.err = T)
polycor::polychor(x = dat$likeToWeighFact, y = dat$doingAbtWt, std.err = T)
polycor::polychor(x = dat$likeToWeighFact, y = dat$BMIcat, std.err = T)
polycor::polychor(x = dat$likeToWeighFact, y = dat$fsAny, std.err = T)
polycor::polychor(x = dat$BMIcat, y = dat$doingAbtWt, std.err = T)
polycor::polychor(x = dat$BMIcat, y = dat$fsAny, std.err = T)
polycor::polychor(x = dat$fsAny, y = dat$doingAbtWt, std.err = T)

table(dat$considerWeightFact, dat$likeToWeighFact)
table(dat$considerWeightFact, dat$doingAbtWt)
table(dat$likeToWeighFact, dat$doingAbtWt)
