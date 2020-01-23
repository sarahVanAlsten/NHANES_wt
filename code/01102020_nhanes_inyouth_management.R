#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: January 10 2020                                                 #
# Purpose: Datamanagement to recode diet behavior and weight for food security  #
# and weight perception in NHANES participants                                  #
# Packages Used: tidyverse, RNHANES, survey, tableone                           #
# Data Used: NHANES 2005, 2007, 2009, 2011, 2003, 2013                          #
# Last Update: Jan 10, 2019                                                     #
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
whq05 <- nhanes_load_data("WHQ", "2005-2006", demographics = T)

whq <- plyr::rbind.fill(whq09, whq11, whq13, whq07, whq05)

#clean up workspce
rm(whq05)
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
fsq05 <- nhanes_load_data("FSQ", "2005-2006", demographics = F)

#put all the data into one frame
fsq <- plyr::rbind.fill(fsq09, fsq11, fsq13,fsq07, fsq05)

#clean up workspce
rm(fsq05)
rm(fsq09)
rm(fsq11)
rm(fsq13)
rm(fsq07)
#############################################################################################################
#youth weight history
yw09 <- nhanes_load_data("WHQMEC", "2009-2010", demographics = T)
yw11 <- nhanes_load_data("WHQMEC", "2011-2012", demographics = T)
yw13 <- nhanes_load_data("WHQMEC", "2013-2014", demographics = T)
yw07 <- nhanes_load_data("WHQMEC", "2007-2008", demographics = T)
yw05 <- nhanes_load_data("WHQMEC", "2005-2006", demographics = T)

#put all the data into one frame
yw <- plyr::rbind.fill(yw09, yw11, yw13, yw07, yw05)

#clean up workspce
rm(yw05)
rm(yw09)
rm(yw11)
rm(yw13)
rm(yw07)
#########################################################################################################
#diet behavior
dbq09 <- nhanes_load_data("dbq", "2009-2010", demographics = F)
dbq11 <- nhanes_load_data("dbq", "2011-2012", demographics = F)
dbq13 <- nhanes_load_data("dbq", "2013-2014", demographics = F)
dbq07 <- nhanes_load_data("dbq", "2007-2008", demographics = F)
dbq05 <- nhanes_load_data("dbq", "2005-2006", demographics = F)

#put all the data into one frame
dbq <- plyr::rbind.fill(dbq09, dbq11, dbq13, dbq07, dbq05)

#clean up workspce
rm(dbq05)
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
duq05 <- nhanes_load_data("duq", "2005-2006", demographics = F)

#put all the data into one frame
duq <- plyr::rbind.fill(duq09, duq11, duq13, duq07, duq05)

#clean up workspce
rm(duq09)
rm(duq11)
rm(duq13)
rm(duq07)
rm(duq05)
###########################################################
##Depression Screen
dpq09 <- nhanes_load_data("dpq", "2009-2010", demographics = F)
dpq11 <- nhanes_load_data("dpq", "2011-2012", demographics = F)
dpq13 <- nhanes_load_data("dpq", "2013-2014", demographics = F)
dpq07 <- nhanes_load_data("dpq", "2007-2008", demographics = F)
dpq05 <- nhanes_load_data("dpq", "2005-2006", demographics = F)

#put all the data into one frame
dpq <- plyr::rbind.fill(dpq09, dpq11, dpq13, dpq07, dpq05)

#clean up workspce
rm(dpq09)
rm(dpq11)
rm(dpq13)
rm(dpq07)
rm(dpq05)
#############################################################
#Smoking
#########################################################################################################
smq09 <- nhanes_load_data("smq", "2009-2010", demographics = F)
smq11 <- nhanes_load_data("smq", "2011-2012", demographics = F)
smq13 <- nhanes_load_data("smq", "2013-2014", demographics = F)
smq07 <- nhanes_load_data("smq", "2007-2008", demographics = F)
smq05 <- nhanes_load_data("smq", "2005-2006", demographics = F)

#put all the data into one frame
smq <- plyr::rbind.fill(smq09, smq11, smq13, smq07, smq05)

#clean up workspce
rm(smq09)
rm(smq11)
rm(smq13)
rm(smq07)
rm(smq05)
#################################################################
#ALCOHOL
#########################################################################################################r
alq09 <- nhanes_load_data("alq", "2009-2010", demographics = F)
alq11 <- nhanes_load_data("alq", "2011-2012", demographics = F)
alq13 <- nhanes_load_data("alq", "2013-2014", demographics = F)
alq07 <- nhanes_load_data("alq", "2007-2008", demographics = F)
alq05 <- nhanes_load_data("alq", "2005-2006", demographics = F)

#put all the data into one frame
alq <- plyr::rbind.fill(alq09, alq11, alq13, alq07, alq05)

#clean up workspce
rm(alq09)
rm(alq11)
rm(alq13)
rm(alq07)
rm(alq05)
##########################################
#ACCULTURATION
#########################################################################################################
acq09 <- nhanes_load_data("acq", "2009-2010", demographics = F)
acq11 <- nhanes_load_data("acq", "2011-2012", demographics = F)
acq13 <- nhanes_load_data("acq", "2013-2014", demographics = F)
acq07 <- nhanes_load_data("acq", "2007-2008", demographics = F)
acq05 <- nhanes_load_data("acq", "2005-2006", demographics = F)

#put all the data into one frame
acq <- plyr::rbind.fill(acq09, acq11, acq13, acq07, acq05)

#clean up workspce
rm(acq09)
rm(acq11)
rm(acq13)
rm(acq07)
rm(acq05)
##########################################
#BODY MEASURES
#########################################################################################################
bmx09 <- nhanes_load_data("bmx", "2009-2010", demographics = F)
bmx11 <- nhanes_load_data("bmx", "2011-2012", demographics = F)
bmx13 <- nhanes_load_data("bmx", "2013-2014", demographics = F)
bmx07 <- nhanes_load_data("bmx", "2007-2008", demographics = F)
bmx05 <- nhanes_load_data("bmx", "2005-2006", demographics = F)

#put all the data into one frame
bmx <- plyr::rbind.fill(bmx09, bmx11, bmx13, bmx07, bmx05)

#clean up workspce
rm(bmx09)
rm(bmx11)
rm(bmx13)
rm(bmx07)
rm(bmx05)
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
rm(sxq05)
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
rm(hsq05)
###############################################################################
#Merge data by respondent ID to get one data frame
dat <-merge(merge(
  merge(
    merge(
      merge(
        merge(
          merge(
            merge(
              merge(
                merge(
                  whq, smq, by = "SEQN", all = T),
                fsq, by = "SEQN", all = T),
              duq, by = "SEQN", all = T), 
            dpq, by = "SEQN", all = T),
          bmx, by = "SEQN", all = T),
        alq, by = "SEQN", all = T),
      acq, by = "SEQN", all = T),
    hsq, by = "SEQN", all = T),
  sxq, by = "SEQN", all = T))

#get rid of unnecessary repeated columns
dat <- dat[,!names(dat)%in% c("file_name.x", "file_name.y", "end_year.y", "end_year.x", "begin_year.x", 
                              "begin_year.y", "begin_year.y.2", "begin_year.x.2", 
                              "begin_year.x.3", "begin_year.y.3", "begin_year.x.1", "begin_year.y.1",
                              "cycle.x.3", "cycle.y.3", "cycle.y.1", "cycle.x.2", "cycle.x.1",
                              "cycle.y", "cycle.x.4"),]

#check the merging
names(dat)

#######################################################################################
#recode variables: Demographics
dat <- dat %>%
  mutate(Income = ifelse(INDFMPIR.x <=1, 1,
                         ifelse(INDFMPIR.x <=2,2,
                                ifelse(INDFMPIR.x <=3, 3, 
                                       ifelse(INDFMPIR.x  <=4, 4,
                                              ifelse(INDFMPIR.x < 5, 5,
                                                     ifelse(INDFMPIR.x ==5, 6, NA))))))) %>%
  mutate(Race = ifelse(RIDRETH1.x %in% c(1,2), 2, #hispanic
                       ifelse(RIDRETH1.x == 3,0, #white
                              ifelse(RIDRETH1.x == 4, 1, #black
                                     ifelse(RIDRETH1.x == 5, 3,NA)))))  %>% #other
  mutate(Male = ifelse(RIAGENDR.x == 1, 1,
                       ifelse(RIAGENDR.x == 2,0, NA)))


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
  mutate(WTMEC8YR = WTMEC2YR.x / 4,
         WTMEC6YR = WTMEC2YR.x / 3,
         WTMEC4YR = WTMEC2YR.x /2)

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
                           ifelse(ALQ140Q == 0, 0,
                                  ifelse(ALQ140Q > 1 & ALQ140Q < 370, 1, NA))))

table(dat$bingeDrk)


########################################################################
#Smoking
dat <-
  dat %>%
  mutate(smoke100 = ifelse(SMQ020 %in% c(9,7,NA), NA,
                           ifelse(SMQ020 == 1, 1,
                                  ifelse(SMQ020 == 2, 0, NA)))) %>%
  mutate(nowSmoke = ifelse(SMQ040 %in% c(9,7, NA), NA,
                           ifelse(SMQ040 == 3, 0, 1))) %>%
  mutate(smkStat = ifelse(nowSmoke == 1, 2, #current
                          ifelse(nowSmoke == 0 & smoke100 == 0, 0, #never
                                 ifelse(nowSmoke == 0 & smoke100 == 1, 1, NA)))) #former
########################################################################
#Age category, marital status, and education
dat <-
  dat %>%
  mutate(age4 = ifelse(RIDAGEYR.x > 20 & RIDAGEYR.x <= 29, 1,
                       ifelse(RIDAGEYR.x > 29 & RIDAGEYR.x <= 39, 2,
                              ifelse(RIDAGEYR.x > 39 & RIDAGEYR.x <= 49, 3,
                                     ifelse(RIDAGEYR.x > 49 & RIDAGEYR.x <= 59, 4, NA)))))
#NOTE: I changed this so that all people who were out of age range for the sexual orientation questions
#were coded 2;
dat <-
  dat %>%
  mutate(age = ifelse(age4 %in% c(1,2), 0,
                      ifelse(age4 %in% c(3,4), 1, 2)))

dat <-
  dat %>%
  mutate(maritalstatus = ifelse(DMDMARTL.x == 1, 0,
                                ifelse(DMDMARTL.x == 5, 1,
                                       ifelse(DMDMARTL.x == 6, 2,
                                              ifelse(DMDMARTL.x %in% c(2,3,4), 3, NA))))) %>%
  mutate(maritalstatus2 = ifelse(maritalstatus %in% c(0,1), 0,
                                 maritalstatus - 2))
dat <-
  dat %>%
  mutate(edu = ifelse(DMDEDUC2.x %in% c(1,2), 0,
                      ifelse(DMDEDUC2.x == 3, 1,
                             ifelse(DMDEDUC2.x %in% c(4,5), 2, NA))))

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
  filter(cycle.x %in% c("2007-2008", "2005-2006"))

fem0911 <- fem %>%
  filter(!cycle.x %in% c("2007-2008", "2005-2006"))

male07 <- male %>%
  filter(cycle.x %in% c("2007-2008", "2005-2006"))

male0911 <- male %>%
  filter(!cycle.x %in% c("2007-2008", "2005-2006"))

#for female 09-11: follow julia's code
#never had any sex: vaginal, anal, oral M or W
fem0911 <- fem0911 %>%
  mutate(neversex = ifelse(SXQ700 != 1 & SXQ703 !=1 & SXQ706 !=1 & SXQ709 != 1, 1, 0))

fem0911 <- fem0911 %>%
  mutate(hetero = ifelse(SXQ294 == 1, 1,
                         ifelse(SXQ294 %in% c(2,3,4,5,9), 0, NA))) %>%
  mutate(orient = ifelse(hetero == 1, 1, #hetero
                         ifelse(SXQ294 == 2, 2, #lesbian/gay
                                ifelse(SXQ294 == 3, 3, #bisexual
                                       ifelse(SXQ294 %in% c(5,7,9), NA, 4)))))%>%#something else
  mutate(wsexw = ifelse(SXQ709 == 1, 1, #had sex with women
                        ifelse(SXQ709 == 2, 0, NA))) %>% #didn't have sex with women
  mutate(newwsw = ifelse(hetero == 1 | wsexw == 0, 0, #hetero or no women sex
                         ifelse(orient %in% c(2,3) | wsexw == 1, 1, NA))) #self labeled lesbian/gay/bi or sex with w

fem07 <- fem07 %>%
  mutate(hetero = ifelse(SXQ294 == 1, 1,
                         ifelse(SXQ294 %in% c(2,3,4,5,9), 0, NA))) %>%
  mutate(orient = ifelse(hetero == 1, 1, #hetero
                         ifelse(SXQ294 == 2, 2, #lesbian/gay
                                ifelse(SXQ294 == 3, 3, #bisexual
                                       ifelse(SXQ294 %in% c(5,7,9), NA, 4)))))%>% #something else
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
                         ifelse(SXQ292 == 2, 2, #gay
                                ifelse(SXQ292 == 3, 3, #bisexual
                                       ifelse(SXQ292 %in% c(5,7,9), NA, 4)))))%>% #something else
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
                         ifelse(SXQ292 == 2, 2, #lesbian/gay
                                ifelse(SXQ292 == 3, 3, #bisexual
                                       ifelse(SXQ292 %in% c(5,7,9), 4, NA)))))%>% #something else
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
  mutate(mjuse4 = ifelse(mjever == 0, 0,
                         ifelse(DUQ217 == 5, 3,
                          ifelse(DUQ217 %in% c(1,2,3,4), 2,
                                ifelse(mjever == 1 & !is.na(momjuse) & !DUQ217 %in% c(7,9), 1,
                                        NA))))) %>%
  mutate(dailymjuse = ifelse(mjuse4 == 3, 1,
                             ifelse(mjuse4 %in% c(0,1,2), 0, NA)))
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
#########################################################
#Weight perceptions for youth weights
yw2 <- merge(yw, bmx, by = "SEQN")

yw2 <- yw2 %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==2 & RIAGENDR==1 & BMXBMI < 14.8, 1,
                         ifelse(RIDAGEYR == 2 &RIAGENDR==1 & BMXBMI  < 18.2, 2,
                                ifelse(RIDAGEYR == 2 & RIAGENDR==1 &BMXBMI < 18.6, 3,
                                       ifelse(RIDAGEYR == 2 & RIAGENDR==1 &!is.na(BMXBMI), 4, NA))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==3 &RIAGENDR==1 & BMXBMI < 14.3, 1,
                         ifelse(RIDAGEYR == 3 &RIAGENDR==1 & BMXBMI < 17.4, 2,
                                ifelse(RIDAGEYR == 3 &RIAGENDR==1 & BMXBMI < 18.2, 3,
                                       ifelse(RIDAGEYR == 3 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==4 &RIAGENDR==1 & BMXBMI < 14, 1,
                         ifelse(RIDAGEYR == 4 &RIAGENDR==1 & BMXBMI < 16.9, 2,
                                ifelse(RIDAGEYR == 4 &RIAGENDR==1 & BMXBMI < 18.8, 3,
                                       ifelse(RIDAGEYR == 4 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==5 &RIAGENDR==1 & BMXBMI < 13.8, 1,
                         ifelse(RIDAGEYR == 5 & RIAGENDR==1 &BMXBMI < 16.8, 2,
                                ifelse(RIDAGEYR == 5 &RIAGENDR==1 & BMXBMI < 18, 3,
                                       ifelse(RIDAGEYR == 5 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==6 &RIAGENDR==1 & BMXBMI < 13.8, 1,
                         ifelse(RIDAGEYR == 6 &RIAGENDR==1 & BMXBMI < 17, 2,
                                ifelse(RIDAGEYR == 6 &RIAGENDR==1 & BMXBMI < 18.4, 3,
                                       ifelse(RIDAGEYR == 6 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==7 &RIAGENDR==1 & BMXBMI < 13.8, 1,
                         ifelse(RIDAGEYR == 7 &RIAGENDR==1 & BMXBMI < 18.2, 2,
                                ifelse(RIDAGEYR == 7 &RIAGENDR==1 & BMXBMI < 19.1, 3,
                                       ifelse(RIDAGEYR == 7 & RIAGENDR==1 &!is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==8 &RIAGENDR==1 & BMXBMI < 13.8, 1,
                         ifelse(RIDAGEYR == 8 &RIAGENDR==1 & BMXBMI < 17.9, 2,
                                ifelse(RIDAGEYR == 8 &RIAGENDR==1 & BMXBMI < 20, 3,
                                       ifelse(RIDAGEYR == 8 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==9 &RIAGENDR==1 & BMXBMI < 14, 1,
                         ifelse(RIDAGEYR == 9 &RIAGENDR==1 & BMXBMI < 18.6, 2,
                                ifelse(RIDAGEYR == 9 & RIAGENDR==1 &BMXBMI < 21, 3,
                                       ifelse(RIDAGEYR == 9 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==10 & RIAGENDR==1 &BMXBMI < 14.2, 1,
                         ifelse(RIDAGEYR == 10 & RIAGENDR==1 &BMXBMI < 19.4, 2,
                                ifelse(RIDAGEYR == 10 & RIAGENDR==1 &BMXBMI < 22, 3,
                                       ifelse(RIDAGEYR == 10 & RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==11 &RIAGENDR==1 & BMXBMI < 14.5, 1,
                         ifelse(RIDAGEYR == 11 &RIAGENDR==1 & BMXBMI < 20.2, 2,
                                ifelse(RIDAGEYR == 11 &RIAGENDR==1 & BMXBMI < 23.2, 3,
                                       ifelse(RIDAGEYR == 11 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==12 &RIAGENDR==1 & BMXBMI < 15, 1,
                         ifelse(RIDAGEYR == 12 &RIAGENDR==1 & BMXBMI < 21, 2,
                                ifelse(RIDAGEYR == 12 &RIAGENDR==1 & BMXBMI < 24.2, 3,
                                       ifelse(RIDAGEYR == 12 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR == 13 &RIAGENDR==1 & BMXBMI < 15.5, 1,
                         ifelse(RIDAGEYR == 13 &RIAGENDR==1 & BMXBMI < 21.8, 2,
                                ifelse(RIDAGEYR == 13 &RIAGENDR==1 & BMXBMI < 25.2, 3,
                                       ifelse(RIDAGEYR == 13 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==14 &RIAGENDR==1 & BMXBMI < 16, 1,
                         ifelse(RIDAGEYR == 14 &RIAGENDR==1 & BMXBMI < 22.6, 2,
                                ifelse(RIDAGEYR == 14 &RIAGENDR==1 & BMXBMI < 26, 3,
                                       ifelse(RIDAGEYR == 14 & RIAGENDR==1 &!is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==15 &RIAGENDR==1 & BMXBMI < 16.6, 1,
                         ifelse(RIDAGEYR == 15 &RIAGENDR==1 & BMXBMI < 23.4, 2,
                                ifelse(RIDAGEYR == 15 &RIAGENDR==1 & BMXBMI < 26.8, 3,
                                       ifelse(RIDAGEYR == 15 &RIAGENDR==1 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  #girls#####################################################################################################################
mutate(BMIcat = ifelse(RIDAGEYR ==2 & RIAGENDR==2 & BMXBMI < 14.4, 1,
                       ifelse(RIDAGEYR == 2 &RIAGENDR==2 & BMXBMI  < 18, 2,
                              ifelse(RIDAGEYR == 2 & RIAGENDR==2 &BMXBMI < 19, 3,
                                     ifelse(RIDAGEYR == 2 & RIAGENDR==2 &!is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==3 &RIAGENDR==2 & BMXBMI <14 , 1,
                         ifelse(RIDAGEYR == 3 &RIAGENDR==2 & BMXBMI < 17.2, 2,
                                ifelse(RIDAGEYR == 3 &RIAGENDR==2 & BMXBMI < 18.3, 3,
                                       ifelse(RIDAGEYR == 3 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==4 &RIAGENDR==2 & BMXBMI < 13.7, 1,
                         ifelse(RIDAGEYR == 4 &RIAGENDR==2 & BMXBMI < 16.8, 2,
                                ifelse(RIDAGEYR == 4 &RIAGENDR==2 & BMXBMI < 18, 3,
                                       ifelse(RIDAGEYR == 4 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==5 &RIAGENDR==2 & BMXBMI < 13.5, 1,
                         ifelse(RIDAGEYR == 5 & RIAGENDR==2 &BMXBMI < 16.8, 2,
                                ifelse(RIDAGEYR == 5 &RIAGENDR==2 & BMXBMI < 18.2, 3,
                                       ifelse(RIDAGEYR == 5 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==6 &RIAGENDR==2 & BMXBMI <13.4 , 1,
                         ifelse(RIDAGEYR == 6 &RIAGENDR==2 & BMXBMI <17.1, 2,
                                ifelse(RIDAGEYR == 6 &RIAGENDR==2 & BMXBMI <18.8 , 3,
                                       ifelse(RIDAGEYR == 6 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==7 &RIAGENDR==2 & BMXBMI < 13.4, 1,
                         ifelse(RIDAGEYR == 7 &RIAGENDR==2 & BMXBMI < 17.6, 2,
                                ifelse(RIDAGEYR == 7 &RIAGENDR==2 & BMXBMI < 19.6, 3,
                                       ifelse(RIDAGEYR == 7 & RIAGENDR==2 &!is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==8 &RIAGENDR==2 & BMXBMI < 13.5, 1,
                         ifelse(RIDAGEYR == 8 &RIAGENDR==2 & BMXBMI < 18.3, 2,
                                ifelse(RIDAGEYR == 8 &RIAGENDR==2 & BMXBMI < 20.7, 3,
                                       ifelse(RIDAGEYR == 8 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==9 &RIAGENDR==2 & BMXBMI < 13.8, 1,
                         ifelse(RIDAGEYR == 9 &RIAGENDR==2 & BMXBMI < 19.1, 2,
                                ifelse(RIDAGEYR == 9 & RIAGENDR==2 &BMXBMI < 21.8, 3,
                                       ifelse(RIDAGEYR == 9 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==10 & RIAGENDR==2 &BMXBMI < 14, 1,
                         ifelse(RIDAGEYR == 10 & RIAGENDR==2 &BMXBMI <20 , 2,
                                ifelse(RIDAGEYR == 10 & RIAGENDR==2 &BMXBMI < 23, 3,
                                       ifelse(RIDAGEYR == 10 & RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==11 &RIAGENDR==2 & BMXBMI < 14.4, 1,
                         ifelse(RIDAGEYR == 11 &RIAGENDR==2 & BMXBMI < 20.8, 2,
                                ifelse(RIDAGEYR == 11 &RIAGENDR==2 & BMXBMI < 24, 3,
                                       ifelse(RIDAGEYR == 11 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==12 &RIAGENDR==2 & BMXBMI < 14.8, 1,
                         ifelse(RIDAGEYR == 12 &RIAGENDR==2 & BMXBMI < 21.7, 2,
                                ifelse(RIDAGEYR == 12 &RIAGENDR==2 & BMXBMI < 25.2, 3,
                                       ifelse(RIDAGEYR == 12 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR == 13 &RIAGENDR==2 & BMXBMI < 15.3, 1,
                         ifelse(RIDAGEYR == 13 &RIAGENDR==2 & BMXBMI < 22.6, 2,
                                ifelse(RIDAGEYR == 13 &RIAGENDR==2 & BMXBMI < 26.25, 3,
                                       ifelse(RIDAGEYR == 13 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==14 &RIAGENDR==2 & BMXBMI < 15.8, 1,
                         ifelse(RIDAGEYR == 14 &RIAGENDR==2 & BMXBMI < 23.4, 2,
                                ifelse(RIDAGEYR == 14 &RIAGENDR==2 & BMXBMI < 27.2, 3,
                                       ifelse(RIDAGEYR == 14 & RIAGENDR==2 &!is.na(BMXBMI), 4, BMIcat))))) %>%
  mutate(BMIcat = ifelse(RIDAGEYR ==15 &RIAGENDR==2 & BMXBMI < 16.3, 1,
                         ifelse(RIDAGEYR == 15 &RIAGENDR==2 & BMXBMI <24, 2,
                                ifelse(RIDAGEYR == 15 &RIAGENDR==2 & BMXBMI < 28, 3,
                                       ifelse(RIDAGEYR == 15 &RIAGENDR==2 & !is.na(BMXBMI), 4, BMIcat))))) 
#bind in the food security
yw2 <- merge(yw2, fsq, by = "SEQN", all.x =T)

#food security questions
yw2 <-
  yw2 %>%
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
yw2 <-
  yw2 %>%
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
yw2 <- yw2 %>% 
  #rowwise will make sure the sum operation will occur on each row
  rowwise() %>% 
  #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
  mutate(adFScont = sum(FSD032Ar ,FSD032Br,FSD032Cr ,FSD041r ,FSD052r , 
                        FSD061r ,FSD071r ,FSD081r, FSD092r ,FSD102r, na.rm=TRUE))

#Continuous score of childhood food insecurity
yw2 <- yw2 %>%
  rowwise() %>%
  mutate(kidFScont = sum(FSD032Dr, FSD032Er, FSD032Fr, FSD111r, FSD122r, FSD132r, FSD141r, FSD146r, na.rm = TRUE))

#household food insecurity total: continuous
yw2 <- yw2 %>%
  rowwise() %>%
  mutate(hhFScont = sum(adFScont, kidFScont, na.rm =T))

#per nhanes guidelines, to collapse accross year create sample weight that is original
#divided by number of collapsed cycles
yw2 <-
  yw2 %>%
  rowwise() %>%
  mutate(WTMEC8YR = WTMEC2YR / 4,
         WTMEC6YR = WTMEC2YR / 3,
         WTMEC4YR = WTMEC2YR /2)

#make a variable to represent food security with and without hunger
yw2 <- yw2 %>%
  mutate(fsWithHunger = ifelse(FSDHH %in% c(2,3,4) & FSD071 == 2, 2, #with hunger
                               ifelse(FSDHH %in% c(2,3,4), 1, #without hunger
                                      ifelse(FSDHH == 1, 0, NA)))) #not insecure

#how do you consider weight
yw2 <- yw2 %>%
  mutate(considerWt = ifelse(WHQ030M == 1, 2,
                             ifelse(WHQ030M == 2, 0,
                                    ifelse(WHQ030M == 3, 1, NA)))) %>%
  mutate(doAboutWt = ifelse(WHQ500 == 1, 0, #lose
                            ifelse(WHQ500 == 2, 2, #gain
                                   ifelse(WHQ500 == 3, 1, NA)))) #same

#separate into those who said yes to lose wt and not
yw2.lose <- subset(yw2, doAboutWt == 0)
yw2.nolose <- subset(yw2, doAboutWt %in% c(1,2,NA))

#why trying to lose weight
yw2.lose <- yw2.lose %>%
  filter(cycle != "2013-2014") %>%
  mutate(whyLose.look = ifelse(is.na(WHQ510A),1,
                               ifelse(WHQ510A == 10, 2, 1)),
         whyLose.health = ifelse(is.na(WHQ510B),1,
                                 ifelse(WHQ510B == 11, 2, 1)),
         whyLose.sports = ifelse(is.na(WHQ510C),1,
                                        ifelse(WHQ510C == 12, 2, 1)),
         whyLose.tease = ifelse(is.na(WHQ510D),1,
                                       ifelse(WHQ510D == 13, 2, 1)),
         whyLose.clothes = ifelse(is.na(WHQ510E),1,
                                         ifelse(WHQ510E == 14, 2, 1)),
         whyLose.boyslike = ifelse(is.na(WHQ510F),1,
                                          ifelse(WHQ510F == 15, 2, 1)),
         whyLose.girlslike = ifelse(is.na(WHQ510G),1,
                                           ifelse(WHQ510G == 16, 2, 1)),
         whyLose.friendare = ifelse(is.na(WHQ510H),1,
                                           ifelse(WHQ510H == 17, 2, 1)),
         whyLose.familyare = ifelse(is.na(WHQ510I),1,
                                    ifelse(WHQ510I == 18, 2, 1)),
         whyLose.parentWants = ifelse(is.na(WHQ510J),1,
                                      ifelse(WHQ510J == 19, 2, 1)),
         whyLose.coachWants = ifelse(is.na(WHQ510K),1,
                                     ifelse(WHQ510K == 20, 2, 1)),
         whyLose.docWants = ifelse(is.na(WHQ510L),1,
                                   ifelse(WHQ510L == 21, 2, 1)),
         whyLose.amFat = ifelse(is.na(WHQ510P),1,
                                ifelse(WHQ510P == 26, 2, 1)),
         whyLose.feelBetter = ifelse(is.na(WHQ510N),1,
                                     ifelse(WHQ510N == 24, 2, 1)),
         whyLose.other = ifelse(is.na(WHQ510U),1,
                                ifelse(WHQ510U == 30, 2, 1)))

#missing val for these
yw2.nolose <- yw2.nolose %>%
  filter(cycle != "2013-2014") %>%
  mutate(whyLose.look = 3,
         whyLose.health = 3,
         whyLose.sports = 3,
         whyLose.tease = 3,
         whyLose.clothes = 3,
         whyLose.boyslike = 3,
         whyLose.girlslike = 3,
         whyLose.friendare = 3,
         whyLose.familyare = 3,
         whyLose.parentWants = 3,
         whyLose.coachWants = 3,
         whyLose.docWants = 3,
         whyLose.amFat = 3,
         whyLose.feelBetter = 3,
         whyLose.other = 3)

#put back together
yw <- rbind(yw2.lose, yw2.nolose)
names(yw)
yw <- yw %>%
  mutate_at(.vars = 63:68, .funs = ~(ifelse(. == 1, 0,
                                            ifelse(. == 2, 1,
                                                   ifelse(. == 3, 2, NA)))))

yw <- yw %>%
  rename(oftenLstWt =WHQ520,
         oftenDiet = WHQ530,
         oftenStarve = WHQ540,
         oftenCutBack = WHQ550,
         oftenSkip = WHQ560,
         oftenExerc = WHQ570,
         oftenLessSweet = WHQ580)

yw <- yw %>% ungroup()

yw <- yw %>%
  select(contains("often")) %>%
  mutate_all(.funs = na_if, y = 9)

yw <- yw %>%
  mutate_all(.funs = ~(ordered(factor(.x))))

#undo the ordinal
yw <- yw %>%
  mutate_all(.funs = ~(as.numeric(.x)))


#summary
yw.sum <- yw %>%
  summarise(sum(oftenLstWt == 1, na.rm =T)/n(),
            sum(oftenLstWt == 2, na.rm = T)/n(),
            sum(oftenDiet == 1, na.rm =T)/n(),
            sum(oftenDiet == 2, na.rm = T)/n(),
            sum(oftenStarve == 1, na.rm =T)/n(),
            sum(oftenStarve == 2, na.rm = T)/n(),
            sum(oftenCutBack == 1, na.rm =T)/n(),
            sum(oftenCutBack == 2, na.rm = T)/n(),
            sum(oftenSkip == 1, na.rm =T)/n(),
            sum(oftenSkip == 2, na.rm = T)/n(),
            sum(oftenExerc == 1, na.rm =T)/n(),
            sum(oftenExerc == 2, na.rm = T)/n(),
            sum(oftenLessSweet == 1, na.rm =T)/n(),
            sum(oftenLessSweet == 2, na.rm = T)/n())

#summary
yw.sum2 <- yw2.lose %>% ungroup()%>%
  summarise_at(vars(contains("whyLose")), .funs = ~(sum(. == 2, na.rm = T)/ n()))

yw2.lose$considerWt <- yw2.lose$considerWt + 1
yw2.lose$doAboutWt <- yw2.lose$doAboutWt + 1
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
write.csv(yw2.lose, "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\yw2lose.csv")
write.csv(dat, "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\0120dat.csv")


get_AIC_BIC <- function(maxclass = 10, data){
  aicVec <- c(rep(0, maxclass))
  bicVec <- aicVec
  for (i in 2:maxclass){
    lc <- poLCA(cbind(whyLose.look,
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
                data = data,
                nclass = i,
                maxiter = 20000)
    aicVec[i] <- lc$aic
    bicVec[i] <- lc$bic
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC(8, yw2.lose) # 5 classes per BIC. somewhere btwn 5-7

lc <- poLCA(cbind(whyLose.look,
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
            data = yw2.lose,
            nclass = 5,
            maxiter = 20000,
            graphs =T)
summary(lc)

lc6 <- poLCA(cbind(whyLose.look,
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
            data = yw2.lose,
            nclass = 6,
            maxiter = 20000)
summary(lc6) # get a 1% prev class... too small

#split by gender
fem.youth <- subset(yw2.lose, RIAGENDR ==2)
male.youth <- subset(yw2.lose, RIAGENDR !=2)

get_AIC_BIC(8, fem.youth) #3 classes
get_AIC_BIC(8, male.youth) # 3 or 4 classes


lc.f <- poLCA(cbind(whyLose.look,
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
            data = fem.youth,
            nclass = 3,
            maxiter = 20000,
            graphs =T)


lc.m <- poLCA(cbind(whyLose.look,
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
              data = male.youth,
              nclass = 3,
              maxiter = 20000,
              graphs =T)
##############################################################
male.youth$predclass <- lc.m$predclass
fem.youth$predclass <- lc.f$predclass

table(fem.youth$predclass, fem.youth$BMIcat)
prop.table(table(fem.youth$predclass, fem.youth$BMIcat),1)
chisq.test(fem.youth$predclass, fem.youth$BMIcat)

table(male.youth$predclass, male.youth$BMIcat)
prop.table(table(male.youth$predclass, male.youth$BMIcat),1)
chisq.test(male.youth$predclass, male.youth$BMIcat)

table(fem.youth$predclass, fem.youth$FSDHH)
prop.table(table(fem.youth$predclass, fem.youth$FSDHH),1)
chisq.test(fem.youth$predclass, fem.youth$FSDHH)

table(male.youth$predclass, male.youth$FSDHH)
prop.table(table(male.youth$predclass, male.youth$FSDHH),1)
descr::CrossTable(male.youth$predclass, male.youth$FSDHH,
                  chisq = T,fisher = T)

table(fem.youth$predclass, fem.youth$considerWt)
prop.table(table(fem.youth$predclass, fem.youth$considerWt),1)
descr::CrossTable(fem.youth$predclass, fem.youth$considerWt,
                  fisher = T)

table(male.youth$predclass, male.youth$considerWt)
prop.table(table(male.youth$predclass, male.youth$considerWt),1)
descr::CrossTable(male.youth$predclass, male.youth$considerWt,
                  fisher = T)

#do same for actual behavior vs why want to lose
get_AIC_BIC_2 <- function(maxclass = 10, data){
  aicVec <- c(rep(0, maxclass))
  bicVec <- aicVec
  for (i in 2:maxclass){
    lc <- poLCA(cbind(oftenLstWt,
                      oftenDiet,
                      oftenStarve,
                      oftenCutBack,
                      oftenSkip,
                      oftenExerc,
                      oftenLessSweet)~1,
                data = data,
                nclass = i,
                maxiter = 20000)
    aicVec[i] <- lc$aic
    bicVec[i] <- lc$bic
  }
  return(cbind(aicVec, bicVec))
}

get_AIC_BIC_2(8, yw)

