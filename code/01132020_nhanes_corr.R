#######################################################
# Author: Sarah Van Alsten                            #
# Date Created: January 13, 2020                      #
# Purpose: Correlations and individual models for wt  #
# perception in NHANES                                #
# Data Used: NHANES 07, 09, 11                        #
# Packages: tidyverse, MASS, readr, corrplot, descr   #
# Last Update: January 13, 2020                       #
#######################################################
library(descr)
library(tidyverse)
library(MASS)
library(corrplot)

# Data Management ---------------------------------------------------------

#read in the data
dat <- readr::read_csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019nhanes.csv")

#check that appropriate years used
table(dat$cycle)


# Correlations/Crosstabs ------------------------------------------------------------
#First, just do the crosstabs (without survey adjustment)
CrossTable(dat$BMIcat, dat$LikeToWeigh, chisq = T)
CrossTable(dat$BMIcat, dat$doingAbtWt, chisq = T)
CrossTable(dat$BMIcat, dat$ConsiderWt, chisq = T)
CrossTable(dat$LikeToWeigh, dat$doingAbtWt, chisq = T)
CrossTable(dat$LikeToWeigh, dat$ConsiderWt, chisq = T)
CrossTable(dat$doingAbtWt, dat$ConsiderWt, chisq = T)


#kendall's tau: like spearman but allows for ties
cor.test(dat$BMIcat, dat$LikeToWeigh, method = "kendall")

