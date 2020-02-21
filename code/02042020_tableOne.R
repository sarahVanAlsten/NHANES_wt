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

#variables I will probably want for descriptives:
#Age, Sex, Race, BMI(cat), Current Smoking, Depression, MJ use