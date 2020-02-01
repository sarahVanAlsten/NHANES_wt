#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: January 24, 2020                                                #
# Purpose: Datamanagement to recode diet behavior and weight for food security  #
# and weight perception in NHANES participants                                  #
# Packages Used: tidyverse, RNHANES, survey, tableone                           #
# Data Used: NHANES 2005, 2007, 2009, 2011                                      #
# Last Update: JAN 24, 2020                                                     #
#################################################################################

#load data
dat <- read.csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\12092019nhanes.csv")


#load packages
library(tidyverse)
library(nnet)
library(survey)
library(RNHANES)

dietRecode <- function(var){
  
  #nhanes used diff numbers to match to each diet bx. have function figure out right number to match

   #create frame with letters and numbers corresponding
   frame.my <- as.data.frame(cbind(LETTERS[1:20],
                                   c(10, 11, 12, 13, 14, 15, 16, 17, 31, 32, 33, 40, 
                                     34, 30, 41, 42, 43, 44, 45, 46)))
 
   #get the letter in the variable name that we need, then name the dataframe so
   #we can get access to the LETTER column
   letter <- substr(var,7,7)
   names(frame.my) <- c("V1", "match")
  
   #figure out which number we need to check against
   number <- frame.my[frame.my$V1 == letter, "match"]

  #do the recode  
  return(as.vector(ifelse(dat$lastYrLose == 0, 0,
         ifelse(is.na(dat[,names(dat) == var]), 0,
                ifelse(dat[,names(dat) == var] == as.numeric(as.character(number)), 1, NA)))))
}

#recode the diet bx
dat$lastYrAteLess <- dietRecode("WHD080A")
dat$lastYrSwitchFood <- dietRecode("WHD080B")
dat$lastYrLessFat <- dietRecode("WHD080C")
dat$lastYrExercise <- dietRecode("WHD080D")
dat$lastYrSkipMeal <- dietRecode("WHD080E")
dat$lastYrDietFood <- dietRecode("WHD080F")
dat$lastYrLiquidDiet <- dietRecode("WHD080G")
dat$lastYrJoinProgram <- dietRecode("WHD080H")
dat$lastYrDietPill <- dietRecode("WHD080I")
dat$lastYrOthRx <- dietRecode("WHD080J")
dat$lastYrLaxVom <- dietRecode("WHD080K")
dat$lastYrOther<- dietRecode("WHD080L")
dat$lastYrMoreH20 <- dietRecode("WHD080M")
dat$lastYrSpecDiet <- dietRecode("WHD080N")
dat$lastYrLowCarb <- dietRecode("WHD080O")
dat$lastYrRestartSmoke <- dietRecode("WHD080P")
dat$lastYrChangeDiet <- dietRecode("WHD080R")
dat$lastYrAteLessSweet <- dietRecode("WHD080S")
dat$lastYrAteLessJunk <- dietRecode("WHD080T")
dat$lastYrFruitVeg <- dietRecode("WHD080Q")


#summarise the number of people doing various weight loss/ try not to gain behaviors
loseBx <- dat %>%
  filter(lastYrLose == 1) %>%
  #group by Race and Sex %>%
  group_by(Race, Male) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)))

loseBxFS <- dat %>%
  filter(lastYrLose == 1) %>%
  #group by Race and Sex %>%
  drop_na(fsWithHunger) %>%
  mutate(FSany = ifelse(fsWithHunger %in%c (1,2), 1, ifelse(fsWithHunger == 0, 0, NA)))%>%
  group_by(Race, Male, FSany) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)))

#also get %
loseBxPerc <- dat %>%
  filter(lastYrLose == 1) %>%
  #group by Race and Sex %>%
  group_by(Race, Male) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)/
                                                     (sum(. == 0, na.rm = T)+sum(. == 1, na.rm=T))))

loseBxFSPerc <- dat %>%
  filter(lastYrLose == 1) %>%
  #group by Race and Sex %>%
  drop_na(fsWithHunger) %>%
  mutate(FSany = ifelse(fsWithHunger %in%c (1,2), 1, ifelse(fsWithHunger == 0, 0, NA)))%>%
  group_by(Race, Male, FSany) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)/
                                                    (sum(. == 0, na.rm = T)+sum(. == 1, na.rm=T))))


##############################################
#how do the weight loss bx vary by weignt perception/like to
#(can't do doing abt wt because that's baked into who gets asked diet q's)

loseBxPercep <- dat %>%
  filter(lastYrLose == 1) %>%
  drop_na(consid)%>%
  #group by Race and Sex %>%
  group_by(Race, Male, consid) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)))

loseBxPercepPerc <- dat %>%
  filter(lastYrLose == 1) %>%
  drop_na(consid)%>%
  #group by Race and Sex %>%
  group_by(Race, Male, consid) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)/
                                                     (sum(. == 0, na.rm = T)+sum(. == 1, na.rm=T))))

#drop the people who think too thin: too few

loseBxPercepPerc2 <- dat %>%
  filter(lastYrLose == 1) %>%
  filter(!consid=="too thin") %>%
  drop_na(consid)%>%
  #group by Race and Sex %>%
  group_by(Race, Male, consid) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)/
                                                     (sum(. == 0, na.rm = T)+sum(. == 1, na.rm=T))))



ngRecode <- function(var){
  
  #nhanes used diff numbers to match to each not gain bs have function figure out right number to match
  
  #create frame with letters and numbers corresponding
  frame.my <- as.data.frame(cbind(LETTERS[1:20],
                                  c(10, 11, 12, 13, 14, 15, 16, 17,
                                    31, 32, 33, 33, 34, #i j k l m
                                    30, 41, 42, 43, 44, 45, 46))) #N o
  
  #get the letter in the variable name that we need, then name the dataframe so
  #we can get access to the LETTER column
  letter <- substr(var,7,7)
  names(frame.my) <- c("V1", "match")
  
  #figure out which number we need to check against
  number <- frame.my[frame.my$V1 == letter, "match"]
  
  #do the recode  
  return(as.vector(ifelse(dat$tryNotGain == 0, 0,
                          ifelse(is.na(dat[,names(dat) == var]), 0,
                                 ifelse(dat[,names(dat) == var] == as.numeric(as.character(number)), 1, NA)))))
}


#recode the diet bx
dat$ngAteLess <- ngRecode("WHD100A")
dat$ngLowCal <- ngRecode("WHD100B")
dat$ngLessFat <- ngRecode("WHD100C")
dat$ngExercise <- ngRecode("WHD100D")
dat$ngSkipMeal <- ngRecode("WHD100E")
dat$ngDietFood <- ngRecode("WHD100F")
dat$ngLiquidDiet <- ngRecode("WHD100G")
dat$ngJoinProgram <- ngRecode("WHD100H")
dat$ngDietPill <- ngRecode("WHD100I")
dat$ngOthRx <- ngRecode("WHD100J")
dat$ngLaxVom <- ngRecode("WHD100K")
dat$ngOther<- ngRecode("WHD100L")
dat$ngMoreH20 <- ngRecode("WHD100M")
dat$ngSpecDiet  <- ngRecode("WHD100N")
dat$ngLowCarb<- ngRecode("WHD100O")
dat$ngRestartSmoke <- ngRecode("WHD100P")
dat$ngFruitVeg <- ngRecode("WHD100Q")
dat$ngChangeDiet <- ngRecode("WHD100R")
dat$ngAteLessSweet <- ngRecode("WHD100S")
dat$ngAteLessJunk <- ngRecode("WHD100T")

table(dat$WHD100G, useNA = "ifany")
#summarise the number of people doing various weight loss/ try not to gain behaviors
ngBx <- dat %>%
  filter(tryNotGain== 1) %>%
  #group by Race and Sex %>%
  #group_by(Race, Male) %>%
  summarise_at(vars(contains("ng", ignore.case = FALSE)), .funs = ~(sum(. == 1, na.rm = T)))

#also get %
ngBxPerc <- dat %>%
  filter(tryNotGain == 1) %>%
  #group by Race and Sex %>%
  group_by(Race, Male) %>%
  summarise_at(vars(contains("ng", ignore.case = FALSE)), .funs = ~(sum(. == 1, na.rm = T)/
                                                     (sum(. == 0, na.rm = T)+sum(. == 1, na.rm=T))))

#want to see ALL the behaviors that any one partcipant endorsed.
#do so in a similar fashion; making a composite variable with letters corresponding to
#eveything that person did. See 1) how many letters are endorsed and 2) what 'patters' just
#visually come up.

dat$all_not_gain <- ""

ngAllBx<- function(data){
  
  
  #create frame with letters and numbers corresponding
  frame.my <- as.data.frame(cbind(paste0("WHD100", LETTERS[1:20]),
                                  c(10, 11, 12, 13, 14, 15, 16, 17,
                                    31, 32, 33, 33, 34, #i j k l m
                                    30, 41, 42, 43, 44, 45, 46))) #N o
  
#name the frame
  names(frame.my) <- c("V1", "match")
  
  #iterate over each letter combo, and each nhanes row
  for (i in 1:nrow(frame.my)){
    data$all_not_gain <- 
      ifelse(data[,names(data) == frame.my$V1[i]] == frame.my$match[i], #if number matches that letter's code
       paste0(data$all_not_gain, frame.my$V1[i]),
       data$all_not_gain) #paste in to growing list
  }

  return(data)

}

test.dat <- ngAllBx(dat)
table(test.dat$all_not_gain)


table(dat$tryNotGain)
