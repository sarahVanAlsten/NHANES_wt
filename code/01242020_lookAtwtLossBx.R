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
  drop_na(ConsiderWt)%>%
  #group by Race and Sex %>%
  group_by(Race, Male, ConsiderWt) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)))

loseBxPercepPerc <- dat %>%
  filter(lastYrLose == 1) %>%
  drop_na(ConsiderWt)%>%
  #group by Race and Sex %>%
  group_by(Race, Male, ConsiderWt) %>%
  summarise_at(vars(contains("lastYr")), .funs = ~(sum(. == 1, na.rm = T)/
                                                     (sum(. == 0, na.rm = T)+sum(. == 1, na.rm=T))))

#drop the people who think too thin: too few

loseBxPercepPerc2 <- dat %>%
  filter(lastYrLose == 1) %>%
  filter(!ConsiderWt=="too thin") %>%
  drop_na(ConsiderWt)%>%
  #group by Race and Sex %>%
  group_by(Race, Male, ConsiderWt) %>%
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
table(dat$ngLowCal)

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


dat$all_lose_bx <-
  dat %>%
  select_at(vars(contains("WHD080", ignore.case = F))) %>%
  mutate_all(.funs = as.character)%>%
  unite("all_not_gain", WHD080A:WHD080U, na.rm = T) %>%
  mutate_all(.funs = ~(str_replace_all(., "10", "A"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "11", "B"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "12", "C"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "13", "D"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "14", "E"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "15", "F"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "16", "G"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "17", "H"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "31", "I"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "32", "J"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "33", "K"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "40", "L"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "34", "M"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "30", "N"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "41", "O"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "42", "P"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "43", "Q"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "44", "R"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "45", "S"))) %>%
  mutate_all(.funs = ~(str_replace_all(., "46", "T"))) 


#make it a vector, not a data frame
dat$all_lose_bx <- dat$all_lose_bx$all_not_gain

#replace the underscores
dat$all_lose_bx <- str_remove_all(dat$all_lose_bx, "_")

#1st: how many unique combinations are there?
length(table(dat$all_lose_bx)) : 1943

#How many individually have each of the letters A:T?
dat %>%
  select(all_lose_bx) %>%
  summarise(a = sum(grepl("A", all_lose_bx)),
            b = sum(grepl("B", all_lose_bx)),
            c = sum(grepl("C", all_lose_bx)),
            d = sum(grepl("E", all_lose_bx)),
            s = sum(grepl("S", all_lose_bx)),
            t = sum(grepl("T", all_lose_bx)),
            e = sum(grepl("E", all_lose_bx)),
            f = sum(grepl("F", all_lose_bx)),
            g = sum(grepl("G", all_lose_bx)),
            h = sum(grepl("H", all_lose_bx)),
            i = sum(grepl("I", all_lose_bx)),
            j = sum(grepl("J", all_lose_bx)),
            k = sum(grepl("K", all_lose_bx)),
            l = sum(grepl("L", all_lose_bx)),
            m = sum(grepl("M", all_lose_bx)),
            n = sum(grepl("N", all_lose_bx)),
            o = sum(grepl("O", all_lose_bx)),
            p = sum(grepl("P", all_lose_bx)),
            q = sum(grepl("Q", all_lose_bx)),
            r = sum(grepl("R", all_lose_bx)))


#How many coccurence of 2 given letters
lookForVec <- c(NULL)

for (j in 1:20){
  letj <- LETTERS[j]
  for (k in (j+1):20){
    letk <- LETTERS[k]
    lookFor <- paste0(letj, letk)
    lookForVec <- append(lookForVec, lookFor)
  }
}

countLookFor <- c(NULL)
for (i in 1:length(lookForVec)){
  countLookFor <- append(countLookFor, sum(grepl(pattern = lookForVec[i], x = dat$all_lose_bx)))
}

twoBx <- as.data.frame(cbind(lookForVec, countLookFor))
twoBx$countLookFor <- as.numeric(as.character(twoBx$countLookFor))
#CD, BC, AD, AB, and DM are all very common
#AD = 1011, ate less + exercise
##################################################################
#how many cocurrences of 3 given letters?
lookForVec <- c(NULL)
for (j in 1:20){
  letj <- LETTERS[j]
  for (k in (j+1):20){
    letk <- LETTERS[k]
    for(l in (k+1):20){
      letl <- LETTERS[l]
      lookFor <- paste0(letj, letk, letl)
      lookForVec <- append(lookForVec, lookFor)
    }
  }
}

countLookFor <- c(NULL)
for (i in 1:length(lookForVec)){
  countLookFor <- append(countLookFor, sum(grepl(pattern = lookForVec[i], x = dat$all_lose_bx)))
}

threeBx <- as.data.frame(cbind(lookForVec, countLookFor))
threeBx$countLookFor <- as.numeric(as.character(threeBx$countLookFor))
#ABC, BCD, QRS, RST, CDM all have more than 400
#QRS = fruitveg, change diet, and less sweet
#ABC = ate less, lowcal, less fat
#BCD = low cal, less fat, and exercise
#CDM = less fat, exercise, h20


####################################################################
#how many coccurences of 4 letters?
##################################################################
#how many cocurrences of 3 given letters?
lookForVec <- c(NULL)
for (j in 1:20){
  letj <- LETTERS[j]
  for (k in (j+1):20){
    letk <- LETTERS[k]
    for(l in (k+1):20){
      letl <- LETTERS[l]
      for (m in (l+1):20){
        letm <- LETTERS[m]
        lookFor <- paste0(letj, letk, letl, letm)
        lookForVec <- append(lookForVec, lookFor)
      }
    }
  }
}

countLookFor <- c(NULL)
for (i in 1:length(lookForVec)){
  countLookFor <- append(countLookFor, sum(grepl(pattern = lookForVec[i], x = dat$all_lose_bx)))
}

fourBx <- as.data.frame(cbind(lookForVec, countLookFor))
fourBx$countLookFor <- as.numeric(as.character(fourBx$countLookFor))
#ABCD and QRST
