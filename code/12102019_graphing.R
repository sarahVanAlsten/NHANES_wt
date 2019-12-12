#######################################################
# Author: Sarah Van Alsten                            #
# Date Created: December 10, 2019                     #
# Purpose: Code to run Latent Class Causal Analysis   #
# Data Used: NHANES 07, 09, 11                        #
# Packages Used: tidyverse, kableExtra                #
# Last Update: December 10, 2019                      #
#######################################################
#open packages
library(tidyverse)
library(kableExtra)

#read in data
gamma <- read.csv(file = "data//raceSexClassPrev.csv")
rho <- read.csv(file = "data//raceSexClassRho.csv")

#make columns for race and sex to aid in faceting for graphs
rho$male <- ifelse(grepl(pattern = "f", x = rho$X), 0, 1)
rho$race <- ifelse(grepl(pattern = "b", x = rho$X), "Black",
                   ifelse(grepl(pattern = "w", x = rho$X), "White",
                          ifelse(grepl(pattern = "h", x = rho$X), "Hispanic", "Other")))

gamma$male <- ifelse(grepl(pattern = "f", x = gamma$X), 0, 1)
gamma$race <- ifelse(grepl(pattern = "b", x = gamma$X), "Black",
                   ifelse(grepl(pattern = "w", x = gamma$X), "White",
                          ifelse(grepl(pattern = "h", x = gamma$X), "Hispanic", "Other")))

names(gamma) <- c("X", "Weigh.Same.Class", "Weigh.More.Class", "Weigh.Less.Class", "Male", "Race")

#read in std errors
gamma.se <- read.csv("data//gammaStdErr.csv")
gamma.se <- t(gamma.se)
gamma.se <- gamma.se[-1,]
gamma.se <- as.data.frame(gamma.se)
names(gamma.se) <- c("Weigh.Same.Class.SE", "Weigh.More.Class.SE", "Weigh.Less.Class.SE")
gamma.se$Race <- ifelse(grepl(pattern = "b", x = rownames(gamma.se)), "Black",
                         ifelse(grepl(pattern = "w", x = rownames(gamma.se)), "White",
                                ifelse(grepl(pattern = "h", x = rownames(gamma.se)), "Hispanic", "Other")))
gamma.se$Male <- ifelse(grepl(pattern = "f", x = rownames(gamma.se)), 0, 1)

#merge in with gammas overall
gamma.full <- gamma %>% 
  right_join(gamma.se, by=c("Race","Male"))

#make plots to see the class prevalences
gamma3class.1 <- gamma.full %>%
  pivot_longer(cols = c("Weigh.Same.Class", "Weigh.More.Class", "Weigh.Less.Class"),
               names_to = "Class", values_to = "Prevalence") %>%
  pivot_longer(cols = c("Weigh.Same.Class.SE", "Weigh.More.Class.SE", "Weigh.Less.Class.SE"),
               names_to = "SEtype", values_to = "SE")%>%
  mutate(maleFact = ifelse(Male == 1, "Male", "Female")) %>%
  rowwise() %>%
  filter(grepl(pattern = as.character(Class), x = as.character(SEtype))) %>%
  ungroup() %>%
  #only keep appropriate SE pairs
  ggplot(aes(x = Race, y = Prevalence, fill = Class)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_errorbar(aes(ymin = Prevalence - as.numeric(as.character(SE)),
                ymax = Prevalence + as.numeric(as.character(SE))), 
                width = .2,
                position = position_dodge(.9))+
  facet_wrap(~maleFact)+
  theme_minimal() +
  ggtitle("Class Prevalence (Gamma) By Race and Sex")+
  labs(y = "Prevalence within Group",
       caption = "Error bars represent SEM per design-based analysis")+
  theme(plot.caption = element_text(hjust = 0))

gamma %>%
  pivot_longer(cols = c("Weigh.Same.Class", "Weigh.More.Class", "Weigh.Less.Class"),
               names_to = "Class", values_to = "Prevalence") %>%
  mutate(maleFact = ifelse(Male == 1, "Male", "Female")) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Prevalence, fill = Class),
           stat = "identity",
           position = "dodge") +
  facet_grid(Race~maleFact) +
  theme_minimal()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())

gamma3class.graph <- gamma.full %>%
  pivot_longer(cols = c("Weigh.Same.Class", "Weigh.More.Class", "Weigh.Less.Class"),
               names_to = "Class", values_to = "Prevalence") %>%
  pivot_longer(cols = c("Weigh.Same.Class.SE", "Weigh.More.Class.SE", "Weigh.Less.Class.SE"),
               names_to = "SEtype", values_to = "SE")%>%
  mutate(maleFact = ifelse(Male == 1, "Male", "Female")) %>%
  rowwise() %>%
  filter(grepl(pattern = as.character(Class), x = as.character(SEtype))) %>%
  ungroup() %>%
  ggplot(aes(x = Class, y = Prevalence, fill = Class)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_errorbar(aes(ymin = Prevalence - as.numeric(as.character(SE)),
                    ymax = Prevalence + as.numeric(as.character(SE))), 
                width = .2,
                position = position_dodge(.9))+
  facet_grid(maleFact~Race) +
  theme_minimal()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))+
  ggtitle("Class Prevalence (Gamma) By Race and Sex")+
  labs(y = "Prevalence within Group",
       caption = "Error bars represent SEM per design-based analysis")


#save the graphs
ggsave(plot = gamma3class.graph, device = "png", dpi = 400,
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\gamma3class.png")

ggsave(plot = gamma3class.1, device = "png", dpi = 400,
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\gamma3classNoFacet.png")


gamma %>%
  mutate(" " = Race)%>%
  select(7, 2:4) %>%
  kable(format = "html", digits = 4,
        caption = "Class Prevalences") %>%
  kableExtra::group_rows(group_label = "Male", start_row = 1, end_row = 4)%>%
  kableExtra::group_rows(group_label = "Female", start_row = 5, end_row = 8)%>%
  kable_styling("striped")


#vars in order are: doingAbtWt, ConsiderWt, LikeToWeigh
#FOR DOING ABOUT WEIGHT
#1: lost weight intentionally
#2: lost weight unintentionally
#3: tried to lose weight but did not
#4: tried to not gain weight (not included in 2013 cycle)
#5: all others (not trying to do anything)
#These are structured as: Var, ResponseLevel, Class, FlatteningConstant

#pivot table so in format rhovalue : variable
rho2 <- rho %>%
  pivot_longer(cols = 2:25, names_to = "RhoVar", values_to = "IEP")

#use substring command to extract values for var, response level, and class
rho2 <- rho2 %>%
  mutate(variable = substr(RhoVar, 5, 5),
         respLev = substr(RhoVar, 7, 7),
         class = substr(RhoVar, 9, 9))

rho2 <- rho2 %>%
  mutate(varString = ifelse(variable == 1, "Doing About Weight",
                            ifelse(variable == 2, "Consider Weight", "Like to Weigh")),
         class = ifelse(class == 1, "Weigh Same Class", ifelse(class == 2, "Weigh More Class",
                                                               "Weigh Less Class")))


#now add labels for each of responses per given variable
rho2 <- rho2 %>%
  mutate(response = ifelse(varString == "Doing About Weight",
                           ifelse(respLev == 1, "Lost Weight Intentionally",
                                  ifelse(respLev == 2, "Lost Weight Unintentionally",
                                         ifelse(respLev == 3, "Tried to Lose Weight",
                                                ifelse(respLev == 4, "Tried to Not Gain Weight",
                                                       "Not Doing Anything About Weight")))),
                           ifelse(varString == "Consider Weight",
                                  ifelse(respLev == 1, "Underweight",
                                         ifelse(respLev == 2, "About Right", "Overweight")),
                                  ifelse(respLev == 1, "Like to Weigh Less",
                                         ifelse(respLev == 2, "Like to Weigh Same", "Like to Weigh More")))))

#read in rho standard errors
rho.se <- read.csv("C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\data\\stderrRho.csv")
rho.se$X <- as.character(rho.se$X)

#get last two characters which will be race/sex
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
rho.se$X2 <- substrRight(rho.se$X, 2)

#add columns for race and sex
rho.se <- rho.se %>%
  mutate(race = ifelse(grepl(pattern = "o", x = X2), "Other",
                       ifelse(grepl(pattern = "b", x = X2), "Black",
                              ifelse(grepl(pattern = "w", x = X2), "White",
                                     ifelse(grepl(pattern = "hm", x = X2) | grepl(pattern = "hf", x = X2),
                                            "Hispanic", "Other"))))) %>%
  mutate(male = ifelse(grepl(pattern = "f", x = X2), 0, 1))


#gather to order in same way as plain rhos: one col for var, one for the se
rho.se <- rho.se %>%
  pivot_longer(cols = 2:16, names_to = "variable", values_to = "SE")

#get rid of X2, not needed anymore
rho.se <- rho.se %>%
  select(-X2)

#use substring command to extract values for var, response level, and class
rho.se <- rho.se %>%
  mutate(respLev = substr(variable, 2, 2),
         varRespondedTo = substr(X, 1, nchar(X)-2),
         class = substr(variable, 10, 10))


rho.se <- rho.se %>%
  mutate(varString = ifelse(grepl(x = varRespondedTo, pattern = "doing"), "Doing About Weight",
                            ifelse(grepl(x = varRespondedTo, "Consider"), "Consider Weight", "Like to Weigh")),
         class = ifelse(class == 1, "Weigh Same Class", ifelse(class == 2, "Weigh More Class",
                                                               "Weigh Less Class")))


#now add labels for each of responses per given variable
rho.se <- rho.se %>%
  mutate(response = ifelse(varString == "Doing About Weight",
                           ifelse(respLev == 1, "Lost Weight Intentionally",
                                  ifelse(respLev == 2, "Lost Weight Unintentionally",
                                         ifelse(respLev == 3, "Tried to Lose Weight",
                                                ifelse(respLev == 4, "Tried to Not Gain Weight",
                                                       "Not Doing Anything About Weight")))),
                           ifelse(varString == "Consider Weight",
                                  ifelse(respLev == 1, "Underweight",
                                         ifelse(respLev == 2, "About Right", "Overweight")),
                                  ifelse(respLev == 1, "Like to Weigh Less",
                                         ifelse(respLev == 2, "Like to Weigh Same", "Like to Weigh More")))))

#finally, for each of the categories, one level was left out as the reference.
#need to readd that level back in and get the IEP for that category
lastRes <- rho2 %>%
  group_by(race, male, varString, class) %>%
  summarise(IEP= 1 -sum(IEP, na.rm = T))

#now, add the column for response to say what the omitted category is
#for consider wt, that category is "overweight"
#for doing abt weight, that category is "not doing anything"
#for like to weigh, that category, is "weigh more"
lastRes <- lastRes %>%
  mutate(response = ifelse(varString == "Consider Weight", "Overweight",
                           ifelse(varString == "Doing About Weight", "Not Doing Anything About Weight",
                                  "Like to Weigh More")))

lastRes <- lastRes %>%
  ungroup()

rho3 <- rho2 %>%
  select(-c(X, RhoVar, variable, respLev))

rho3 <- rbind(rho3, lastRes)

#now want to merge the standard errors with the estimates, doing so by matching
#the class, varString, and response
rho.full <- rho3 %>%
  right_join(rho.se, by = c("class", "response", "varString", "race", "male"))

#now make a graph that shows item endorsement prob for each item/response by race/sex/class,
#including the standard erros
#to make easier, will just be doing the individual variables separtely
#first: what are you doing about your weight?
###########################################################################################
doAbtWt.facetRace <- rho.full %>%
  filter(varString == "Doing About Weight") %>%
  mutate(dispClass = str_remove_all(class, pattern = "Class")) %>%
  mutate(dispClass = str_remove_all(dispClass, pattern = "Weigh")) %>%
  mutate(maleFact = ifelse(male, "Male", "Female")) %>%
  ggplot(aes(x = dispClass, y = IEP, group = response, fill = response))+
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  theme_minimal()+
  facet_grid(maleFact~race) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_errorbar(aes(ymin = IEP - 1.96*SE, ymax = IEP + 1.96*SE), width = .2, position = position_dodge(.9))+
  xlab("Class") + ylab("Item Endorsement Probability")+ 
  labs(fill = "Response", 
       caption = "Error Bars Represent 95% Confidence Interval. Standard Errors Weighted for Survey Design.")+
  ggtitle("What Are You Doing About Your Weight?")


doAbtWt.facetClass <- rho.full %>%
  filter(varString == "Doing About Weight") %>%
  mutate(maleFact = ifelse(male, "Male", "Female")) %>%
  ggplot(aes(x = race, y = IEP, group = response, fill = response))+
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  theme_minimal()+
  facet_grid(maleFact~class) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.caption = element_text(hjust = .5))+
  geom_errorbar(aes(ymin = IEP - 1.96*SE, ymax = IEP + 1.96*SE), width = .25, 
                position = position_dodge(.9))+
  xlab("Race") + ylab("Item Endorsement Probability")+
  labs(fill = "Response", 
       caption = "Error Bars Represent 95% Confidence Interval. Standard Errors Weighted for Survey Design.")+
  ggtitle("What Are You Doing About Your Weight?")

ggsave(plot = doAbtWt.facetClass, device = "png",
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\doAbtWt.facetClass.png")
ggsave(plot = doAbtWt.facetRace, device = "png",
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\doAbtWt.facetRace.png")
############################################################################################################
#now, how do you consider your weight?

ConsiderWt.facetRace <- 
  rho.full %>%
  filter(varString == "Consider Weight") %>%
  mutate(dispClass = str_remove_all(class, pattern = "Class")) %>%
  mutate(dispClass = str_remove_all(dispClass, pattern = "Weigh")) %>%
  mutate(maleFact = ifelse(male, "Male", "Female")) %>%
  ggplot(aes(x = dispClass, y = IEP, group = response, fill = response))+
  geom_bar(stat = "identity", position = position_dodge(.75)) +
  theme_minimal()+
  facet_grid(maleFact~race) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_errorbar(aes(ymin = IEP - 1.96*SE, ymax = IEP + 1.96*SE), width = .2,
                position = position_dodge(.75))+
  xlab("Class") + ylab("Item Endorsement Probability")+ 
  labs(fill = "Response", 
       caption = "Error Bars Represent 95% Confidence Interval. Standard Errors Weighted for Survey Design.")+
  ggtitle("How Do You Consider Your Weight?")

considerWt.facetClass <- rho.full %>%
  filter(varString == "Consider Weight") %>%
  mutate(maleFact = ifelse(male, "Male", "Female")) %>%
  ggplot(aes(x = race, y = IEP, group = response, fill = response))+
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  theme_minimal()+
  facet_grid(maleFact~class) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.caption = element_text(hjust = .5))+
  geom_errorbar(aes(ymin = IEP - 1.96*SE, ymax = IEP + 1.96*SE), width = .25, 
                position = position_dodge(.9))+
  xlab("Race") + ylab("Item Endorsement Probability")+
  labs(fill = "Response", 
       caption = "Error Bars Represent 95% Confidence Interval. Standard Errors Weighted for Survey Design.")+
  ggtitle("How Do Your Consider Your Weight?")

ggsave(plot = considerWt.facetClass, device = "png",
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\considerWt.facetClass.png")
ggsave(plot = ConsiderWt.facetRace, device = "png",
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\considerWt.facetRace.png")


####################################################################################
#what would you like to weigh?

#likeWeigh.facetRace <- 
  rho.full %>%
  filter(varString == "Like to Weigh") %>%
  mutate(dispClass = str_remove_all(class, pattern = "Class")) %>%
  mutate(dispClass = str_remove_all(dispClass, pattern = "Weigh")) %>%
  mutate(maleFact = ifelse(male, "Male", "Female")) %>%
  ggplot(aes(x = dispClass, y = IEP, group = response, fill = response))+
  geom_bar(stat = "identity", position = position_dodge(.75)) +
  theme_minimal()+
  facet_grid(maleFact~race) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))+
  geom_errorbar(aes(ymin = IEP - 1.96*SE, ymax = IEP + 1.96*SE), width = .2,
                position = position_dodge(.75))+
  xlab("Class") + ylab("Item Endorsement Probability")+ 
  labs(fill = "Response", 
       caption = "Error Bars Represent 95% Confidence Interval. Standard Errors Weighted for Survey Design.")+
  ggtitle("What Would You Like to Weigh?")


#considerWt.facetClass <- 
  rho.full %>%
  filter(varString == "Like to Weigh") %>%
  mutate(maleFact = ifelse(male, "Male", "Female")) %>%
  ggplot(aes(x = race, y = IEP, group = response, fill = response))+
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  theme_minimal()+
  facet_grid(maleFact~class) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.caption = element_text(hjust = .5))+
  geom_errorbar(aes(ymin = IEP - 1.96*SE, ymax = IEP + 1.96*SE), width = .25, 
                position = position_dodge(.9))+
  xlab("Race") + ylab("Item Endorsement Probability")+
  labs(fill = "Response", 
       caption = "Error Bars Represent 95% Confidence Interval. Standard Errors Weighted for Survey Design.")+
  ggtitle("What Would You Like To Weigh?")

ggsave(plot = considerWt.facetClass, device = "png",
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\considerWt.facetClass.png")
ggsave(plot = ConsiderWt.facetRace, device = "png",
       filename = "C:\\Users\\Owner\\OneDrive\\Documents\\Duncan_Lab_2018\\NHANES_WeightPerception\\NHANES_wt\\images\\considerWt.facetRace.png")
