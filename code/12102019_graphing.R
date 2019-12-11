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

