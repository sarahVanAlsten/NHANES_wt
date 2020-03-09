#Sarah Van Alsten
#looking to see how food insecurity/obesity trends change
#over time in the selected NHANES years

#open library
library(tidyverse)
library(survey)

#read in the dataset
dat <- haven::read_dta("C:\\Users\\sarah.vanalsten\\Downloads\\nhanes2.dta")
dat$obese <- round(dat$obese)
dat$fsAny <- round(dat$fsAny)
dat$overweight <- ifelse(dat$BMIcat == 3, 1,0)

#set the survey design
svy <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC10YR,
                 data = dat[!is.na(dat$WTMEC10YR) & !is.na(dat$SDMVSTRA),],
                 nest = T)

#set the survey design
svy05 <- subset(svy, cycle_x_4 == "2005-2006")
svy07 <- subset(svy, cycle_x_4 == "2007-2008")
svy09 <- subset(svy, cycle_x_4 == "2009-2010")
svy11 <- subset(svy, cycle_x_4 == "2011-2012")
svy13 <- subset(svy, cycle_x_4 == "2013-2014")

#Association between fs and obesity
svytable(~fsAny + obese, design = svy)
#col %
prop.table(svytable(~fsAny + obese, design = svy), margin = 2)
#row %
prop.table(svytable(~fsAny + obese, design = svy), margin = 1)

svytable(~fsAny + cycle_x_4, design = svy)
prop.table(svytable(~fsAny + cycle_x_4, design = svy), margin = 2)
########################################################################################

#standard errors of obesity by fs prev
t1 <- svyby(formula = ~obese, by = ~fsAny, design = svy05, FUN = svymean)
t2 <- svyby(formula = ~obese, by = ~fsAny, design = svy07, FUN = svymean)
t3 <- svyby(formula = ~obese, by = ~fsAny, design = svy09, FUN = svymean)
t4 <- svyby(formula = ~obese, by = ~fsAny, design = svy11, FUN = svymean)
t5 <- svyby(formula = ~obese, by = ~fsAny, design = svy13, FUN = svymean)

#standard errors of overweight by fs prev
t6 <- svyby(formula = ~overweight, by = ~fsAny, design = svy05, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t7 <- svyby(formula = ~overweight, by = ~fsAny, design = svy07, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t8 <- svyby(formula = ~overweight, by = ~fsAny, design = svy09, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t9 <- svyby(formula = ~overweight, by = ~fsAny, design = svy11, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t10 <- svyby(formula = ~overweight, by = ~fsAny, design = svy13, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)

names(t6) <- names(t1)
names(t7) <- names(t1)
names(t8) <- names(t1)
names(t9) <- names(t1)
names(t10) <- names(t1)

surveyRes <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9 ,t10)
surveyRes$cycle <- rep(c("2005-2006", "2005-2006",
                     "2007-2008", "2007-2008",
                     "2009-2010", "2009-2010",
                     "2011-2012", "2011-2012",
                     "2013-2014", "2013-2014"),2)
surveyRes$cycle <- as.factor(surveyRes$cycle)
surveyRes$oborow <- c(rep("Obesity",10),
                      rep("Overweight", 10))
surveyRes$group <- paste0(surveyRes$oborow, surveyRes$fsAny)
surveyRes$fsAny <- ifelse(surveyRes$fsAny == 0, "No", "Yes")



surveyRes %>%
  ggplot(aes(x = cycle, y = obese, color = factor(fsAny),
             group = factor(fsAny))) +
  geom_point() + geom_path() +
  geom_errorbar(aes(ymin = obese - 1.96*se,
                    ymax = obese + 1.96*se),
                width = .15) +
  facet_grid(~oborow) +
  ylim(0, .6) +
  theme_bw() +
  labs(y = "Obesity Prevalence",
       x = "Survey Year",
       color = "Food Insecure",
       title = "Obesity and Overweight Prevalence by Food Insecurity Status",
       caption = "Error Bars Represent 95% Confidence Intervals")

########################################################
#get trends in what people are doing about their weight/like to weigh/consider
t1 <- svyby(formula = ~factor(doingAbtWt), by = ~fsAny, design = svy05, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t2 <- svyby(formula = ~factor(doingAbtWt), by = ~fsAny, design = svy07, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t3 <- svyby(formula = ~factor(doingAbtWt), by = ~fsAny, design = svy09, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t4 <- svyby(formula = ~factor(doingAbtWt), by = ~fsAny, design = svy11, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)
t5 <- svyby(formula = ~factor(doingAbtWt), by = ~fsAny, design = svy13, FUN = svymean, na.rm =T, drop.empty.groups = TRUE)

doingRes <- gtools::smartbind(t1, t2, t3, t4, t5)

doingRes <- doingRes %>%
  pivot_longer(cols = 2:11,
               names_to = "DoingAbtWt",
               values_to = "Estimate")

doingRes$type <- ifelse(str_detect(doingRes$DoingAbtWt, "se"), "se", "prevalence")
doingRes$DoingAbtWt <- c(rep(1:5,20))
doingRes$cycle <- c(rep("2005-2006",20),
                    rep("2007-2008",20),
                    rep("2009-2010",20),
                    rep("2011-2012",20),
                    rep("2013-2014",20))
