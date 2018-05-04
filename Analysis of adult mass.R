#Change in adult mass
library(tidyverse)

adult <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Adult body condition change data 2017.csv")


adult2 <- adult %>% filter(!is.na(Mass1) & !is.na(Mass2))

ggplot(adult2, aes(x=Mass1, y=Mass2, color=Sex))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  facet_grid(~Sex)




ggplot(adult2, aes(y=Mass2-Mass1, x=FledgeSize, color=Sex))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  facet_grid(~Sex)
#Looks like females loose more mass with larger fledge sizes. Males aren't as
#bothered-- they probably aren't working as hard as the females.

#sample size for the males is 11, for the females it's 38
#I feel very comfortable making conclusions about the females, but not so great about the males. 

sum(adult2$Sex=="F")
