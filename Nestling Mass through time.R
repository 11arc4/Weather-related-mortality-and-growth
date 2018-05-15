#Are nestlings fledging at a lower weight than they did in the past? 

#I'm interested in knowing this because it could help explain why we see
#differences in recruitment across years. It could also explain why local
#weather conditions only affect survival when the population is declining. If
#the juveniles are poor quality they don't have much buffer and both they and
#their parents may have to work harder to feed themselves in poor weather. 
library(tidyverse)
library(lme4)
library(MuMIn)

dat <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis/Nestling Measurements for Analysis 1975-2017.csv", as.is=T)

#There look to be some points in there that must be typos or misrecordings because there's no way the bird was that size. 
ggplot(dat %>% filter(mass<30, age>0, age<20), aes(x=age, y=mass))+
  geom_point()+
  geom_smooth(method="lm", formula=y~poly(x, 3))


dat2 <- dat %>% filter(mass<30, age>2, age<17)



ggplot(dat2 %>% filter(age>=10), aes(x=age, y=mass))+
  geom_point()+
  geom_smooth(method="lm")+
  ggthemes::theme_few()

#looks like even at the very coarse population status level, we may be seeing
#differences in mass for the older birds.



#OK let's try modelling changes in nestling mass through time. If we model all
#the ages we probably need to include something to deal with the polynomial
#nature of the data. Instead, perhaps it's better to only look at the last
#measurements (day 10 and on), when they are closer to fledging. I used only one
#measurement per nestling (the one closest to 12 days old)
dat3 <- dat2 %>% filter(!is.na(year) & !is.na(age) & age>9 & age<=16 ) %>% group_by(nestlingID) %>% slice(which.min(abs(age)-12))
length(unique(dat3$nestID))
#Data set has 28, 577 rows, and 2,915 unique nests, 13,842 unique nestlings. 
ggplot(dat3, aes(x=age, y=mass))+
  geom_point()+
  geom_smooth(method="lm", formula=y~x)

dat3$year2 <- (dat3$year-1977)/10
dat3$age2 <- dat3$age-10
dat3$age2 <- dat3$age-15


mod <- lmer(mass~age*year2 + (1|nestID), data=dat3, na.action="na.fail")

plot(mod)
hist(resid(mod))
plot(resid(mod)~dat3$age)
plot(resid(mod)~dat3$year2)
#This model looks really good. 

#Need to keep the random nest effect
summary(mod)

dredge(mod)
anova(mod, test="F")

#We should keep all terms

mam <- lmer(mass~age*year2 + (1|nestID), data=dat3, na.action="na.fail")
summary(mam)

#We'll center age at 10 and 15


lmerTest

library(lmerTest)
anova(mam, test="F")


newdata <- data.frame(age=c(rep(10, 41 ), rep(15, 41)), 
                      year2=rep(seq(0.2,4.2 , 0.1), 2), 
                      age2= c(rep(0, 41 ), rep(5, 41)),
                      year=rep(seq(1977,2017 , 1), 2), 
                      predicted=NA, 
                      lcl=NA, 
                      ucl=NA)

#calculate predicted values. 
newdata$predicted <- predict(mam, newdata, re.form=NA)
#age= 16 for the ready to fledge birds and 12 for the not

#bootstrap confidence intervales based on https://github.com/lme4/lme4/issues/388 
## param only (not including individual variation or anything like that)
b3 <- bootMer(mam,FUN=function(x) predict(x,newdata=newdata,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)

bootsum <- function(x,ext="_1") {
  d <- t(data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975))))))
  colnames(d) <- c("bpred","lwr","upr")
  return(d)
}

newdata[, 5:7] <- bootsum(b3,"_3")

newdata$age <- as.factor(newdata$age)

ggplot(newdata, aes(x=year))+
  geom_ribbon(aes( ymin=lcl, ymax=ucl, fill=age), alpha=0.3)+
  geom_line(aes(y=predicted, linetype=age), color="black")+
  labs(y="Body mass (g)", x="Year", linetype="Age (days)", fill="Age (days)")+ 
  xlim(1975,2017)+
  scale_fill_grey()+
  scale_color_grey()+
  #geom_point(data=dat3 %>% filter(age==15 | age==10), aes(x=year, y=mass, color= factor(age)), alpha=0.5)+
  ggthemes::theme_few(base_size = 16, base_family="serif")+
  theme(legend.position = c(0.85, 0.8))
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Nestling mass through time.jpeg", units="in", width=5, height=4, device="jpeg")








##################Need to double check that nestlings haven't just gotten smaller. 
dat4 <- dat %>% filter(!is.na(year) & !is.na(ninprim) & !is.na(age) & ninprim<80 & age>9 & age<16) %>% group_by(nestlingID) %>% slice(which.min(abs(age)-12))
ggplot(dat4, aes(x=age, y=ninprim))+
  geom_point()+
  geom_smooth()

dat4$year2 <- (dat4$year-1975) /10

wmod <- lmer(ninprim~age*year2 + (1|nestID), data=dat4, na.action="na.fail")
plot(wmod)
hist(resid(wmod))
plot(resid(wmod)~dat4$year)
plot(resid(wmod)~dat4$age)
#looks good. Although there is a fairly large chunk of time where we are missing measurements

dredge(wmod)
anova(wmod)
wmam <- lmer(ninprim~age+year2 + (1|nestID), data=dat4, na.action="na.fail")
summary(wmam)
anova(wmam)


wnewdata <- data.frame(age=c(rep(10, 30 ), rep(15, 30)), 
                      year2=rep(seq(1.3,4.2 , 0.1), 2), 
                      year=rep(seq(1988,2017 , 1), 2), 
                      predicted=NA, 
                      lcl=NA, 
                      ucl=NA)

#calculate predicted values. 
wnewdata$predicted <- predict(wmam, wnewdata, re.form=NA)
#age= 16 for the ready to fledge birds and 12 for the not

#bootstrap confidence intervales based on https://github.com/lme4/lme4/issues/388 
## param only (not including individual variation or anything like that)
b3 <- bootMer(mam,FUN=function(x) predict(x,newdata=wnewdata,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)


wnewdata[4:6] <- bootsum(b3,"_3")

wnewdata$age <- as.factor(wnewdata$age)


ggplot(wnewdata, aes(x=year))+
  geom_line(aes(y=predicted, color=age))+
  geom_ribbon(aes( ymin=lcl, ymax=ucl, fill=age), alpha=0.3)+
  labs(y="Wing chord (mm)", x="Year", color="Age (days)", fill="Age (days)")+ 
  xlim(1975,2017)+
  scale_fill_manual(values=c("steelblue","orchid3"))+
  scale_color_manual(values=c("steelblue","orchid3"))+
  #geom_point(data=dat4 %>% filter(age==15 | age==10), aes(x=year, y=ninprim, color= factor(age)), alpha=0.5)+
  theme_classic(base_size = 16)
