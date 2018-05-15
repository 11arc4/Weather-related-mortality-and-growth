#Packages Needed
library(lubridate)
library(lme4)
library(nlme)
library(tidyverse)
library(MuMIn)
library(lmerTest)
#We need to a better analysis of adult mass using all the data. This means GLMM
#because I have multiple measurements of the same individual each year. 

#The data we are loading in is all the adult body measurements, and includes the
#data for first nest the bird had that year









adult <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Adult morphometrics 1975-2017.csv", as.is = T, na.strings=c("", "NA"))
names(adult)
str(adult)
adult$band<- as.factor(adult$band)
adult$sex [which(adult$sex=="6" | adult$sex=="7")] <- "U"
adult$sex [which(adult$sex=="F ")] <- "F"
adult$sex<- as.factor(adult$sex)
adult$dateMeas2 <- NA

adult$dateMeas2[which(grepl(pattern="-", adult$dateMeas))] <- lubridate::yday(as.Date(adult$dateMeas[which(grepl(pattern="-", adult$dateMeas))], format="%Y-%m-%d"))
adult$dateMeas2[which(grepl(pattern="/", adult$dateMeas))] <- lubridate::yday(as.Date(adult$dateMeas[which(grepl(pattern="/", adult$dateMeas))], format= "%m/%d/%Y"))

adult$year2 <- adult$year -1974 #rescales eveyrone so analysis works better
adult$age <- as.factor(adult$age) 
#I"m not sure if there's a better way to deal with this. There might be. Age is 
#not REALLY a factor because it has an order to it, but it's not numeric either 
#because their age estimates... I also need to remove all the birds that were
#polygynous because I still haven't really figured out a good way to deal with
#them in the dataset....
adult <- adult[which(adult$age!= "ASY/ASY" & adult$age != "AHY/AHY" & adult$age != "SY + ASY"),]

adult$band <- factor(adult$band)
adult$hatchdate[which(adult$hatchdate==0)] <- NA
adult$mass[which(adult$mass<15 | adult$mass>30 )] <- NA

#Make a dummy variable to show when (based on laydate) we measured the bird.
#Can't do hatching because many birds won't have made it to hatching but we
#caught them during inclubation
adult$diff <- adult$dateMeas2-adult$laydate

#I will start with the basic linear mixed model and build up from there
#I will include bird band as a random intercept
#Let me first just subset the data so I have only nice stuff without NAs
adult$Period <- "Incubation"
adult$Period[adult$diff>=18] <- "Nestling"
adult$Period <- factor(adult$Period)


adult2 <- adult %>% filter((sex=="M" | sex=="F") & !is.na(diff) & !is.na(mass) )
summary(adult2$year)
adult3 <- adult %>% filter((sex=="M" | sex=="F") & !is.na(diff) & !is.na(mass) & diff>-5 & diff<30)


####Has adult mass changed through years? Will need to control for when in the
####breeding season we are looking because they will loose mass. Males and females don't respond the
####same and may be polynomially because 
ggplot(adult3, aes(x=diff, y=mass))+
  geom_point()+
  stat_smooth(method="lm", formula=y~poly(x, 3))+
  stat_smooth(method="lm", aes(group=Period))+
  facet_grid(sex~.)+
  geom_vline(xintercept=19, linetype="dashed")

#I think I need to model sexes seperately. We have lots of data for the females
#during incubation but it's probably pretty not good to do incubation for males
#with that little information for the years.

adult3_F <- adult3 %>% filter(sex=="F")
adult3_M <- adult3 %>% filter(sex=="M" & diff>17)

mod <- lmer(mass ~ poly(diff, 1)*year2 + (1|band), data=adult3_F, na.action="na.fail")
summary(mod)
mod_2 <- lmer(mass ~ poly(diff, 2)*year2 + (1|band), data=adult3_F, na.action="na.fail")
summary(mod_2)
mod_3 <- lmer(mass ~ poly(diff, 3)*year2 + (1|band), data=adult3_F, na.action="na.fail")
summary(mod_2)
AICc(mod, mod_2, mod_3)
#Definitely need to retain the random effect of bird. 
#Chose to use the 2rd order polynomial. 

mod_F <- lmer(mass ~ poly(diff, 2)*year2 + (1|band), data=adult3_F, na.action="na.fail")
plot(mod_F)
hist(resid(mod_F))
plot(resid(mod_F)~adult3_F$diff)
plot(resid(mod_F)~adult3_F$year)
plot(resid(mod_F)~adult3_F$Period)

dredge(mod_F)
anova(mod_F, test="F")

mam_F <- lmer(mass ~ poly(diff, 2)*year2 + (1|band), data=adult3_F, na.action="na.fail")
summary(mam_F)
anova(mam_F)


newdata_F <- data.frame(year=c(rep(1990, 30), rep(2010, 30)), 
                        year2=c(rep(1990-1974, 30), rep(2010-1974, 30)),
                        diff=seq(0, 29, 1), 
                        Period=factor("Incubation", levels=c("Incubation", "Nestling")), 
                        predicted=NA, 
                        lcl=NA, 
                        ucl=NA)
newdata_F$Period[which(newdata_F$diff>=18)]<- "Nestling"

library(boot)
b3 <- bootMer(mam_F,FUN=function(x) predict(x,newdata=newdata_F,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)
#### Confidence and prediction intervals for *unobserved* levels
bootsum <- function(x,ext="_1") {
  d <- data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975)))))
  d <- setNames(d,paste0(c("bpred","lwr","upr"),ext))
  return(d)
}
newdata_F[,5:7]<- t(bootsum(b3,"_3"))


PanelA <- ggplot()+
  geom_ribbon(data=newdata_F, aes(x=diff, ymin=lcl, ymax=ucl, fill=factor(year)), alpha=0.4)+
  geom_line(data=newdata_F, aes(x=diff, y=predicted, linetype=factor(year)), color="black")+
  geom_vline(xintercept=14, linetype="dashed")+
  scale_fill_grey()+
  scale_color_grey()+
  xlim(0,30)+
  ylim(19,22.7)+
  geom_text(aes(x=28, y=22.5, label="Female"), family="serif", size=6)+
  labs(x=" ", y="Body mass (g)", linetype="", fill="")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")+
  theme(legend.position = c(0.2, 0.2))



##################
#Now let's do the males. 
mod <- lmer(mass ~ poly(diff, 1)*year2 + (1|band), data=adult3_M, na.action="na.fail")
summary(mod)
mod_2 <- lmer(mass ~ poly(diff, 2)*year2 + (1|band), data=adult3_M, na.action="na.fail")
summary(mod_2)
mod_3 <- lmer(mass ~ poly(diff, 3)*year2 + (1|band), data=adult3_M, na.action="na.fail")
summary(mod_2)
AICc(mod, mod_2, mod_3)
#SHould retain the random effect but only barely. Also should go with a linear
#relationship.


mod_M <- lmer(mass ~ diff*year2+ (1|band), data=adult3_M, na.action="na.fail")

plot(mod_M)
hist(resid(mod_M))
shapiro.test(resid(mod_M))
plot(resid(mod_M)~adult3_M$diff)
plot(resid(mod_M)~adult3_M$year)

dredge(mod_M)
anova(mod_M)
car::Anova(mod_M)

mam_M <- lmer(mass ~ diff*year2 + (1|band), data=adult3_M, na.action="na.fail")
summary(mam_M)
anova(mam_M)


newdata_M <- data.frame(year=c(rep(1990, 12), rep(2010, 12)), 
                        year2=c(rep(1990-1974, 12), rep(2010-1974, 12)),
                        diff=rep(seq(18, 29, 1), 2), 
                        Period=factor("Incubation", levels=c("Incubation", "Nestling")), 
                        predicted=NA, 
                        lcl=NA, 
                        ucl=NA)
newdata_M$Period[which(newdata_M$diff>=18)]<- "Nestling"
b3 <- bootMer(mam_M,FUN=function(x) predict(x,newdata=newdata_M,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)
#### Confidence and prediction intervals for *unobserved* levels
bootsum <- function(x,ext="_1") {
  d <- data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975)))))
  d <- setNames(d,paste0(c("bpred","lwr","upr"),ext))
  return(d)
}
newdata_M[,5:7]<- t(bootsum(b3,"_3"))


update_geom_defaults("text", list(colour = "grey20", family = theme_get()$text$family))
PanelB <- ggplot()+
  geom_ribbon(data=newdata_M, aes(x=diff, ymin=lcl, ymax=ucl, fill=factor(year)), alpha=0.4)+
  geom_line(data=newdata_M, aes(x=diff, y=predicted, linetype=factor(year)), color="black")+
  scale_fill_grey()+
  scale_color_grey()+
  geom_vline(xintercept=14, linetype="dashed")+
  xlim(0, 30)+
  ylim(19,22.7)+
  geom_text(aes(x=28, y=22.5, label="Male"), family="serif", size=6)+
  labs(x="Time since laying began", y="Body mass (g)", linetype="", fill="")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")+
  theme(legend.position = c(0.2, 0.2))

PanelB
PanelA

cowplot::plot_grid(PanelA, PanelB, nrow=2, ncol=1, labels = c("a", "b"))
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Adult mass through time.jpeg", units="in", width=5, height=8, device="jpeg")




#Has wing chord also decreased? Because if it has they're just smaller, not in poorer condition
adult4 <- adult %>% filter((sex=="M" | sex=="F") & !is.na(diff) & !is.na(wingChord) & wingChord>80 & wingChord<135 & diff>-5 & diff<30)

mod2 <- lmer(wingChord ~ sex*year + (1|band), data=adult4, na.action="na.fail")
plot(mod2)
shapiro.test(resid(mod2))
hist(resid(mod2))
plot(resid(mod2)~adult4$sex)
plot(resid(mod2)~adult4$year)
summary(mod2) #Need to retain band ID-- you are measured multiple times and you'll be about the same
dredge(mod2)
anova(mod2, test="F")
mam_wing <- lm(wingChord ~ sex, data=adult4, na.action="na.fail")
summary(mam_wing)
#Wing chord has not deccreased-- these birds aren't actually just getting smaller. They're loosing body condition. 
