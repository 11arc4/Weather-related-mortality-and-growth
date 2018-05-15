dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Nestling survival and Weather Data.csv", as.is=T)  #already excluding AG
library(dplyr)
library(survival)
library(coxme)
library(MuMIn)
library(nlme)


dat$ThermoReg[which(dat$Age1<=6)] <- "Poikilotherm"
dat$ThermoReg[which(dat$Age2<9 & dat$Age1>5)] <- "Intermediate"
dat$ThermoReg[which(dat$Age1>8)] <- "Endotherm"
dat$ThermoReg <- as.factor(dat$ThermoReg)
levels(dat$ThermoReg)
#Status =0 when nest is dead, Status=1 when nest is still alive (even if some
#nestlings died) I might be able to use cluster() to indicate that nestlings
#should be clustered within a nest
#However this is NOT how coxph is expecting it. Coxph wants 0 (or F) for alive, 1 (or T) for dead

dat$Dead <- NA
dat$Dead[which(dat$Status==0)] <- T
dat$Dead[which(dat$Status==1)] <- F

#Question of interest: Does the combination of bad weather characterists add up
#together to do cause low quality nestlings to die more than any single weather
#parameter? Is that particularly common in one of the thermoregulatory
#strategies?
dat2 <- dat %>% filter (!is.na(Mass1) & !is.na(Age1))

#Use our best model of mass to calculate residual masses. 
mamMass <- gls(Mass1 ~ poly(Age1, 3), weights = varFixed(~Age1), data=dat2)
plot(mamMass)
dat2$ResidMass1 <- resid(mamMass)



#OK we are ready to make our global model now. It's huge because we have 2 sets
#of weather variables that need to make it in This may be an issue. What if I
#break it up and consider PC1 and PC2 Seperately? The real issue is probably the
#endotherms. They have such high survival that there aren't many deaths and shit
#doesn't converge.
nrow(dat2 %>% filter(ThermoReg =="Endotherm" & WhyFail!="PREDATION"))
dat3 <- dat2 %>% filter(ThermoReg !="Endotherm" & WhyFail!="PREDATION")
dat3$ThermoReg <- factor(dat3$ThermoReg, levels= c("Poikilotherm", "Intermediate")) #Need this otherwise it tries to figure it out for endotherms too but there's no data

##########################################################################
##################Does Max temp 2 days prior predict survival? 

#answers based on random effects
mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MaxTemp3day+ ResidMass1*MaxTemp3day + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
mod1
#nestlings ID SD is greater than the variance explained for both our random
#effects-- therefore I think we can at least start by getting rid of the
#nestlingID
mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MaxTemp3day+ ResidMass1*MaxTemp3day + (1|NestID), na.action = "na.fail",
               data = dat3)

mod2
#NestID SD <var so we need to keep it. 


options(na.action="na.fail")
dredge(mod2)
#there is a clear winner here.
anova(mod2)

mam_maxtemp <- coxme( Surv(time=Time1, time2=Time2, Dead) ~  MaxTemp3day+ ThermoReg + ResidMass1 + (1|NestID), na.action = "na.fail",
              data = dat3)
summary(mam_maxtemp)






##########################################################################
##################Does mean temp 2 days prior predict survival? 

#answers based on random effects
mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MeanTemp3day+ ResidMass1*MeanTemp3day + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
mod1
#Don't need nestling random effect


mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MeanTemp3day+ ResidMass1*MeanTemp3day + (1|NestID), na.action = "na.fail",
               data = dat3)
mod2
#Do need nest random effect. 

dredge(mod2)
anova(mod2)

mam_meanTemp <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg + MeanTemp3day + (1|NestID), na.action = "na.fail",
                       data = dat3)
summary(mam_meanTemp)




##########################################################################
##################Does whether it rains or not 2 days prior predict survival? 


mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*TotalRainFall3day+ ResidMass1*TotalRainFall3day + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
summary(mod1)
#Definitely don't need the nestling random effect

mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*TotalRainFall3day+ ResidMass1*TotalRainFall3day + (1|NestID), na.action = "na.fail",
               data = dat3)
summary(mod2)
#DO need nest, very much so

dredge(mod2)
#Only one best model
anova(mod2)

mam_rain <- coxme( Surv(time=Time1, time2=Time2, Dead) ~  ThermoReg*TotalRainFall3day + (1|NestID), na.action = "na.fail",
                   data = dat3)
summary(mam_rain)



##############################################################################
##################Does mean windspeed 2 days prior predict survival? 
mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MeanWindspeed3day+ ResidMass1*MeanWindspeed3day + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
summary(mod1)
#No need for nestlings random effect

mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ResidMass1*MeanWindspeed3day+ MeanWindspeed3day*ThermoReg  + (1|NestID), na.action = "na.fail",
               data = dat3)
summary(mod2)
#definitely need nest random effect

dredge(mod2)
anova(mod2)

mam_meanwindspeed <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ MeanWindspeed3day + (1|NestID), na.action = "na.fail",
               data = dat3)
summary(mam_meanwindspeed)


##############################################################################
##################Does the combination of weather conditions (measured with PC1
##################and PC2) 2 days prior predict survival?

#this is a little hinky-- I had to drop the nestling random ID and wasn't able
#to test whether it was important for this model because of too many parameters.
#This could definitely be worse though since it was so unimportant for
#everything else.
mod1 <- coxme(Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*PC13day + ThermoReg*PC23day  +ResidMass1+ (1|NestID), na.action = "na.fail",
              data = dat3)
summary(mod1)
dredge(mod1)
anova(mod1)
mam_PC <- coxme(Surv(time=Time1, time2=Time2, Dead) ~   ThermoReg +PC13day+ (1|NestID), na.action = "na.fail",
                data = dat3)

anova(mam_PC)
summary(mam_PC)
#Increasing either PC1 or PC2 increases mortality risk. Increasing residual mass
#decreases risk but to a comparatively small extent.


AICcTable <- AICc(mam_maxtemp, mam_meanTemp, mam_meanwindspeed, mam_rain, mam_PC) 
AICcTable$delta <- AICcTable$AICc - min(AICcTable$AICc)
AICcTable
#PCs are easily the best predictor. 




summary(mam_PC)
#Higher mass makes you less likely to die, unless PC1 is very large (i.e. in lousy conditions, it's better to be lighter already), but not a very strong effect in the
#face of PC1 and PC2 which are insanely strong!


coefmam <- coef(mam_PC)




#What if I make 3 plots, each holding a different all but one of the varibales at their mean?
#Resid Mass
dat3%>% group_by(ThermoReg) %>% summarize(
                                          min(PC13day), 
                                          max(PC13day))

newdata1 <- data.frame(ThermoReg= c(rep("Poikilotherm", 80), rep("Intermediate", 80)), 
                      PC13day= c(rep(seq(-3.39, 1.8, length.out = 40), 2), rep(seq(-3.39, 1.8,length.out=40), 2)) ,
                      Predicted=NA)
#predict the poikilotherms
newdata1$Predicted[newdata1$ThermoReg=="Poikilotherm"] <- newdata1$PC1[newdata1$ThermoReg=="Poikilotherm"]*coefmam[2]  #PC1
  

#Predict the intermediates
newdata1$Predicted[newdata1$ThermoReg=="Intermediate"] <- newdata1$PC1[newdata1$ThermoReg=="Intermediate"]*coefmam[2] + #PC1
  coefmam[1] #Intermediate

newdata1$ThermoReg <- factor(newdata1$ThermoReg, levels=c("Poikilotherm", "Intermediate"))
ggplot(newdata1, aes(x=PC13day, y=Predicted, linetype=ThermoReg))+
  geom_line(color="black")+
  labs(x="Mean PC1 over 3 days", y="Mortality risk", color="", linetype="" )+
  ggthemes::theme_few(base_size=16, base_family="serif")+
  scale_color_grey()+
  theme(legend.position = c(0.8, 0.8))
  
#WHen temperatures are low, and it's kind of windy, the nestlings are very likely to die. 

ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Mortality risk with PC1.jpeg", units="in", width=8, height=4, device="jpeg")

