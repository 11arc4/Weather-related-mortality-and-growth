setwd("C:/Users/11arc/Dropbox/Kennedy Everitt Honors Thesis/Statistical Models/Survival Models")
dat <- read.csv("Nestling Survival Data.csv", as.is=T)  #already excluding AG
library(dplyr)
library(survival)
library(survminer)
library(coxme)
library(lmtest)
library(MuMIn)


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


#Now we need to calculated a PC axis for the weather variables.
# https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
#first we need to make sure all the variables are centered and scaled
library(stats)
weatherVar2 <- dat2[,10:19] 
#we are going to use all the weather variables because that's what we did in the
#other analyses and it's important to be consistant, even thoguh the results are
#no different either way

weather.pca <- prcomp(weatherVar2, 
                      center=T, 
                      scale=T)

plot(weather.pca, type="lines")
summary(weather.pca)
#By using weather PCs 1 and 2 we can capture 72% of the weather variation. We will use those 2. 
dat2$PC1 <- predict(weather.pca, dat2[,10:19])[,1]
dat2$PC2 <- predict(weather.pca, dat2[,10:19])[,2]


#OK we are ready to make our global model now. It's huge because we have 2 sets
#of weather variables that need to make it in This may be an issue. What if I
#break it up and consider PC1 and PC2 Seperately? The real issue is probably the
#endotherms. They have such high survival that there aren't many deaths and shit
#doesn't converge.
dat3 <- dat2 %>% filter(ThermoReg !="Endotherm" & WhyFail!="PREDATION")
dat3$ThermoReg <- factor(dat3$ThermoReg, levels= c("Poikilotherm", "Intermediate")) #Need this otherwise it tries to figure it out for endotherms too but there's no data

dat3$TotalPrecip2 <- 0
dat3$TotalPrecip2[dat3$TotPrecip>0] <- 1
dat3$TotalPrecip2 <- factor(dat3$TotalPrecip2)

##########################################################################
##################Does Max temp 2 days prior predict survival? 

#answers based on random effects
mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MaxTemp+ ResidMass1*MaxTemp + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
mod1
#nestlings ID SD is greater than the variance explained for both our random
#effects-- therefore I think we can at least start by getting rid of the
#nestlingID
mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MaxTemp+ ResidMass1*MaxTemp + (1|NestID), na.action = "na.fail",
               data = dat3)
mod2
#NestID SD >var so we need to keep it. 


options(na.action="na.fail")
dredge(mod2)
#there is a clear winner here. 

mam_maxtemp <- coxme( Surv(time=Time1, time2=Time2, Dead) ~  MaxTemp*ResidMass1  +ThermoReg + (1|NestID), na.action = "na.fail",
              data = dat3)
summary(mam_maxtemp)

AICc(mam_)
AICc(mod2)




##########################################################################
##################Does mean temp 2 days prior predict survival? 

#answers based on random effects
mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MeanTemp+ ResidMass1*MeanTemp + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
mod1
#Don't need nestling random effect


mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*MeanTemp+ ResidMass1*MeanTemp + (1|NestID), na.action = "na.fail",
               data = dat3)
mod2
#Do need nest random effect. 

dredge(mod2)
#Not really clear whether or not we need residual mass. Since that's what I'm interested in I will leave it in though. (they're both in the top models)

mam_meanTemp <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg+ResidMass1 + MeanTemp + (1|NestID), na.action = "na.fail",
                       data = dat3)
summary(mam_meanTemp)

#Higher residual mass lowers your risk of death. High temperatures also lower
#your risk of death. Intermediates are much more likely to die than
#poikilotherms.



##########################################################################
##################Does whether it rains or not 2 days prior predict survival? 


mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*TotalPrecip2+ ResidMass1*TotalPrecip2 + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
summary(mod1)
#Definitely don't need the nestling random effect

mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*TotalPrecip2+ ResidMass1*TotalPrecip2 + (1|NestID), na.action = "na.fail",
               data = dat3)
summary(mod2)
#DO need nest, very much so

dredge(mod2)
#Only one best model

mam_rain <- coxme( Surv(time=Time1, time2=Time2, Dead) ~  ResidMass1*TotalPrecip2 + (1|NestID), na.action = "na.fail",
                   data = dat3)
summary(mam_rain)
#Much more likely to die is it's raining. But also more likely to die on a
#non-rainy day if you're light. Doesn't matter how old you are-- everyone is
#equally affected by rain


##############################################################################
##################Does mean windspeed 2 days prior predict survival? 
mod1 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*meanwindspeed+ ResidMass1*meanwindspeed + (1|NestID/NestlingID), na.action = "na.fail",
               data = dat3)
summary(mod1)
#No need for nestlings random effect

mod2 <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*meanwindspeed+ ResidMass1*meanwindspeed + (1|NestID), na.action = "na.fail",
               data = dat3)
summary(mod2)
#definitely need nest random effect

dredge(mod2)

mam_meanwindspeed <- coxme( Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg*meanwindspeed + (1|NestID), na.action = "na.fail",
               data = dat3)
summary(mam_meanwindspeed)
###Intermmediates have MUCH higher mortality risk, but are also less effected by
###windspeed. This model really doesn't make much sense.


##############################################################################
##################Does the combination of weather conditions (measured with PC1
##################and PC2) 2 days prior predict survival?

#this is a little hinky-- I had to drop the nestling random ID and wasn't able
#to test whether it was important for this model because of too many parameters.
#This could definitely be worse though since it was so unimportant for
#everything else.
mod1 <- coxme(Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg+ResidMass1*PC1 + ResidMass1*PC2 + (1|NestID), na.action = "na.fail",
              data = dat3)
summary(mod1)
dredge(mod1)
#I'm taking the best model with residual mass-- it's my second best model but
#still just within 2 delta AICc. ALSo the previous model won't converge so
#probabl isn't as good as it seems.

mam_PC <- coxme(Surv(time=Time1, time2=Time2, Dead) ~ ThermoReg+ResidMass1+ PC2 + PC1  + (1|NestID), na.action = "na.fail",
                data = dat3)
summary(mam_PC)
#Increasing either PC1 or PC2 increases mortality risk. Increasing residual mass
#decreases risk but to a comparatively small extent.


AICcTable <- AICc(mam_maxtemp, mam_meanTemp, mam_meanwindspeed, mam_rain, mam_PC) 
AICcTable$delta <- AICcTable$AICc - min(AICcTable$AICc)
AICcTable
#PCs are easily the best predictor. 




summary(mam_PC)
#Higher mass makes you less likely to die, but not a very strong effect in the
#face of PC1 and PC2 which are insanely strong!




dat3$Predicted[dat3$ThermoReg=="Poikilotherm"] <- dat3$ResidMass1[dat3$ThermoReg=="Poikilotherm"]*coef(mam_PC)[2]+ dat3$PC2[dat3$ThermoReg=="Poikilotherm"]*coef(mam_PC)[3] + dat3$PC1[dat3$ThermoReg=="Poikilotherm"]*coef(mam_PC)[4]
dat3$Predicted[dat3$ThermoReg=="Intermediate"] <- coef(mam_PC)[1] + dat3$ResidMass1[dat3$ThermoReg=="Intermediate"]*coef(mam_PC)[2]+ dat3$PC2[dat3$ThermoReg=="Intermediate"]*coef(mam_PC)[3] + dat3$PC1[dat3$ThermoReg=="Intermediate"]*coef(mam_PC)[4]


ggplot(dat3, aes(x=Time1, y=Predicted, group=ThermoReg))+
  geom_point(aes(color=ResidMass1), alpha=0.5)+
  labs(x="Julian Date", y="Predicted Mortality Risk", color="Residual Mass")
  


#What if I make 3 plots, each holding a different all but one of the varibales at their mean?
#Resid Mass
dat3%>% group_by(ThermoReg) %>% summarize(min(ResidMass1), 
                                          max(ResidMass1))

newdata1 <- data.frame(ThermoReg= c(rep("Poikilotherm", 40), c(rep("Intermediate", 40))), 
                      ResidMass1=c(seq(-6.34, 6.16, length.out = 40), seq(-5.59, 6.11, length.out = 40)), 
                      PC1=mean(dat3$PC1), 
                      PC2=mean(dat3$PC2), 
                      Predicted=NA)
newdata1$Predicted[newdata1$ThermoReg=="Poikilotherm"] <- newdata1$ResidMass1[newdata1$ThermoReg=="Poikilotherm"]*coef(mam_PC)[2]+ newdata1$PC2[newdata1$ThermoReg=="Poikilotherm"]*coef(mam_PC)[3] + newdata1$PC1[newdata1$ThermoReg=="Poikilotherm"]*coef(mam_PC)[4]
newdata1$Predicted[newdata1$ThermoReg=="Intermediate"] <- coef(mam_PC)[1] + newdata1$ResidMass1[newdata1$ThermoReg=="Intermediate"]*coef(mam_PC)[2]+ newdata1$PC2[newdata1$ThermoReg=="Intermediate"]*coef(mam_PC)[3] + newdata1$PC1[newdata1$ThermoReg=="Intermediate"]*coef(mam_PC)[4]

ggplot(newdata1, aes(x=ResidMass1, y=Predicted, color=ThermoReg))+
  geom_line()+
  labs(x="Residual Mass", y="Predicted Mortality Risk", color="")+
  ggthemes::theme_few(base_family = "serif", base_size = 16)

#PC1
dat3%>% group_by(ThermoReg) %>% summarize(min(PC1), 
                                          max(PC1))
newdata2 <- data.frame(ThermoReg= c(rep("Poikilotherm", 40), c(rep("Intermediate", 40))), 
                       ResidMass1=mean(dat3$ResidMass1), 
                       PC1=c(seq(-3.24, 3.03, length.out = 40),seq(-2.75, 4.67, length.out = 40)), 
                       PC2=mean(dat3$PC2), 
                       Predicted=NA)
newdata2$Predicted[newdata2$ThermoReg=="Poikilotherm"] <- newdata2$ResidMass1[newdata2$ThermoReg=="Poikilotherm"]*coef(mam_PC)[2]+ newdata2$PC2[newdata2$ThermoReg=="Poikilotherm"]*coef(mam_PC)[3] + newdata2$PC1[newdata2$ThermoReg=="Poikilotherm"]*coef(mam_PC)[4]
newdata2$Predicted[newdata2$ThermoReg=="Intermediate"] <- coef(mam_PC)[1] + newdata2$ResidMass1[newdata2$ThermoReg=="Intermediate"]*coef(mam_PC)[2]+ newdata2$PC2[newdata2$ThermoReg=="Intermediate"]*coef(mam_PC)[3] + newdata2$PC1[newdata2$ThermoReg=="Intermediate"]*coef(mam_PC)[4]

ggplot(newdata2, aes(x=PC1, y=Predicted, color=ThermoReg))+
  geom_line()+
  labs(x="PC1", y="Predicted Mortality Risk", color="")+
  ggthemes::theme_few(base_family = "serif", base_size = 16)
#PC1 is hugely influential

#PC2
dat3%>% group_by(ThermoReg) %>% summarize(min(PC2), 
                                          max(PC2))
newdata3 <- data.frame(ThermoReg= c(rep("Poikilotherm", 40), c(rep("Intermediate", 40))), 
                       ResidMass1=mean(dat3$ResidMass1), 
                       PC2=c(seq(-1.67, 3.5, length.out = 40),seq(-5.27, 3.5, length.out = 40)), 
                       PC1=mean(dat3$PC1), 
                       Predicted=NA)
newdata3$Predicted[newdata3$ThermoReg=="Poikilotherm"] <- newdata3$ResidMass1[newdata3$ThermoReg=="Poikilotherm"]*coef(mam_PC)[2]+ newdata3$PC2[newdata3$ThermoReg=="Poikilotherm"]*coef(mam_PC)[3] + newdata3$PC1[newdata3$ThermoReg=="Poikilotherm"]*coef(mam_PC)[4]
newdata3$Predicted[newdata3$ThermoReg=="Intermediate"] <- coef(mam_PC)[1] + newdata3$ResidMass1[newdata3$ThermoReg=="Intermediate"]*coef(mam_PC)[2]+ newdata3$PC2[newdata3$ThermoReg=="Intermediate"]*coef(mam_PC)[3] + newdata3$PC1[newdata3$ThermoReg=="Intermediate"]*coef(mam_PC)[4]

ggplot(newdata3, aes(x=PC2, y=Predicted, color=ThermoReg))+
  geom_line()+
  labs(x="PC2", y="Predicted Mortality Risk", color="")+
  ggthemes::theme_few(base_family = "serif", base_size = 16)



