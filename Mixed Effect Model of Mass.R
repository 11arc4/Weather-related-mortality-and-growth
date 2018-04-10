#Load up our packages
library(ggplot2) #for making plots
library(dplyr) #for subsetting the data
library(lme4) #for doing the mixed effect models
library(car) # for testing significance

#Load in the data
dat <- read.csv("file:///C:/Users/11arc/Documents/Kennedy's 537 Thesis Project/Nestling and Weather Data.csv", as.is=T)
dat <- read.csv("TRESDATAwithweather.csv")
#relabel the columns more clearly. 
names(dat)[20:22]<- c("MaxTemp", "MinTemp", "MeanTemp")
names(dat)[25:26]<- c("TotalRain", "TotalPrecip")

str(dat)
#make nestling ID and Nest ID catagories, because they are currently characters 
dat$NestID <- as.factor(dat$NestID)
dat$NestlingID <- as.factor(dat$NestlingID)

#Create a thermoregulatory strategy catagory
dat$ThermoReg <- NA
dat$ThermoReg[which(dat$Age<=6)] <- "Poikilotherm"
dat$ThermoReg[which(dat$Age<=8 & dat$Age>6)] <- "Intermediate"
dat$ThermoReg[which(dat$Age>8)] <- "Endotherm"
dat$ThermoReg <- as.factor(dat$ThermoReg)

#Take all the airstrip grids out because those nestlings aren't a part of your 
dat2 <- dat %>% filter (substring(NestID, 1,2)!="AG")



#Get the data ready for the model by making a subset of the data that includes
#only nestling measurements where we know everything necessary to put into the
#model



dat3 <- dat2 %>% filter (!is.na(Mass) & !is.na(Age) & !is.na(ThermoReg) & FledgeSuccess>0 )
dat3$AlivetoEnd<- "NO"

for (i  in 1:nrow(dat3)){
  ID <- dat3$NestlingID[i]
  any(dat3$NestlingID==ID & dat3$Age>=12)
  dat3$AlivetoEnd <- "YES"
}

NoSelection <- dat3 %>% filter(AlivetoEnd=="YES" & Age<13)

ggplot(NoSelection, aes(x=Age, y=Mass))+
  geom_point()+
  #geom_smooth(method="loess")+
  stat_smooth(method="lm", formula= y~poly(x, 3))

NoSelection$fAge <- factor(NoSelection$Age)
vf <- varIdent(form= ! 1 | fAge)
mod3 <- gls(Mass ~ poly(Age, degree=3),weights=varIdent(form = !1|fAge), data=NoSelection)
plot(mod3) #this looks quite not normal. 
hist(resid(mod3)) #this doesn't look horrible but also isn't good
shapiro.test(resid(mod3))
plot(resid(mod3)~NoSelection$Age) #We are a fair bit better at predicting for the youngest nestlings for a polynomial.


mod2 <- lm(Mass ~ poly(Age, degree=2), data=NoSelection)
mod1 <- lm(Mass ~ poly(Age, degree=1), data=NoSelection)


AICc(mod1, mod2, mod3)
#3rd order polynomial for the win!



#There are pretty serious issues with including both the polynomial and the random effects. It really doesn't want to converge, even excluding nestling effects. 
mod <- lmer(Mass ~ poly(Age, degree=3) + 1|NestID, data=dat3)
plot(mod)
summary(mod)

mod <- lmer(Mass ~ Age+ I(Age)^2 + 1|NestID/NestlingID, data=dat3)
mod <- lmer(Mass ~ Age + 1|NestID/NestlingID, data=dat3)




#What if we model the residual of each of those things? 

dat3$ResidMass <- resid(mod3) #residual mass from the 3rd order polynomial


#Do you tend to have lower mass when there was lousy weather 2 days prior, and
#does that effect depend on your thermoregulatory strategy?
ggplot(dat3, aes(y=ResidMass, x=MeanTemp))+
  geom_point()+
  geom_smooth()+
  facet_grid(~ThermoReg)

mod_mass <- lmer(ResidMass~MeanTemp + 1|NestID/NestlingID, data=dat3)
plot(mod_mass)
hist(resid(mod_mass))
plot(resid(mod_mass)~dat3$NestlingID) 
plot(resid(mod_mass)~dat3$NestID) 
plot(resid(mod_mass)~dat3$MeanTemp) 
#Looks like it fits ok

summary(mod_mass)
#We should drop nestling random effect

mod_mass <- lm(ResidMass ~ MaxTemp*ThermoReg , data=dat3)
summary(mod_mass)

ggplot(dat3, aes(x=MaxTemp, y=ResidMass))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

