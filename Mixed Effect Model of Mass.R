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

#Create a precipitation T/F 
dat$TotalPrecip2 <- 0
dat$TotalPrecip2[dat$TotalPrecip>0]<- 1


#Take all the airstrip grids out because those nestlings aren't a part of your 
dat2 <- dat %>% filter (substring(NestID, 1,2)!="AG")



#Get the data ready for the model by making a subset of the data that includes
#only nestling measurements where we know everything necessary to put into the
#model
dat3 <- dat2 %>% filter (!is.na(Mass) & !is.na(Age) & !is.na(ThermoReg) & Age<=12 )


ggplot(dat3, aes(x=Age, y=Mass))+
  geom_point()+
  stat_smooth(method="lm", formula=y~poly(x, 3))+
  geom_smooth(method="lm")
#Either way could fit pretty well. 



M.lm <- gls(Mass ~ poly(Age, 3), data=dat3)

library(nlme)
M.gls1 <- gls(Mass ~ poly(Age, 3), weights = varFixed(~Age), data=dat3)
M.gls2 <- gls(Mass ~ poly(Age, 3), weights = varIdent(~1|as.factor(Age)), data=dat3)

AICc(M.lm, M.gls1, M.gls2)

anova(M.lm, M.gls1, M.gls2)
#The clear winnter is M.gls1--- we want variance to increase with age!!!

summary(M.gls1)
plot(M.lm)
plot(M.gls1)
plot(M.gls2)

#Should we use the polynomial or a linear? 

M.gls1_3 <- gls(Mass ~ poly(Age, 3), weights = varFixed(~Age), data=dat3)
M.gls1_2 <- gls(Mass ~ poly(Age, 2), weights = varFixed(~Age), data=dat3)
M.gls1_1 <- gls(Mass ~ poly(Age, 1), weights = varFixed(~Age), data=dat3)
AICc(M.gls1_1, M.gls1_2, M.gls1_3)
#OK We will use the 3rd order


mamMass <- gls(Mass ~ poly(Age, 3), weights = varFixed(~Age), data=dat3)

dat3$ResidMass <- resid(mamMass)


#Do you tend to have lower mass when there was lousy weather 2 days prior, and
#does that effect depend on your thermoregulatory strategy?
ggplot(dat3, aes(y=ResidMass, x=MeanTemp))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)


ggplot(dat3, aes(y=ResidMass, x=MaxTemp))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

ggplot(dat3, aes(y=ResidMass, x=meanwindspeed))+
  geom_point()+
  geom_line(aes(color=NestlingID), alpha=0.6, show.legend = F)
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

ggplot(dat3, aes(y=ResidMass, x=as.factor(TotalPrecip2)))+
  geom_point()+
  #geom_smooth(method="lm")+
  facet_grid(~ThermoReg)



mod_mass <- lm(ResidMass ~ MaxTemp*ThermoReg , data=dat3)
plot(mod_mass)
summary(mod_mass)

ggplot(dat3, aes(x=MaxTemp, y=ResidMass))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

