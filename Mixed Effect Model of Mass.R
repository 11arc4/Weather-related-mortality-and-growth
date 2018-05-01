#Load up our packages
library(ggplot2) #for making plots
library(dplyr) #for subsetting the data
library(lme4) #for doing the mixed effect models
library(nlme)

###############
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


####Calculate PC values for weather characteristics. 

weatherVar2 <- dat3[,c(20:22,26:29)] 
#we are going to use all the weather variables because that's what we did in the
#other analyses and it's important to be consistant, even thoguh the results are
#no different either way

weather.pca <- prcomp(weatherVar2, 
                      center=T, 
                      scale=T)

plot(weather.pca, type="lines")
summary(weather.pca)
#By using weather PCs 1 and 2 we can capture 72% of the weather variation. We will use those 2. 
dat3$PC1 <- predict(weather.pca, dat3[,c(20:22,26:29)])[,1]
dat3$PC2 <- predict(weather.pca, dat3[,c(20:22,26:29)])[,2]




########################################################################
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


##########################################################
#############Does max temperature perdict residual mass?
mod1 <- lmer(ResidMass ~ MaxTemp*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$MaxTemp)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Don't need nestling ID as a random effect

mod2 <- lmer(ResidMass ~ MaxTemp*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Do need nest ID as a random effect

dredge(mod2)

mam_maxtemp <- lmer(ResidMass ~ MaxTemp*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_maxtemp)
#Poikilotherm's mass is more effected by max temp than intermediates or
#endotherms. HIgher max temp= lower residual mass, particulaly for
#poikilotherms-- likely due to weak selection.


##########################################################
#############Does mean temperature perdict residual mass?
mod1 <- lmer(ResidMass ~ MeanTemp*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)

plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$MeanTemp)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Don't need nestling ID

mod2 <- lmer(ResidMass ~ MeanTemp*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Should keep Nest ID as a random effect

dredge(mod2)
#Full model is easily the best predictor

mam_meantemp <- lmer(ResidMass ~ MeanTemp*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_meantemp)
#Increasing mean temp increases body mass for endotherms, doesn't do anything
#for intermediates, and decreases it for poikilotherms.


##########################################################
#############Does whether is rains or not predict residual mass?
mod1 <- lmer(ResidMass ~ TotalPrecip2*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$TotalPrecip2)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)

summary(mod1)
#Can drop the random nestling ID effect

mod2 <- lmer(ResidMass ~ TotalPrecip2*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Should keep the nestID effect

dredge(mod2)
#Best model is the full model. However, with delta=1.97 is the much simpler just total precipitation

mam_rain <- lmer(ResidMass ~ TotalPrecip2*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_rain)

##########################################################
#############Does meanwindspeed predict residual mass?
mod1 <- lmer(ResidMass ~ meanwindspeed*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$meanwindspeed)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)

summary(mod1)
#Good to drop the nestling ID random effect

mod2 <- lmer(ResidMass ~ meanwindspeed*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#need to keep the NestID random effect

dredge(mod2)
#I'm going with the top model but the next best is only 1.17 away and doesn't include thermoreg at all. 

mam_windspeed <- lmer(ResidMass ~ meanwindspeed*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_windspeed)


########################################################## Does the combination
#############of weather variables (PC1 and PC2) predict residual mass better
#############than any one alone?
mod1 <- lmer(ResidMass ~ PC1*ThermoReg+ PC2*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1)
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$PC1)
plot(resid(mod1)~dat3$PC2)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Should drop the nestling ID

mod2 <- lmer(ResidMass ~ PC1*ThermoReg+ PC2*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Need to keep the NestID

dredge(mod2)

#Full model is the best by a long shot

mam_PC <- lmer(ResidMass ~ PC1*ThermoReg+ PC2*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_PC)


AICcTable <- AICc(mam_maxtemp, mam_meantemp, mam_rain, mam_windspeed, mam_PC) 
AICcTable$delta <- AICcTable$AICc-min(AICcTable$AICc) 
AICcTable

#PC is the best predictor of Mass


mam <- mam_PC






