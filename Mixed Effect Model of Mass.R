#Load up our packages
library(ggplot2) #for making plots
library(dplyr) #for subsetting the data
library(lme4) #for doing the mixed effect models
library(nlme)

###############
#Load in the data
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Nestling mass and Weather Data.csv", as.is=T)

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
dat3 <- dat2 %>% filter (!is.na(Mass) & !is.na(Age) & !is.na(ThermoReg) & Age<=12 & !is.na(PC13day))


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




########################################################################
#Do you tend to have lower mass when there was lousy weather 2 days prior, and
#does that effect depend on your thermoregulatory strategy?
ggplot(dat3, aes(y=ResidMass, x=MeanTemp3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)


ggplot(dat3, aes(y=ResidMass, x=MaxTemp3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

ggplot(dat3, aes(y=ResidMass, x=MeanWindspeed3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

ggplot(dat3, aes(y=ResidMass, x=TotalRainFall3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)


ggplot(dat3 %>% filter(HatchDate<170), aes(y=ResidMass, x=HatchDate))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)



##########################################################
#############Does max temperature perdict residual mass?
mod1 <- lmer(ResidMass ~ MaxTemp3day*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$MaxTemp)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Don't need nestling ID as a random effect

mod2 <- lmer(ResidMass ~ MaxTemp3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Do need nest ID as a random effect

dredge(mod2)

mam_maxtemp <- lmer(ResidMass ~ MaxTemp3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_maxtemp)



##########################################################
#############Does mean temperature perdict residual mass?
mod1 <- lmer(ResidMass ~ MeanTemp3day*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)

plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$MeanTemp3day)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Don't need nestling ID

mod2 <- lmer(ResidMass ~ MeanTemp3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Should keep Nest ID as a random effect

dredge(mod2)
#Full model is easily the best predictor

mam_meantemp <- lmer(ResidMass ~ MeanTemp3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_meantemp)
#Increasing mean temp increases body mass for endotherms, doesn't do anything
#for intermediates, and decreases it for poikilotherms.


##########################################################
#############Does whether is rains or not predict residual mass?
mod1 <- lmer(ResidMass ~ TotalRainFall3day*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$TotalRainFall3day)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)

summary(mod1)
#Can drop the random nestling ID effect

mod2 <- lmer(ResidMass ~ TotalRainFall3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Should keep the nestID effect

dredge(mod2)
#Best model is the full model. However, with delta=1.97 is the much simpler just total precipitation

mam_rain <- lmer(ResidMass ~ TotalRainFall3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mam_rain)

##########################################################
#############Does meanwindspeed predict residual mass?
mod1 <- lmer(ResidMass ~ MeanWindspeed3day*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$MeanWindspeed3day)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)

summary(mod1)
#Good to drop the nestling ID random effect

mod2 <- lmer(ResidMass ~ MeanWindspeed3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#need to keep the NestID random effect

dredge(mod2)

mam_windspeed <- lmer(ResidMass ~ MeanWindspeed3day + (1|NestID), data=dat3, REML=FALSE)
summary(mam_windspeed)


########################################################## Does the combination
#############of weather variables (PC1 and PC2) predict residual mass better
#############than any one alone?
mod1 <- lmer(ResidMass ~ PC13day*ThermoReg+ PC23day*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
plot(mod1)
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$PC13day)
plot(resid(mod1)~dat3$PC23day)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Should drop the nestling ID

mod2 <- lmer(ResidMass ~ PC13day*ThermoReg+ PC23day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)
summary(mod2)
#Need to keep the NestID

dredge(mod2)

#Full model is the best by a long shot
dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c("Poikilotherm", "Endotherm", "Intermediate"))
dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c(  "Intermediate", "Poikilotherm","Endotherm"))


mam_PC <- lmer(ResidMass ~ PC13day*ThermoReg+ PC23day + (1|NestID), data=dat3, REML=FALSE)
summary(mam_PC)


AICcTable <- AICc(mam_maxtemp, mam_meantemp, mam_rain, mam_windspeed, mam_PC) 
AICcTable$delta <- AICcTable$AICc-min(AICcTable$AICc) 
AICcTable

#Total rainfall and PCs are now similarly good at predicting things. 


mam <- mam_rain



###########################
###Let's try to make an informative plot about what happens to residual mass as rainfaill changes
dat3%>% group_by(ThermoReg) %>% summarise(min(TotalRainFall3day), max(TotalRainFall3day) )
summary(dat3$PC23day)
newdata<- data.frame(ThermoReg=factor(c(rep("Poikilotherm", 30), rep("Intermediate", 30), rep("Endotherm", 30))), levels=c("Poikilotherm", "Intermediate", "Endotherm"), 
                     TotalRainFall3day=rep(seq(0, 23.4, length.out = 30), 3), 
                     Predicted=NA, 
                     lcl=NA, 
                     ucl=NA)

#newdata$Predicted <- predict(mam, newdata,re.form=~0) #level=0 tells us to ignore the random effect and just pick the mean nest!
library(boot)
b3 <- bootMer(mam_rain,FUN=function(x) predict(x,newdata=newdata,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)
#### Confidence and prediction intervals for *unobserved* levels
bootsum <- function(x,ext="_1") {
  d <- data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975)))))
  d <- setNames(d,paste0(c("bpred","lwr","upr"),ext))
  return(d)
}
newdata[,4:6]<- t(bootsum(b3,"_3"))



ggplot()+
  #geom_point()+
  geom_line(data=newdata, aes(x=TotalRainFall3day, y=Predicted))+
  geom_ribbon(data=newdata, aes(x=TotalRainFall3day, ymin=lcl, ymax=ucl), alpha=0.3)+
  facet_grid(~ThermoReg, labeller = as_labeller(ThermoRegLabels) )+
  labs(x="Total rainfall (mm)", y="Residual mass")+
  ggthemes::theme_few(base_family = "serif", base_size = 16)

ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Resdidual mass with rain.jpeg", units="in", width=8, height=4, device="jpeg")










###Let's try to make an informative plot about what happens to residual mass as PC1 and PC2 change. 
dat3%>% group_by(ThermoReg) %>% summarise(min(PC13day), max(PC13day) )
summary(dat3$PC23day)
newdata2<- data.frame(ThermoReg=factor(c(rep("Poikilotherm", 60), rep("Intermediate", 60), rep("Endotherm", 60)), levels=c("Poikilotherm", "Intermediate", "Endotherm")), 
                     PC13day=c(rep(seq(-3.39, 1.92, length.out = 30), 2), rep(seq(-3.39, 1.8, length.out = 30), 2), rep(seq(-3.39, 2.09, length.out = 30), 2)), 
                     PC23day=as.factor(rep(c(rep(-1.30, 30), rep(1.51, 30)), 3)), 
                     Predicted=NA, 
                     lcl=NA, 
                     ucl=NA)

#newdata$Predicted <- predict(mam, newdata,re.form=~0) #level=0 tells us to ignore the random effect and just pick the mean nest!
library(boot)
b3 <- bootMer(mam_PC,FUN=function(x) predict(x,newdata=newdata2,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)
#### Confidence and prediction intervals for *unobserved* levels
bootsum <- function(x,ext="_1") {
  d <- data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975)))))
  d <- setNames(d,paste0(c("bpred","lwr","upr"),ext))
  return(d)
}
newdata2[,4:6]<- t(bootsum(b3,"_3"))

ThermoRegLabels <- c(`Poikilotherm`= "Poikilotherm", 
                     `Intermediate`= "Intermediate", 
                     `Endotherm`= "Homeotherm")


ggplot()+
  #geom_point()+
  geom_line(data=newdata2, aes(x=PC13day, y=Predicted, color=PC23day))+
  geom_ribbon(data=newdata2, aes(x=PC13day, ymin=lcl, ymax=ucl, fill=PC23day), alpha=0.3)+
  facet_grid(~ThermoReg, labeller = as_labeller(ThermoRegLabels) )+
  labs(x="PC1", y="Residual mass", color="PC2", fill="PC2")+
  ggthemes::theme_few(base_family = "serif", base_size = 16)

ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Resdidual mass with PC1.jpeg", units="in", width=8, height=4, device="jpeg")

#High PC1= warmer, less rain, less wind
#higher PC2= cooler, wetter, much less windy







