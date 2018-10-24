#Load up our packages
library(tidyverse)
library(lme4) #for doing the mixed effect models
library(nlme)
library(MuMIn)

###############
#Load in the data
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Nestling mass and Weather Data 3day.csv", as.is=T)

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


plot(M.gls1_3)
plot(M.gls1_2)
plot(M.gls1_1)

fit <- nls(Mass ~ SSlogis(Age, Asym, xmid, scal), data = dat3)

fit <- nls(Mass ~ SSlogis(Age, Asym, xmid, scal), weights = Age, data = dat3)
plot(fit)

AICc(M.gls1_1, M.gls1_2, M.gls1_3, M.gls1_4, fit)
#OK We will use the 3rd order


mamMass <- gls(Mass ~ poly(Age, 3), weights = varFixed(~Age), data=dat3)
anova(mamMass)
dat3$ResidMass <- resid(mamMass)


ggplot(dat3, aes(x=Age, y=Mass))+
  geom_jitter(alpha=0.3, width=0.1)+
  geom_smooth(method=lm, formula=y~poly(x,3), se=F, color="black")+
  theme_classic(base_family = "serif", base_size =  12)+
  scale_x_continuous(breaks=seq(0,13,2))
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Bodymass by age plot.jpeg", units="in", width=4, height=3, device="jpeg")


dat4 <- dat3 %>% group_by(NestlingID) %>% mutate(ResidMass_scaled=ResidMass-mean(ResidMass))


########################################################################
#Do you tend to have lower mass when there was lousy weather 2 days prior, and
#does that effect depend on your thermoregulatory strategy?
ggplot(dat4, aes(y=ResidMass_scaled, x=MeanTemp3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)


ggplot(dat4, aes(y=ResidMass_scaled, x=MaxTemp3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

ggplot(dat4, aes(y=ResidMass_scaled, x=MeanWindspeed3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)

ggplot(dat4, aes(y=ResidMass_scaled, x=TotalRainFall3day))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)


ggplot(dat4 %>% filter(HatchDate<170), aes(y=ResidMass_scaled, x=HatchDate))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~ThermoReg)



##########################################################
#############Does max temperature perdict residual mass?
mod1 <- lmer(ResidMass_scaled ~ MaxTemp3day*ThermoReg + (1|NestID/NestlingID), data=dat4, REML=FALSE)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat4$MaxTemp3day)
plot(resid(mod1)~dat4$ThermoReg)
plot(resid(mod1)~dat4$NestID)


summary(mod1)
#Don't need nestling ID as a random effect

mod2 <- lmer(ResidMass_scaled ~ MaxTemp3day*ThermoReg + (1|NestID), data=dat4, REML=FALSE)
summary(mod2)
#Do not need nest ID as a random effect


mod3 <- lm(ResidMass_scaled ~ MaxTemp3day*ThermoReg, data=dat4, na.action = "na.fail")


dredge(mod3)

mam_maxtemp <- lm(ResidMass_scaled ~ MaxTemp3day*ThermoReg, data=dat4)
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
anova(mam_rain)

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
mod1 <- lmer(ResidMass_scaled ~ PC13day*ThermoReg+ PC23day*ThermoReg + (1|NestID/NestlingID), data=dat4, REML=FALSE)
plot(mod1)
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~dat3$PC13day)
plot(resid(mod1)~dat3$PC23day)
plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)


summary(mod1)
#Should drop the nestling ID

mod2 <- lmer(ResidMass_scaled ~ PC13day*ThermoReg+ PC23day*ThermoReg + (1|NestID), data=dat4, REML=FALSE)
summary(mod2)
#don't need to keep the NestID

mod3 <- lm(ResidMass_scaled ~ PC13day*ThermoReg+ PC23day*ThermoReg, data=dat4)

dredge(mod3)
anova(mod3)


dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c("Poikilotherm", "Endotherm", "Intermediate"))
dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c(  "Intermediate", "Poikilotherm","Endotherm"))


mam_PC <- lm(ResidMass_scaled ~ PC13day*ThermoReg+ PC23day, data=dat4)
summary(mam_PC)

plot(mam_PC)


BM_PC1 <- ggplot(dat4, aes(x=PC13day, y=ResidMass_scaled))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  facet_grid(~ThermoReg)+
  labs(x="PC1 (warmer, less rain)", y="Residual body mass")+
  theme_classic(base_family = "serif", base_size = 12)
  
BM_PC2 <- ggplot(dat4, aes(x=PC23day, y=ResidMass_scaled))+
  geom_point()+
  labs(x="PC2 (rainy, less wind)", y="Residual body mass")+
  geom_smooth(method="lm", color="black")+
  theme_classic(base_family = "serif", base_size = 12)
  
cowplot::plot_grid(BM_PC1, BM_PC2, nrow = 2, ncol=1, labels = c("a", "b"), label_fontfamily = "serif")
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/PC rescaled bodymass plot.jpeg", units="in", width=8, height=8, device="jpeg")

weather.pca <- readRDS("~/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Weather-related-mortality-and-growth/WeatherPCA.rds")
#PC1 increases with warmer days with little rain. 
#PC2 increases with more rain and less wind. 
summary(weather.pca)
rm(weather.pca)

AICcTable <- AICc(mam_maxtemp, mam_meantemp, mam_rain, mam_windspeed, mam_PC) 
AICcTable$delta <- AICcTable$AICc-min(AICcTable$AICc) 
AICcTable

#Total rainfall and PCs are now similarly good at predicting thing

mam <- mam_rain

r.squaredGLMM(mam_maxtemp )
r.squaredGLMM( mam_meantemp)
r.squaredGLMM(mam_rain )
r.squaredGLMM( mam_windspeed )
r.squaredGLMM( mam_PC )

#Of the single weather variables, weather is easilty the best with the best R2.
#PCs don't explain more than weather alone.

#Cite Nakagawa and Schielzeth 2012 for this pseudo R2 calculation

###########################
###Let's try to make an informative plot about what happens to residual mass as rainfaill changes
dat3%>% group_by(ThermoReg) %>% summarise(min(TotalRainFall3day), max(TotalRainFall3day) )
ThermoRegLabels <- c(`Poikilotherm`= "Poikilotherm", 
                     `Intermediate`= "Intermediate", 
                     `Endotherm`= "Homeotherm")

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

newdata$ThermoReg <- factor(newdata$ThermoReg, levels=c("Poikilotherm",
                                                        "Intermediate", 
                                                        "Endotherm"))

ggplot()+
  #geom_point()+
  geom_ribbon(data=newdata, aes(x=TotalRainFall3day, ymin=lcl, ymax=ucl, fill=ThermoReg), alpha=0.3)+
  geom_line(data=newdata, aes(x=TotalRainFall3day, y=Predicted, linetype=ThermoReg))+
  #facet_grid(~ThermoReg, labeller = as_labeller(ThermoRegLabels) )+
  scale_fill_grey(labels=c("Poikilotherm", "Intermediate", "Homeotherm"))+
  scale_linetype(labels=c("Poikilotherm", "Intermediate", "Homeotherm"))+
  
  labs(x="Total rainfall (mm)", y="Residual mass", fill="", linetype="")+
  theme_classic(base_family = "serif", base_size = 16)+
  theme(legend.position = c(0.2, 0.2))

ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Resdidual mass with rain.jpeg", units="in", width=5, height=4, device="jpeg")




ggplot()+
  #geom_point()+
  geom_ribbon(data=newdata, aes(x=TotalRainFall3day, ymin=lcl, ymax=ucl, fill=ThermoReg), alpha=0.3)+
  geom_line(data=newdata, aes(x=TotalRainFall3day, y=Predicted, linetype=ThermoReg))+
  #facet_grid(~ThermoReg, labeller = as_labeller(ThermoRegLabels) )+
  scale_fill_grey(labels=c("Poikilotherm", "Intermediate", "Homeotherm"))+
  scale_linetype(labels=c("Poikilotherm", "Intermediate", "Homeotherm"))+
  
  labs(x="Total rainfall (mm)", y="Residual mass", fill="", linetype="")+
  theme_classic(base_family = "serif", base_size = 16)+
  theme(legend.position = c(0.2, 0.2))

ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Resdidual mass with rain.jpeg", units="in", width=5, height=4, device="jpeg")



ggplot()+
  geom_point(data=dat3 %>% filter(ThermoReg=="Endotherm"), aes(x=TotalRainFall3day, y=ResidMass))+
  geom_ribbon(data=newdata %>% filter(ThermoReg=="Endotherm"), aes(x=TotalRainFall3day, ymin=lcl, ymax=ucl), alpha=0.3)+
  geom_line(data=newdata %>% filter(ThermoReg=="Endotherm"),aes(x=TotalRainFall3day, y=Predicted))+
  #facet_grid(~ThermoReg, labeller = as_labeller(ThermoRegLabels) )+
  labs(x="Total rainfall (mm)", y="", fill="", linetype="")+
  theme_classic( base_size = 20)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))


ggsave(filename="~/Masters Thesis Project/NACCB Conference/Presentation Figures/Nestling mass by rain.jpeg", units="in", width=6, height=5, device="jpeg")














###### Can we put everything in together? I think this is a bad idea because
#it's only when you are able to include the interaction with thermoreg that we
#even really expect to see something interesting.

cor(dat3[,c(20,22,23)])
#corelations between total rainfall and temp, and rain and wind are pretty
#strong. Wind and temp don't correlate.



mod1 <- lmer(ResidMass ~ TotalRainFall3day*ThermoReg+ + MaxTemp3day*ThermoReg + MeanWindspeed3day*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
summary(mod1)
plot(mod1) #this is OK
hist(resid(mod1)) #looks pretty good but there is a bit of a tail. 
shapiro.test(resid(mod1)) #this very conservative test says we aren't fitting. I'm thinking I'll ignore it for now but maybe will need to deal with this. 
plot(resid(mod1)~as.factor(dat3$TotalRainFall3day_2))
plot(resid(mod1)~dat3$MaxTemp3day)
plot(resid(mod1)~dat3$MeanWindspeed3day)

plot(resid(mod1)~dat3$ThermoReg)
plot(resid(mod1)~dat3$NestID)
plot(resid(mod1)~dat3$NestlingID)

#This looks fine. Drop the nestling ID beccause variance smaller than SD

mod2 <- lmer(ResidMass ~ TotalRainFall3day*ThermoReg+  MaxTemp3day*ThermoReg + MeanWindspeed3day*ThermoReg + (1|NestID), data=dat3, REML=FALSE)

AICc(mod1, mod2) #Hmmm this says  not to do that.... the differences are pretty close. We will stick to mod1

anova(mod1)
dredge(mod1)
#Best to keep everything. 

summary(mod1)

ggplot(dat3, aes(x=TotalRainFall3day, y=ResidMass))+
  geom_point()+
  geom_smooth()+
  facet_grid(~ThermoReg)

ggplot(dat3, aes(x=MaxTemp3day, y=ResidMass))+
  geom_point()+
  geom_smooth()+
  facet_grid(~ThermoReg)

ggplot(dat3, aes(x=MeanWindspeed3day, y=ResidMass))+
  geom_point()+
  geom_smooth()+
  facet_grid(~ThermoReg)





#OK now make the same model, but using rescaled variables to determine relative
#importance of each term, for each age.
dat3$TotalRainFall3day_2 <- scale(dat3$TotalRainFall3day)
dat3$MaxTemp3day_2 <- scale(dat3$MaxTemp3day)
dat3$MeanWindspeed3day_2 <- scale(dat3$MeanWindspeed3day)

mod1 <- lmer(ResidMass ~ TotalRainFall3day_2*ThermoReg+ + MaxTemp3day_2*ThermoReg + MeanWindspeed3day_2*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
sm <- summary(mod1)
anova(mod1)
Estimates <- data.frame(Age=c(rep("Homeotherm", 3), rep("Intermediate", 3), rep("Poikilotherm", 3)), 
           Weather= rep(c("Rain", "Temp.", "Wind"), 3), 
           Estimate=as.numeric(rep(NA_real_)),
           SE=rep(NA_real_))
dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c("Endotherm", "Poikilotherm", "Intermediate"))
Estimates[1, 3:4] <- sm$coefficients[2,1:2] #rain homeotherm 
Estimates[2, 3:4] <- sm$coefficients[5,1:2] #Temp homeotherm
Estimates[3, 3:4] <- sm$coefficients[6,1:2] #Wind Homeotherm


dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c( "Intermediate","Endotherm", "Poikilotherm"))
mod1 <- lmer(ResidMass ~ TotalRainFall3day_2*ThermoReg+ + MaxTemp3day_2*ThermoReg + MeanWindspeed3day_2*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
sm <- summary(mod1)

Estimates[4, 3:4] <- sm$coefficients[2,1:2] #rain intermediate 
Estimates[5, 3:4] <- sm$coefficients[5,1:2] #Temp intermediate
Estimates[6, 3:4] <- sm$coefficients[6,1:2] #Wind intermediate


dat3$ThermoReg <- factor(dat3$ThermoReg, levels=c(  "Poikilotherm","Intermediate","Endotherm"))
mod1 <- lmer(ResidMass ~ TotalRainFall3day_2*ThermoReg+ + MaxTemp3day_2*ThermoReg + MeanWindspeed3day_2*ThermoReg + (1|NestID/NestlingID), data=dat3, REML=FALSE)
sm <- summary(mod1)

Estimates[7, 3:4] <- sm$coefficients[2,1:2] #rain poikilotherm 
Estimates[8, 3:4] <- sm$coefficients[5,1:2] #Temp poikilotherm
Estimates[9, 3:4] <- sm$coefficients[6,1:2] #Wind poikilotherm


ggplot(data=Estimates, aes(x=Estimate, y=Weather, color=Age))+
  geom_point(size=3)+
  geom_vline(xintercept=0)+
  geom_segment(aes(x=Estimate+SE, xend=Estimate-SE, y=Weather, yend=Weather))+
  xlim(-1.2,1.2)+
  labs(x="", y="")+
  theme_classic( base_size=20)+
  theme(legend.title.align =0.5, 
        legend.position = c(0.8,0.2), 
        legend.background = element_rect( size = 0.5, colour = 1), 
        legend.text = element_text(size=14), 
        legend.title =element_text(size=16) )+
  scale_color_manual(values=c("red3", "darkorchid3", "blue1"))
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Relative importance of weather variables on nestling mass.jpeg", units="in", width=6, height=4.5, device="jpeg")


ggplot(data=Estimates, aes(x=Estimate, y=Weather, color=Age))+
  geom_vline(xintercept=0)+
  
  geom_point(size=5, aes(shape=Age))+
  geom_segment(aes(x=Estimate+SE, xend=Estimate-SE, y=Weather, yend=Weather), show.legend = F)+
  xlim(-1.2,1.2)+
  labs(x="Effect Size", y="", color="", shape="")+
  theme_classic(base_family ="serif", base_size=16)+
  scale_color_grey()+
  theme(legend.position = c(0.8,0.2),
        legend.box.background   = element_rect(colour = "black"), 
        legend.background = element_blank(), 
        legend.title = element_blank())
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Mass Effect size plot_grey.jpeg", units="in", width=6, height=5, device="jpeg")
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Mass Effect size plot_grey.pdf", units="in", width=6, height=5, device="pdf")


PanelA_2 <- ggplot(dat3, aes(x=MeanWindspeed3day, y=ResidMass, group=ThermoReg))+
  geom_point(shape=1)+
  geom_smooth(method="lm", color="black")+
  facet_grid(ThermoReg~.)+
  labs(x="Mean windspeed (m/s)\n", y="Residual mass")+
  theme_classic(base_size = 16, base_family = "serif")


PanelB_2 <- ggplot(dat3, aes(x=MaxTemp3day, y=ResidMass, group=ThermoReg))+
  geom_point(shape=1)+
  geom_smooth(method="lm", color="black")+
  facet_grid(ThermoReg~.)+
  labs(x=expression("Max. temperature " ( degree*C)), y="Residual mass")+
  theme_classic(base_size = 16, base_family = "serif")

PanelC_2 <- ggplot(dat3, aes(x=TotalRainFall3day, y=ResidMass, group=ThermoReg))+
  geom_point(shape=1)+
  geom_smooth(method="lm", color="black")+
  facet_grid(ThermoReg~.)+
  labs(x="Total Rainfall (mm)\n", y="Residual mass")+
  theme_classic(base_size = 16, base_family = "serif")


cowplot::plot_grid(PanelA_2, PanelB_2, PanelC_2, nrow = 1, ncol=3, labels = c("a", "b", "c"), label_fontfamily = "serif")
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Weather Mass plot with points.jpeg", units="in", width=8, height=8, device="jpeg")














##########################
#Based on reviewer 2, we should not have any colinearity in our models, so it would be better to use the PCs



