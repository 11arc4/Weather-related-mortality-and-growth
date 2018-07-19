library(tidyverse)

feeding <- read.csv("file:///C:/Users/11arc/Dropbox/TRES RFID 2017/feeding.csv", na.strings="NA")
#ffv and mfv are feeding visits per hour (female and male respectively). This is
#what we want to test for changes with daily weather conditions
weather <- read.csv("file:///C:/Users/11arc/Dropbox/Kennedy Everitt Honors Thesis/Daily weather data 2017.csv")
names(weather)[c(5,6,7,10,12,13,14)]<- c("MaxTemp", "MinTemp", "MeanTemp", "TotalRain", "meanwindspeed", "maxwindspeed", "minwindspeed")
weather.pca <- readRDS("~/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Weather-related-mortality-and-growth/WeatherPCA.rds")

weather$PC1 <- predict(weather.pca, weather)[,1]
weather$PC2 <- predict(weather.pca, weather)[,2]




feeding$starttime
feeding$date <- as.Date(substring(feeding$starttime, 1,10), format="%Y-%m-%d")

nestdata <- read.csv("file:///C:/Users/11arc/Dropbox/Kennedy Everitt Honors Thesis/TRESDATA.csv") 
nestdata2 <- nestdata %>% filter(Age>8 & Age <=12)%>% 
  group_by(NestID, NestlingID)  %>% 
  summarise(FledgeSize=first(FledgeSize),
            HatchSize=first(HatchSize),
            Mass1 =first(Mass), 
            Mass2 =last(Mass), 
            Age1= first(Age), 
            Age2=last(Age)) %>% 
  filter(Age1!=Age2)
nestdata2$LateStageGrowth <- (nestdata2$Mass2-nestdata2$Mass1)/(nestdata2$Age2-nestdata2$Age1)

nestdata3 <- nestdata2 %>% group_by(NestID) %>% summarise(FledgeSize=first(FledgeSize), 
                                                          LateStageGrowth=mean(LateStageGrowth, na.rm=T), 
                                                          HatchSize=first(HatchSize))


feeding$time[nchar(as.character(feeding$starttime))==16] <- 
  as.numeric(substring(feeding$starttime[nchar(as.character(feeding$starttime))==16], 12, 13)) + 
  as.numeric(substring(feeding$starttime[nchar(as.character(feeding$starttime))==16], 15, 16))/60

feeding$time[nchar(as.character(feeding$starttime))==15] <- 
  as.numeric(substring(feeding$starttime[nchar(as.character(feeding$starttime))==15], 12, 12)) + 
  as.numeric(substring(feeding$starttime[nchar(as.character(feeding$starttime))==15], 14, 15))/60



weather$Date.Time <- as.Date (weather$Date.Time, format="%Y-%m-%d")



feeding <- cbind(feeding, matrix(NA, ncol=18, nrow=nrow(feeding)))
names(feeding)[13:30] <- c("MaxTemp", "MinTemp", "MeanTemp", "TotalRain", "meanwindspeed", "maxwindspeed", "minwindspeed", "PC1", "PC2", "MaxTemp_3day", "MinTemp_3day", "MeanTemp_3day", "TotalRain_3day", "meanwindspeed_3day", "maxwindspeed_3day", "minwindspeed_3day", "PC1_3day", "PC2_3day")
for(date in unique(feeding$date)){
  rf <- which(feeding$date==date)
  rw <- which(weather$Date.Time==date)
  rw3 <- which(weather$Date.Time<date & weather$Date.Time>=date-3)
  feeding[rf,13:21] <- weather[rw,c(5,6,7,10, 12,13,14, 16, 17)]
  feeding[rf, c(22:24, 26:30)] <- weather[rw3,c(5,6,7, 12,13,14, 16, 17)] %>% summarise_all(mean)
  feeding$TotalRain_3day[rf] <- sum(weather[rw3,10])
}

feeding$TotalRain_3day2 <- "None"
feeding$TotalRain_3day2[feeding$TotalRain_3day>4]<- "Rain"
feeding$TotalRain_3day2 <- factor(feeding$TotalRain_3day2)

feeding$LateStageGrowth <- NA
feeding$FledgeSize <- NA
feeding$HatchSize <- NA

for(i in 1:nrow(feeding)){
  if(any(as.character(feeding$boxID)[i]==nestdata3$NestID)){
    feeding$LateStageGrowth[i] <- nestdata3$LateStageGrowth[which(as.character(feeding$boxID)[i]==nestdata3$NestID)]
    feeding$FledgeSize[i] <- nestdata3$FledgeSize[which(as.character(feeding$boxID)[i]==nestdata3$NestID)]
    feeding$HatchSize[i] <- nestdata3$HatchSize[which(as.character(feeding$boxID)[i]==nestdata3$NestID)]
    
  }
    
  }


ggplot(feeding, aes(x=NestlingsAlive, y=ffv))+
  geom_point()
#number of nestlings seems to really make a difference. I'll include that as a covariate. 


#Let's look first at how weather conditions on day 10 of nestling development affect female feeding rates. 
feeding_female <- feeding %>% filter(!is.na(ffv))
#mean temp?
mod <- lm(ffv~MeanTemp*NestlingsAlive, data=feeding_female)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$MeanTemp)
plot(resid(mod)~feeding_female$NestlingsAlive)

anova(mod)
#Nope mean temp doesn't affect female feeding rate (nestlings alive obviously does.) 

#Max temp?
mod <- lm(ffv~MaxTemp*NestlingsAlive, data=feeding_female)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$MaxTemp)
plot(resid(mod)~feeding_female$NestlingsAlive)

anova(mod)
#nope max temp doesn't matter either (nestlings still does of course)

#meanwindspeed?
mod <- lm(ffv~meanwindspeed*NestlingsAlive, data=feeding_female)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$meanwindspeed)
plot(resid(mod)~feeding_female$NestlingsAlive)

anova(mod)
#nope windspeed is unimportant

#total rain? There isn't really any point doing this one-- we didn't measure on
#days when it was raining, so it wouldn't have been raining at the time even if
#it rained later.

#a combination? 
mod <- lm(ffv~PC1*NestlingsAlive + PC2*NestlingsAlive, data=feeding_female)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$PC1)
plot(resid(mod)~feeding_female$PC2)
plot(resid(mod)~feeding_female$NestlingsAlive)

anova(mod)
#Nothing.




#WEATHER CONDITIONS (OR AT LEAST THE ONES WE MEASURED) DO NOT AFFECT FEMALE
#EFFORT. IT SEEMS TO BE QUITE VARI ABLE, BUT NOT DRIVEN BY WEATHER
mam <- lm(ffv~NestlingsAlive, data=feeding_female)
summary(mam)
anova(mam)


#How are males affected by local weather conditions? 
feeding_male <- feeding %>% filter(!is.na(mfv))

#mean temp?
mod <- lm(mfv~MeanTemp*NestlingsAlive, data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male$MeanTemp)
anova(mod) 
#THis is ALMOST significant. It's funny though. Males don't seem to be
#influenced by nestlings and their demand, but rather increase feeding when it's
#easier (warmer)
ggplot(feeding_male, aes(x=MeanTemp, y=mfv))+
  geom_point()+
  geom_smooth(method="lm")
mod <- lm(mfv~MeanTemp, data=feeding_male, na.action="na.fail")
anova(mod)
summary(mod)

#Max Temp?
mod <- glm(mfv~MaxTemp*NestlingsAlive, family="Gamma", data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
plot(resid(mod)~feeding_male$MaxTemp)
anova(mod, test="F") 
#nope. Max temp makes little difference


#Mean windspeed?
mod <- glm(mfv~meanwindspeed*NestlingsAlive,family="Gamma", data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
plot(resid(mod)~feeding_male$meanwindspeed)
summary(mod)
anova(mod, test="F") 
#Nope not at all. 
ggplot(feeding_male, aes(x=NestlingsAlive, y=mfv))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="Gamma"), se=F)

#A combination of weather variables?
mod <- lm(mfv~PC1+NestlingsAlive+PC2, data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male$PC1)
plot(resid(mod)~feeding_male$PC2)
plot(resid(mod)~feeding_male$NestlingsAlive)

anova(mod)
#Nope


mod <- lm(mfv~NestlingsAlive, data=feeding_male, na.action="na.fail")
anova(mod)

#Do females get to slack a bit if the males work harder? 
ggplot(feeding, aes(x=ffv, y=mfv))+
  geom_point()+
  geom_smooth(method="lm")

feeding2 <- feeding %>% filter(!is.na(mfv) & !is.na(ffv))
mod <- lm(mfv ~ffv, data=feeding2)
plot(mod)
hist(resid(mod))
plot(resid(mod)~feeding2$mfv)
anova(mod)
summary(mod)
#Nope females who have harder working males are also working harder!

cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")

feeding2[which(cooksd>4*mean(cooksd)),]

####In sum. Females don't seem to really modulate their work based on weather
####conditions but rather how many nestlings they have. They're probably just
####working at their max. harder working males have harder working females--
####perhaps this is a quality measure? Males do modulate their effort, and work
####harder if the mean temperatures are higher (more food around) but it's a
####weak signal. They don't really increase for increasing nestlings.




###############################################
#Does the weather in the preceding three days affect provisioning rates? 

##########Female provisioning rates

#Does rain in the previous 3 days predict? 
mod <- lm(ffv ~NestlingsAlive*TotalRain_3day2, data=feeding_female, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$NestlingsAlive)
plot(resid(mod)~as.factor(feeding_female$TotalRain_3day2))
anova(mod)
MuMIn::dredge(mod)

mam_rain3day <- lm(ffv ~NestlingsAlive+TotalRain_3day2, data=feeding_female)
anova(mam_rain3day)
summary(mam_rain3day)

ggplot(feeding_female, aes(x=TotalRain_3day2, y=residffv))+
  geom_boxplot(aes(fill=TotalRain_3day2), show.legend = F)+
  geom_jitter(width=0.2)+
  labs(y="Residual female provisioning rate \n (vists/hr per nestling)", x="Rainfall in previous 3 days", fill="")+
  theme_classic(base_size = 16, base_family = "serif")

#Does mean temperature in the previous 3 days predict?
mod <- lm(ffv ~NestlingsAlive*MeanTemp_3day, data=feeding_female, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$NestlingsAlive)
plot(resid(mod)~feeding_female$MeanTemp_3day)
anova(mod)
MuMIn::dredge(mod)

mam <- lm(ffv ~NestlingsAlive+MeanTemp_3day, data=feeding_female)
anova(mam)
#Nope. Not at all

#Does max temperature in the previous 3 days predict?
mod <- lm(ffv ~NestlingsAlive*MaxTemp_3day, data=feeding_female, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$NestlingsAlive)
plot(resid(mod)~feeding_female$MaxTemp_3day)
anova(mod)
MuMIn::dredge(mod)

mam <- lm(ffv ~NestlingsAlive+MaxTemp_3day, data=feeding_female)
anova(mam)
#Nope. Not at all


#Does mean windspeed in the previous 3 days predict?
mod <- lm(ffv ~NestlingsAlive*meanwindspeed_3day, data=feeding_female, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$NestlingsAlive)
plot(resid(mod)~feeding_female$MeanTemp_3day)
anova(mod)
MuMIn::dredge(mod)

mam_wind3day <- lm(ffv ~NestlingsAlive+meanwindspeed_3day, data=feeding_female)
anova(mam)
#very much so!
summary(mam)

ggplot(feeding_female, aes(x=meanwindspeed_3day, y=residffv))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(y="Residual female provisioning rate \n (vists/hr per nestling)", x="Mean windspeed (m/s) in previous 3 days", fill="")+
  theme_classic(base_size = 16, base_family = "serif")


#Does overall weather condition in the previous 3 days matter? 
mod <- lm(ffv ~NestlingsAlive*PC1 + NestlingsAlive*PC2, data=feeding_female, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female$NestlingsAlive)
plot(resid(mod)~feeding_female$MeanTemp_3day)
anova(mod)
MuMIn::dredge(mod)
#No

null <- lm(ffv ~NestlingsAlive, data=feeding_female, na.action="na.fail")


MuMIn::AICc(mam_rain3day, mam_wind3day, null)
#They're equally good predictors and both better than the null! 




#What if we add wind and rain in together? 

both <- lm(ffv ~NestlingsAlive+ TotalRain_3day2+ meanwindspeed_3day, data=feeding_female, na.action="na.fail")
plot(both)
hist(resid(both))
shapiro.test(resid(both))
plot(resid(both)~feeding_female$NestlingsAlive)
plot(resid(both)~feeding_female$MeanTemp_3day)
anova(both)
MuMIn::dredge(both)
#When you pop both into the model, the better term is total rain (over windspeed)



############is male feeding rate predicted by weather in the preceding 3 days?
#Can't do rain-- we only have 3 points without rain. 
#mean temp?
mod <- lm(mfv ~NestlingsAlive*MeanTemp_3day, data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male$NestlingsAlive)
plot(resid(mod)~feeding_male$MeanTemp_3day)
anova(mod)
MuMIn::dredge(mod)
#Nope

#max temp?
mod <- lm(mfv ~NestlingsAlive*MaxTemp_3day, data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male$NestlingsAlive)
plot(resid(mod)~feeding_male$MaxTemp_3day)
anova(mod)
MuMIn::dredge(mod)


#mean windspeed?
mod <- lm(mfv ~NestlingsAlive*meanwindspeed_3day, data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male$NestlingsAlive)
plot(resid(mod)~feeding_male$meanwindspeed_3day)
anova(mod)
MuMIn::dredge(mod)


#mean PC1 and PC2?
mod <- lm(mfv ~NestlingsAlive*PC1_3day + NestlingsAlive*PC2_3day, data=feeding_male, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male$NestlingsAlive)
plot(resid(mod)~feeding_male$PC1_3day)
plot(resid(mod)~feeding_male$PC2_3day)
anova(mod)
MuMIn::dredge(mod)
#Nope! 




####################
#Figures



PanelA <- ggplot(feeding_female, aes(x=TotalRain_3day2, y=ffv/NestlingsAlive))+
  geom_boxplot(aes(), show.legend = F)+
  geom_jitter(width=0.2)+
  labs(y="Female provisioning rate \n (vists/hr per nestling)", x="Rainfall in previous 3 days", fill="")+
  theme_classic(base_size = 16, base_family = "serif")


PanelB <- ggplot(feeding_female, aes(x=NestlingsAlive, y=ffv))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x="# of nestlings", y="Female provisioning \n (visits/hr)")+
  theme_classic(base_size = 16, base_family = "serif")


PanelC <- ggplot(feeding_male, aes(x=MeanTemp, y=mfv))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x=expression('Mean temperature ('*degree*C*')'), y="Male provisioning \n (visits/hr)")+
  theme_classic(base_size = 16, base_family = "serif")


PanelD <- ggplot(feeding, aes(x=ffv, y=mfv))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x="Female provisioning (visits/hr)", y="Male provisioning \n (visits/hr)")+
  theme_classic(base_size = 16, base_family = "serif")


cowplot::plot_grid(PanelA, PanelB, PanelC, PanelD, nrow=2, ncol=2, labels=c("a","b","c", "d"), label_fontfamily = "serif")
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Provisioning Rates.jpeg", units="in", width=9, height=9, device="jpeg")



