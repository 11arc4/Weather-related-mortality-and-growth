library(tidyverse)

feeding <- read.csv("file:///C:/Users/11arc/Dropbox/TRES RFID 2017/feeding.csv", na.strings="NA")
#ffv and mfv are feeding visits per hour (female and male respectively). This is
#what we want to test for changes with daily weather conditions
weather <- read.csv("file:///C:/Users/11arc/Dropbox/Kennedy Everitt Honors Thesis/Daily weather data 2017.csv")



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



feeding <- cbind(feeding, matrix(NA, ncol=9, nrow=nrow(feeding)))
names(feeding)[13:21] <- c("MaxTemp", "MinTemp", "MeanTemp", "TotalRain", "meanwindspeed", "maxwindspeed", "minwindspeed", "PC1", "PC2")
for(date in unique(feeding$date)){
  rf <- which(feeding$date==date)
  rw <- which(weather$Date.Time==date)
  feeding[rf,13:19] <- weather[rw,c(5,6,7,10, 12,13,14)]
}

weather.pca <- readRDS("~/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Weather-related-mortality-and-growth/WeatherPCA.rds")

feeding[,20:21] <- predict(weather.pca, feeding)[,1:2]

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






#I'll just make a couple of quick figures. 




PanelA <- ggplot(feeding_female, aes(x=NestlingsAlive, y=ffv))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x="# of nestlings", y="Female provisioning \n (visits/hr)")+
  theme_classic(base_size = 16, base_family = "serif")


PanelB <- ggplot(feeding_male, aes(x=MeanTemp, y=mfv))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x=expression('Mean temperature ('*degree*C*')'), y="Male provisioning \n (visits/hr)")+
  theme_classic(base_size = 16, base_family = "serif")


PanelC <- ggplot(feeding2, aes(x=mfv, y=ffv))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x="Male provisioning \n (visits/hr)", y="Female provisioning \n (visits/hr)")+
  theme_classic(base_size = 16, base_family = "serif")
PanelC

cowplot::plot_grid(PanelA, PanelB, PanelC, nrow=3, ncol=1, labels=c("a","b","c"), label_fontfamily = "serif")
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Provisioning Rates.jpeg", units="in", width=3.5, height=9, device="jpeg")



############################
#Redo the entire sweet of analyses with males and females together. 



feeding_long <- reshape2::melt(feeding, id.vars = c(1:5,8:24), variable="sex", value.name="provisioningrate") %>% filter(!is.na(provisioningrate))
feeding_long$sex <- as.character(feeding_long$sex)
feeding_long$sex[feeding_long$sex=="ffv"]<- "F"
feeding_long$sex[feeding_long$sex=="mfv"]<- "M"
feeding_long$sex <- as.factor(feeding_long$sex)

###How does weather 
mod <- lm(provisioningrate ~ sex*MeanTemp*NestlingsAlive, data=feeding_long, na.action="na.fail")

plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_long$sex)
plot(resid(mod)~feeding_long$MeanTemp)
plot(resid(mod)~feeding_long$NestlingsAlive)

anova(mod)
MuMIn::dredge(mod)
#only thing that matters is nestlings alive if we do it like this. Not even sex really matters. 


mod <- lm(provisioningrate ~ sex*MaxTemp*NestlingsAlive, data=feeding_long, na.action="na.fail")

plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_long$sex)
plot(resid(mod)~feeding_long$MaxTemp)
plot(resid(mod)~feeding_long$NestlingsAlive)

anova(mod)
MuMIn::dredge(mod)
#again only nestlings alive matters

mod <- lm(provisioningrate ~ sex*meanwindspeed*NestlingsAlive, data=feeding_long, na.action="na.fail")

plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_long$sex)
plot(resid(mod)~feeding_long$meanwindspeed)
plot(resid(mod)~feeding_long$NestlingsAlive)

anova(mod)
MuMIn::dredge(mod)
#again nothing


mod <- lm(provisioningrate ~ sex*PC1*NestlingsAlive+sex*PC2*NestlingsAlive , data=feeding_long, na.action="na.fail")

plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_long$sex)
plot(resid(mod)~feeding_long$PC1)
plot(resid(mod)~feeding_long$PC2)
plot(resid(mod)~feeding_long$NestlingsAlive)

anova(mod)
MuMIn::dredge(mod)
#NOthing. 















library#####Is visitation rate a good indicator of somethign that benefits the nestlings? 

#Does mother or father visitation rate predict nestling growth rate? 

ggplot(feeding_female, aes(x=ffv, y=LateStageGrowth))+
  geom_point()

ggplot(feeding_male, aes(x=mfv, y=LateStageGrowth))+
  geom_point()+
  geom_smooth(method="lm")


ggplot(feeding, aes(x=ffv, y=FledgeSize/NestlingsAlive))+
  geom_point()+
  geom_smooth()
ggplot(feeding_female, aes(x=mfv, y=FledgeSize/NestlingsAlive))+
  geom_point()+
  geom_smooth(method="lm")




#None of these look promising. This is probably because those nestings have already been affected 

#Provisioning rate doesn't predict nestling growth from day 8-12
rmod <- lm(ffv~NestlingsAlive, data=feeding_female)
feeding_female$residffv <- resid(rmod)

feeding_female2 <- feeding_female %>% filter(!is.na(LateStageGrowth))


mod <- lm(LateStageGrowth ~residffv, data=feeding_female2, na.action = "na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_female2$residffv)
anova(mod)
MuMIn::dredge(mod)
summary(mod)



feeding_male2 <- feeding_male %>% filter(!is.na(LateStageGrowth))
mod <- lm(LateStageGrowth ~mfv, data=feeding_male2, na.action = "na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~feeding_male2$ffv)
plot(resid(mod)~feeding_male2$NestlingsAlive)
anova(mod)
#nothing



