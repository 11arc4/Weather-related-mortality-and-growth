#Packages Needed
library(lubridate)
library(lme4)
library(nlme)
library(car)
library(MuMIn)
library(glmmADMB)
library(dplyr)
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

mod <- lm(mass ~ sex*poly(diff, 1)*year, data=adult3, na.action="na.fail")
mod_2 <- lm(mass ~ sex*poly(diff, 2)*year, data=adult3, na.action="na.fail")
mod_3 <- lm(mass ~ sex*poly(diff, 3)*year, data=adult3, na.action="na.fail")

AICc(mod, mod_2, mod_3)
#Chose to use the 3rd order polynomial. 


mod_F <- lm(mass ~ poly(diff,3)*year, data=adult3_F, na.action="na.fail")



plot(mod_F)
hist(resid(mod_F))
plot(resid(mod_F)~adult3_F$diff)
plot(resid(mod_F)~adult3_F$year)
plot(resid(mod_F)~adult3_F$Period)

dredge(mod_F)
anova(mod_F, test="F")

mam_F <- lm(mass ~ poly(diff, 3)*year, data=adult3_F, na.action="na.fail")
summary(mam_F)


newdata_F <- data.frame(year=c(rep(1990, 30), rep(2017, 30)), 
                        diff=seq(0, 29, 1), 
                        Period=factor("Incubation", levels=c("Incubation", "Nestling")), 
                        predicted=NA, 
                        lcl=NA, 
                        ucl=NA)
newdata_F$Period[which(newdata_F$diff>=18)]<- "Nestling"

newdata_F[,4:6]<- predict(mam_F, newdata = newdata_F, interval = "confidence")
ggplot()+
  geom_line(data=newdata_F, aes(x=diff, y=predicted, color=factor(year)))+
  geom_ribbon(data=newdata_F, aes(x=diff, ymin=lcl, ymax=ucl, fill=factor(year)), alpha=0.4)+
  geom_point(data=adult3_F %>% filter(year==1990 |year==2017), aes(x=diff, y=mass, color=factor(year)))


mod_M <- lm(mass ~ poly(diff, 3)*year, data=adult3_M, na.action="na.fail")

plot(mod_M)
hist(resid(mod_M))
plot(resid(mod_M)~adult3_M$diff)
plot(resid(mod_M)~adult3_M$year)

dredge(mod_M)
anova(mod_M, test="F")

mam_M <- lm(mass ~ poly(diff, 3)* year, data=adult3_M, na.action="na.fail")
summary(mam_M)



newdata_M <- data.frame(year=c(rep(1990, 12), rep(2017, 12)), 
                        diff=rep(seq(18, 29, 1), 2), 
                        Period=factor("Incubation", levels=c("Incubation", "Nestling")), 
                        predicted=NA, 
                        lcl=NA, 
                        ucl=NA)
newdata_M$Period[which(newdata_M$diff>=18)]<- "Nestling"

newdata_M[,4:6]<- predict(mam_M, newdata = newdata_M, interval = "confidence")
ggplot()+
  geom_line(data=newdata_M, aes(x=diff, y=predicted, color=factor(year)))+
  geom_ribbon(data=newdata_M, aes(x=diff, ymin=lcl, ymax=ucl, fill=factor(year)), alpha=0.4)










#Has wing chord also decreased? Because if it has they're just smaller, not in poorer condition
adult4 <- adult %>% filter((sex=="M" | sex=="F") & !is.na(diff) & !is.na(wingChord) & diff>-5 & diff<30)

mod2 <- lm(wingChord ~ sex*year, data=adult4, na.action="na.fail")
plot(mod2)
hist(resid(mod2))
plot(resid(mod2)~adult4$sex)
plot(resid(mod2)~adult4$year)

dredge(mod2)
anova(mod2, test="F")
mam_wing <- lm(wingChord ~ sex, data=adult4, na.action="na.fail")

#Wing chord has not deccreased-- these birds aren't actually just getting smaller. They're loosing body condition. 
