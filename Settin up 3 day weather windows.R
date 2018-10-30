#Set up weather data for a 3 day window prior to when we measured the nestlings 
#We have picked a 3 day window based on Winkler et al. 
#Load in the data
weather <- read.csv("C:/Users/11arc/Dropbox/Kennedy Everitt Honors Thesis/Daily weather data 2017.csv")

#Want only weather for days between 145 and 198
weather <- weather[weather$JulianDate>144 & weather$JulianDate<195,]
names(weather)[5:11] <-  c("MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotalRain", "TotalPrecip")

#weatherVar2 <- weather[,c(5:7, 10, 12:14)]  

#It's better to drop min and max windspeed so that the loadings for total rain
#is better (before it's not on the top 2 PC because there were more wind and temp variables than rain)
weatherVar2 <- weather[,c(5:7, 10, 12)]  
weatherVar2 <- weatherVar2 %>% mutate(Rain= ifelse(TotalRain>0, 1, 0))

ggplot(weatherVar2, aes(x=as.factor(Rain), y=meanwindspeed))+
  geom_boxplot()+
  geom_point()

weather.pca <- prcomp(weatherVar2, 
                      center=T, 
                      scale=T)

plot(weather.pca, type="lines")
summary(weather.pca)


cor(weatherVar2)



#By using weather PCs 1 and 2 we can capture 72% of the weather variation. We will use those 2. 
saveRDS(weather.pca, file="~/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Weather-related-mortality-and-growth/WeatherPCA.rds")
weather$PC1 <- predict(weather.pca, weather[,c(5:7, 10, 12:14)]  )[,1]
weather$PC2 <- predict(weather.pca, weather[,c(5:7, 10, 12:14)]  )[,2]









#Add in 3 day window averages for temperature and windspeed, and total rainfall
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Nestling and Weather Data.csv", as.is=T)[,-c(20:29)]
dat <- cbind(dat, matrix(nrow=nrow(dat), ncol=6, NA))

names(dat)[20:25] <- c("MaxTemp3day", "MeanTemp3day", "MeanWindspeed3day", "TotalRainFall3day", "PC13day", "PC23day")

for(i in 1:nrow(dat)){
  r <- which(dat$Date[i]> weather$JulianDate & dat$Date[i]-3<=weather$JulianDate)
  dat$MaxTemp3day[i] <- mean(weather$MaxTemp[r])
  dat$MeanTemp3day[i] <- mean(weather$MeanTemp[r])
  dat$MeanWindspeed3day[i] <- mean(weather$meanwindspeed[r])
  dat$TotalRainFall3day[i] <- sum(weather$TotalRain[r])
  dat$PC13day[i] <- mean(weather$PC1[r])
  dat$PC23day[i] <- mean(weather$PC2[r])
  
}



survdat <- read.csv("C:/Users/11arc/Dropbox/Kennedy Everitt Honors Thesis/Statistical Models/Survival Models/Nestling Survival Data.csv", as.is=T)[,-c(11:20)]  #already excluding AG
survdat <- cbind(survdat, matrix(nrow=nrow(survdat), ncol=6, NA))
names(survdat)[12:17] <- c("MaxTemp3day", "MeanTemp3day", "MeanWindspeed3day", "TotalRainFall3day", "PC13day", "PC23day")


for(i in 1:nrow(survdat)){
  r <- which(survdat$Time2[i]> weather$JulianDate & survdat$Time2[i]-3<=weather$JulianDate)
  survdat$MaxTemp3day[i] <- mean(weather$MaxTemp[r])
  survdat$MeanTemp3day[i] <- mean(weather$MeanTemp[r])
  survdat$MeanWindspeed3day[i] <- mean(weather$meanwindspeed[r])
  survdat$TotalRainFall3day[i] <- sum(weather$TotalRain[r])
  survdat$PC13day[i] <- mean(weather$PC1[r])
  survdat$PC23day[i] <- mean(weather$PC2[r])
  
}


write.csv(dat,"file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Nestling mass and Weather Data 3day.csv" , row.names = F, na="")
write.csv(survdat,"file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Nestling survival and Weather Data 3day.csv" , row.names = F, na="")

