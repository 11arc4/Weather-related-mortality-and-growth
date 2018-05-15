# #Analysis of long term weather conditions during nestling development 
 library(tidyverse)
# 
# data <- as.data.frame(matrix(nrow= 12000, ncol=4))
# colnames(data) <- c("NestID", "Year", "LayDate","HatchDate", "FledgeDate", "FemaleAge")
# 
# i=0
# for (nest in as.list(globalData$nests)){
#   i=i+1
#   data$NestID[i] <- paste(nest$siteID, nest$year, nest$renestStatus, sep="-")
#   data$Year[i]  <- nest$year
#   data$LayDate[i] <- nest$firstEggDate
#   data$HatchDate[i] <- nest$hatchDate
#   data$FledgeDate[i] <- nest$fledgeDate
#   
# 
#   #data$FemaleAge[i] <- nest$fAge
# 
# 
# 
# }
# 
# 
# data<- data[1:i, ]
# 
# write.csv(data, "file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Long term nestling dates.csv", row.names = F, na="")

dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Long term nestling dates.csv")  %>% filter(HatchDate>130)# filter out bad nests because I checked and they're definitely wrong

dat$FledgeDate[dat$FledgeDate==0] <- NA

weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
rm(weather_pre)
colnames(weather) <- c("Date", "Year",  
                       "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%m/%d/%Y"))
weather$MeanTemp <- as.numeric(weather$MeanTemp)
weather$MaxTemp <- as.numeric(weather$MaxTemp)
weather$TotRain <- as.numeric(weather$TotRain)



dat$TotalRain <- NA
for(i in 1:nrow(dat)){
  if(!is.na(dat$HatchDate[i])& !is.na(dat$FledgeDate[i])){
    dat$TotalRain[i] <- sum(weather$TotRain[weather$Year==dat$Year[i] & weather$JDate>=dat$HatchDate[i] & weather$JDate<dat$FledgeDate[i]], na.rm=T)
  }
}



hist(data$HatchDate, breaks=20)

data2 <- dat %>% group_by(Year) %>% summarise(FirstNestlings = min(HatchDate, na.rm=T),
                                               FirstNestlings25 = summary(HatchDate)[2],
                                               LastNestlings = max( FledgeDate, na.rm=T),
                                               LastNestlings75=summary(FledgeDate)[5],
                                              MeanTotalRain = mean(TotalRain, na.rm=T), 
                                               TotalRain=NA, 
                                               TotalRainQ = NA, 
                                               NAs=NA, 
                                               NAsQ = NA 
                                              )
data2$LastNestlings [data2$LastNestlings==-Inf] <- NA
data2$MeanTotalRain[data2$MeanTotalRain==0 | is.nan(data2$MeanTotalRain)] <- NA






for(i in 1:nrow(data2)){
  data2$TotalRain[i] <- sum(weather$TotRain[weather$Year==data2$Year[i] & weather$JDate>=data2$FirstNestlings[i] & weather$JDate<data2$LastNestlings[i]], na.rm=T)
  data2$NAs[i] <- anyNA(weather$TotRain[weather$Year==data2$Year[i] & weather$JDate>=data2$FirstNestlings[i] & weather$JDate<data2$LastNestlings[i]])
  
  data2$TotalRainQ[i] <- sum(weather$TotRain[weather$Year==data2$Year[i] & weather$JDate>=data2$FirstNestlings25[i] & weather$JDate<data2$LastNestlings75[i]], na.rm=T)
  data2$NAsQ[i] <- anyNA(weather$TotRain[weather$Year==data2$Year[i] & weather$JDate>=data2$FirstNestlings25[i] & weather$JDate<data2$LastNestlings75[i]])
  
  }
#Nas are mainly not a problem, unless TotRain=0 -- then there wasnt any data that year

data2$TotalRain[data2$TotalRain==0] <- NA
data2$TotalRainQ[data2$TotalRainQ==0] <- NA

data3 <- data2 %>% filter(!is.na(MeanTotalRain))


breakpoints <- strucchange::breakpoints(formula= data3$MeanTotalRain~data3$Year)
#There are no breakpoints. This is good. I prefer that it's one smooth line.
#That matches out other stuff better!
mod <- lm(MeanTotalRain~Year, data=data3, na.action="na.fail")
plot(mod)
shapiro.test(resid(mod)) #It's only not normal because of that one outlier year. 
plot(resid(mod)~data3$Year)

car::Anova(mod)
MuMIn::dredge(mod)
summary(mod)

ggplot(data2, aes(x=Year, y=MeanTotalRain))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(x="Year", y="Mean rainfall during \nnestlings development (mm)")+
  ggthemes::theme_few(base_family = "serif", base_size = 16)
ggsave(filename="~/Masters Thesis Project/Weather determined growth and mortality paper/Plots/Rainfall increases through time.jpeg", units="in", width=8, height=4, device="jpeg")


ggplot(data2 , aes(x=Year) )+
  geom_point(aes(y=FirstNestlings))+
  geom_vline(xintercept = 1991)+
  labs(x="Year", y="Date first nestlings hatched")
#Nestlings are starting hatch earlier

ggplot(data2 , aes(x=Year) )+
  geom_point(aes(y=LastNestlings))+
  geom_vline(xintercept = 1991)
#Last nestlings are still fledging at the same time!
#That means the nestling season is probably getting longer




