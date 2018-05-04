####Pull out the measurements of adults in 2017 that would fit criteria for 
library(lubridate)

adult <- data.frame(matrix(nrow=400, ncol=27, NA))
names(adult) <- c("ID", "Sex", "Age", "NestID", "Provisioning Rate",
                  "IncubationDay", "IncubationDateMeasured" , "IncubationJDateMeasured", "Mass1", "Tarsus1", "WingChord1", "FatScore1", 
                  "NestlingDay", "NestlingDate", "NestlingJDate", "Mass2", "Tarsus2", "WingChord2", "FatScore2",
                  "LayDate","IncubationDate", "ClutchSize", "HatchDate", "HatchSize", "FledgeDate", "FledgeSize", "CauseofFailure")

i <- 0
for(bird in as.list(globalData$birds)){
  for(year in bird$yearsSeen$as.list()){
    #For a bird that was there in 2017, let's add them to the list here. We will
    #try to fill out what we can and decide whether to use them in analysis
    #based on whether things get filled out properly or not
    if(year$year==2017 & year$nest$length>0){
      if (year$observations$length>0){
        i <- i+1
        adult$ID[i] <- bird$bandID
        adult$Sex[i] <- bird$sex
        adult$Age[i] <- year$age
        
        #Get their first nest info
        adult$NestID[i] <- year$nest$buffer[[1]]$m_key
        nest <- get(year$nest$buffer[[1]]$m_key, globalData$nests)
        adult$LayDate[i] <- nest$firstEggDate
        adult$ClutchSize[i] <- nest$clutchSize
        adult$IncubationDate[i] <- nest$lastEggDate
        adult$ClutchSize[i] <- nest$clutchSize
        adult$HatchDate[i] <- nest$hatchDate
        adult$HatchSize[i] <- nest$hatchSize
        adult$FledgeDate[i] <- nest$fledgeDate
        adult$FledgeSize[i] <- nest$fledgeSize
        adult$CauseofFailure[i] <- nest$reasonforFailure
        
        #Go through the measurements and put them in as either the incubation
        #measurement closest to day 10, or the nestling measurement closes to
        #day 11.
        if(!is.na(nest$hatchDate)){
        for(measurement in year$observations$as.list()){
          if(measurement$type=="bodymeasurement"){
            jdate <- yday(as.Date(measurement$date, format="%Y-%m-%d"))
            if(nest$hatchSize>0){
            if(jdate<nest$hatchDate){
              #this was during incubation
              iday <- jdate-nest$lastEggDate
              #is this measurement closer to ID10 than a previous one? 
              if(is.na(adult$IncubationDay[i]) | abs(adult$IncubationDay[i]-10) > abs(iday-10)){
                adult$IncubationDateMeasured[i] <- measurement$date
                adult$IncubationJDateMeasured[i] <- jdate
                adult$IncubationDay[i] <- iday
                adult$Mass1[i] <- measurement$mass
                adult$Tarsus1[i] <- measurement$tarsus
                adult$WingChord1[i] <- measurement$wingChord
                
              }
            } else {
              #this was during nestling development
              nday <- jdate-nest$hatchDate
              #is this nestling date closer to NS11?
              if(is.na(adult$NestlingDay[i]) | abs(adult$NestlingDay[i]-11)> abs(nday-11)){
                adult$NestlingDate[i] <- measurement$date
                adult$NestlingJDate[i] <- jdate
                adult$NestlingDay[i] <- iday
                adult$Mass2[i] <- measurement$mass
                adult$Tarsus2[i] <- measurement$tarsus
                adult$WingChord2[i] <- measurement$wingChord
              }
            }
            }
          }
          }
        }
      }
    }
  }
}



#Add in fat scores. Those aren't in the database. 

bands <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Adult morphometrics 2017.csv")

for(i in 1:nrow(adult)){
  if(!is.na(adult$IncubationDateMeasured[i])){
    adult$FatScore1[i] <- bands$fat.score[which(bands$DATE==adult$IncubationDateMeasured[i] & bands$band.number==adult$ID[i])]
  }
  if(!is.na(adult$NestlingDate[i])){
    adult$FatScore2[i] <- bands$fat.score[which(bands$DATE==adult$NestlingDate[i] & bands$band.number==adult$ID[i])]
  }
}



write.csv( adult, "file:///C:/Users/11arc/Documents/Masters Thesis Project/Weather determined growth and mortality paper/Weather Analysis/Adult body condition change data 2017.csv", na="", row.names = F)
