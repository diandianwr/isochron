library(rgdal)
library(dplyr)
library(sp)
library(geosphere)

# setwd("C:\\Users\\weiran\\Documents\\HHSurvey\\isochronr")

# read data
trip<-read.table("5_PSRC2015_GPS_Trip.csv",header=TRUE,sep=",",quote="\"",flush=TRUE,fill=TRUE,
                 stringsAsFactors=FALSE)
loc<-read.table("6_PSRC2015_GPS_Location.csv",header=TRUE,sep=",",quote="\"",flush=TRUE,fill=TRUE,
                   stringsAsFactors=FALSE)

#----------------------------------------------------
# add neighborhood column

dat<-readOGR(dsn="neighborhoods",layer="Neighborhoods")

sp<-SpatialPointsDataFrame(data.frame(trip$olng,trip$olat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),trip)

neighborhoods<-spTransform(dat,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
originsInNeighborhood<-over(sp,dat)
sp@data$neighborhood<-originsInNeighborhood$S_HOOD

trip$neighborhood<-sp$neighborhood
#---------------------------join-------------------------
  
#join tripid to the start location of loc by start_time of every trip 
#change to time type data
loc$time <- strptime(loc$collected_at,format="%d-%b-%y %H:%M:%OS")
trip$time.start <- strptime(trip$time_start,format="%m/%d/%Y %H:%M:%OS")

#create key for join
loc$key1 <- paste(loc$personid,as.character(loc$time),sep = "_")
trip$key2 <- paste(trip$personid,as.character(trip$time.start),sep = "_")

trip1<- trip[!duplicated(trip$key2),]
loc1 <- loc[!duplicated(loc$key1),]

trip1<- trip1%>%
  select(tripid, key2, mode)
loc1<- loc1%>%
  select(-time)

#join 
mloc1 <- loc1 %>%
  left_join(trip1,by=c("key1"="key2"))

loc_tripid<- mloc1%>%
  mutate(tripid = as.character(tripid))

#----------------------------------------------------

#find 5 minutes location

loc_tripid$time <- strptime(loc_tripid$collected_at,format="%d-%b-%y %H:%M:%OS")
upper <- as.difftime("00:05:40","%H:%M:%S",units="secs")
lower <- as.difftime("00:04:20","%H:%M:%S",units="secs")

loc_tripid$ID <- seq.int(nrow(loc_tripid))
tripkey  <- na.omit(loc_tripid$tripid)
ntrip <- length(tripkey)
trip5min <-list()

for (j in 1:ntrip){
  start <- loc_tripid[which(loc_tripid$tripid==tripkey[j]),]
  startID <-start$ID + 1
  endID <- start$ID + 100
  for(i in startID:endID){
    if (is.na(loc_tripid$tripid[i])){
      d<-difftime(loc_tripid$time[i],start$time,units = "secs")
      if(d<=upper && d>=lower){
      trip5min$lat5[j] <- loc_tripid$lat[i]
      trip5min$lon5[j] <- loc_tripid$lon[i]
      trip5min$tripid5[j] <- tripkey[j]
      trip5min$key1[j] <- loc_tripid$key1[i]
      break
    }
    }
    else{
      break
    }
  }
}



#find 10 minutes location

upper1 <- as.difftime("00:10:40","%H:%M:%S",units="secs")
lower2 <- as.difftime("00:09:00","%H:%M:%S",units="secs")

trip10min <-list()
for (j in 1:ntrip){
  start <- loc_tripid[which(loc_tripid$tripid==tripkey[j]),]
  startID <-start$ID + 1
  endID <- start$ID + 200
  for(i in startID:endID){
    if (is.na(loc_tripid$tripid[i])){
      d<-difftime(loc_tripid$time[i],start$time,units = "secs")
      if(d<=upper1 && d>=lower2){
        trip10min$lat10[j] <- loc_tripid$lat[i]
        trip10min$lon10[j] <- loc_tripid$lon[i]
        trip10min$tripid10[j] <- tripkey[j]
        trip10min$key1[j] <- loc_tripid$key1[i]
        break
      }
    }
    else{
      break
    }
  }
}

#---------------------------------------------------------------
# join 5minutes locations to trip data

trip5m <- trip5min%>%
  as.data.frame()%>%
  na.omit()%>%
  mutate(tripid5=as.character(tripid5))

trip1<- trip[!duplicated(trip$key2),]

trip_5 <- trip1 %>%
  select(-time.start)%>%
  mutate(tripid=as.character(tripid))%>%
  right_join(trip5m,by=c("tripid"="tripid5"))%>%
  select(olat,olng,lat5,lon5,neighborhood,mode,time_start)%>%
  filter(mode>0 & mode <40)%>% #exclude outlier  
  na.omit()   # exclude trips outside region, neiborhood is na
#2275 observations and 1034 observations are in car


# join 5minutes locations to trip data
trip10m <- trip10min%>%
  as.data.frame()%>%
  na.omit()%>% 
  mutate(tripid10=as.character(tripid10))

trip_10 <- trip1 %>%
  select(-time.start)%>%
  mutate(tripid=as.character(tripid))%>%
  right_join(trip10m,by=c("tripid"="tripid10"))%>%
  select(olat,olng,lat10,lon10,neighborhood,mode,time_start)%>%
  filter(mode>0 & mode <40)%>% #exclude outlier  
  na.omit()   # exclude trips outside region, neiborhood is na



#1753 observations and 862 observations are in car

#---------------------------------------------------------------

#caculate distance from origin to 5, 10 location
start <- cbind(trip_5$olng,trip_5$olat)
end <- cbind(trip_5$lon5,trip_5$lat5)
for (i in 1:length(trip_5$lat5))
  {
  trip_5$dis5[i] <- distm(start[i,], end[i,], fun = distHaversine)
}

start <- cbind(trip_10$olng,trip_10$olat)
end <- cbind(trip_10$lon10,trip_10$lat10)
for (i in 1:length(trip_10$lat10))
{
  trip_10$dis10[i] <- distm(start[i,], end[i,], fun = distHaversine)
}


trip_5<-trip_5%>%
  select(-mode)

trip_5_10<-inner_join(trip_5,trip_10,by = "olat")
trip_5_10car <- trip_5_10%>%
  filter(mode ==6|mode ==7 |mode ==16|mode ==17)# exclude non-motorized trip


#---------------------------

#adding peak hour variable
trip_5_10car$time_start <- strptime(trip_5_10car$time_start.x,format="%m/%d/%Y %H:%M:%OS")
trip_5_10car$group <- paste0(substr(trip_5_10car$time_start,12,13))
trip_5_10car$group <- as.numeric(trip_5_10car$group)

for(i in 1:length(trip_5_10car$group)){
  if((trip_5_10car$group[i] >=7 &&  trip_5_10car$group[i] <9)){
    trip_5_10car$hour[i] <- "peakhour"
  } else if((trip_5_10car$group[i] >=16 && trip_5_10car$group[i] <19)){
    trip_5_10car$hour[i] <- "peakhour"
  }  else{
    trip_5_10car$hour[i] <- "other"
  }
}

write.csv(trip_5_10car,"trip_5_10car.csv")

#chose mode and summarise

#summary distance by neighborhood
trip_5_10car$time_start <- strptime(trip_5_10car$time_start.x,format="%m/%d/%Y %H:%M:%OS")

trip_car_dis <- trip_5_10car%>%
  select(-time_start)%>%
  group_by(neighborhood.x)%>%
  summarise(count= n(),dis5mean=mean(dis5), dis5sd=sd(dis5),
            dis5max=max(dis5),dis5min=min(dis5),
            dis10mean=mean(dis10), dis10sd=sd(dis10),
            dis10max=max(dis10),dis10min=min(dis10))

write.csv(trip_car_dis,"trip_car_dis.csv")
