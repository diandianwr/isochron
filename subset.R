library(rgdal)
library(dplyr)
setwd("/Users/Wei/Desktop/final project")

# read data
trip<-read.table("trip.csv",header=TRUE,sep=",",quote="\"",flush=TRUE,fill=TRUE,
                 stringsAsFactors=FALSE)
loc_id<-read.table("loc_id.csv",header=TRUE,sep=",",quote="\"",flush=TRUE,fill=TRUE,
                stringsAsFactors=FALSE)

dat<-readOGR(dsn="Neighborhoods",layer="Neighborhoods")

sp<-SpatialPointsDataFrame(data.frame(trip$olng,trip$olat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),trip)

# add neighborhood column
neighborhoods<-spTransform(dat,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
originsInNeighborhood<-over(sp,dat)
sp@data$neighborhood<-originsInNeighborhood$S_HOOD

# starting locations
origins<-loc$locnum==1
origins.df<-loc[origins,]

# orgin key
start.time<-paste(origins.df$collected_at,origins.df$lat,origins.df$lon)
key.time<-strsplit(trip$time_start, " ")
key.time.splits<-unlist(lapply(key.time, "[",2))
start.time.key<-paste(trip$traveldate,key.time.splits,trip$olat,trip$olng)

# sanity check
table(start.time %in% start.time.key)

# create trip.id vector
origin.index<-which(loc$locnum==1)-1
origin.index<-origin.index[-1]
origin.index[length(origin.index)+1]<-nrow(loc)
origin.val<-loc$locnum[origin.index]
trip.id<-rep(start.time,origin.val)

# add key
loc$trip.key<-trip.id
trip$trip.key<-start.time.key
trip$neighborhood<-sp$neighborhood



#change 
loc$time <- strptime(loc$collected_at,format="%d-%b-%y %H:%M:%OS")
trip$start.time <- strptime(trip$time_start,format="%m/%d/%Y %H:%M:%OS")
trip$end.time <- strptime(trip$time_end,format="%m/%d/%Y %H:%M:%OS")

#merge
loc1$time <- as.character(loc1$time)
loc$key1 <- paste(loc$personid,loc$time.key,sep = "_")
trip$start.key <- as.character(trip1$start.time)
trip$end.key <- as.character(trip$end.key)
trip$key2 <- paste(trip$personid,trip$start.key,sep = "_")
trip1<- trip[!duplicated(trip$key2),]
trip1<- trip1%>%
  select(tripid, key2)
loc1 <- loc[!duplicated(loc$key1),]

# mloc <- merge(x=trip1,y = loc,by ="start.key", all.y = TRUE)
mloc1 <- loc1 %>%
  left_join(trip1,by=c("key1"="key2"))

# groups <- cut(as.POSIXct(loc$time), breaks="5 min")

# ddply(loc, "groups", tail, 1)[,1]


# ntrip <-length(unique(loc$trip.key))
# key <- unique(loc$trip.key)
# for (j in 1:length(key) ){
#   triploop <- subset(loc,loc$trip.key==key[j])
#   for(i in 1:length(triploop$locnum)){
#     d<-difftime(triploop$time[i],triploop$time[1],units = "secs")
#     if(d<=upper && d>=lower){
#       trip5min$lat[j] <- triploop$lat[i]
#       trip5min$lon[j] <- triploop$lon[i]
#       trip5min$time[j] <- triploop$time[i]
#       trip5min$trip.key[j] <- triploop$trip.key[i]
#       break
#     }
#   }
# }

loc<- loc_id%>%
  mutate(tripid = as.character(tripid))
upper <- as.difftime("00:05:40","%H:%M:%S",units="secs")
lower <- as.difftime("00:04:20","%H:%M:%S",units="secs")



loc$ID <- seq.int(nrow(loc))
#
tripkey  <- unique(loc$tripid)
tripkey <-na.omit(tripkey)
ntrip <- length(tripkey)
trip5min <-list()

for (j in 1:ntrip){
  start <- loc[which(loc$tripid==tripkey[j]),]
  startID <-start$ID + 1
  endID <- start$ID + 50
  for(i in startID:endID){
    if (is.na(loc$tripid[i])){
      d<-difftime(loc$time[i],start$time,units = "secs")
      if(d<=upper && d>=lower){
      trip5min$lat[j] <- loc$lat[i]
      trip5min$lon[j] <- loc$lon[i]
      trip5min$time[j] <- loc$time[i]
      trip5min$tripid[j] <- tripkey[j]
      trip5min$key1[j] <- loc$key1[i]
      print(tripkey[j])
      break
    }
    }
    else{
      break
    }
  }
}

# upper1 <- as.difftime("00:10:40","%H:%M:%S",units="secs")
# lower2 <- as.difftime("00:09:20","%H:%M:%S",units="secs")

# join to trip data

trip5m <- na.omit(as.data.frame(trip5min))

