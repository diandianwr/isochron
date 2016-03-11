# setwd("/Users/Wei/Documents/2016 Winter/Github/isochron")
dist <- read.csv("trip_5_10car.csv")


dist$time_start <- strptime(dist$time_start.x,format="%m/%d/%Y %H:%M:%OS")
dist$group <- paste0(substr(dist$time_start,12,13))
dist$group <- as.numeric(dist$group)

for(i in 1:length(dist$group)){
  if((dist$group[i] >=7 &&  dist$group[i] <9)){
    dist$hour[i] <- "peakhour"
  } else if((dist$group[i] >=16 && dist$group[i] <19)){
    dist$hour[i] <- "peakhour"
  }  else{
    dist$hour[i] <- "other"
  }
}

lunch <- subset(dist, dist$hour == "lunchhour")
other <- subset(dist, dist$hour == "other")

hist(peak$dis10)
hist(other$dis10)

