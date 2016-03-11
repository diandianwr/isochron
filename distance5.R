setwd("/Users/Wei/Documents/2016 Winter/Github/isochron")
dist5 <- read.csv("trip_5.csv")
test <- read.csv("trip_5_10_dis.csv")
dist5$time_start <- strptime(dist5$time_start,format="%m/%d/%Y %H:%M:%OS")

