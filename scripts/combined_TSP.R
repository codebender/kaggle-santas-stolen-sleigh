library(geosphere)
library(TSP)
library(dplyr)
library(aspace)
library (data.table)
library(readr)
library(fpc)

set.seed(1337)

AVG_EARTH_RADIUS = 6371

haversine <- function(lat1,lng1,lat2,lng2, miles=FALSE){
  lat1=as_radians(lat1)
  lat2=as_radians(lat2)
  lng1=as_radians(lng1)
  lng2=as_radians(lng2)

  lat = lat2 - lat1
  lng = lng2 - lng1
  d = sin(lat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(lng / 2) ** 2
  h = 2 * AVG_EARTH_RADIUS * asin(sqrt(d))
  if(miles==TRUE) return (h * 0.621371)
  if(miles==FALSE) return (h)
}

base_data <- read.csv("../input/gifts.csv")
##calculates total distance for each trip
##function input - vector of gift numbers
weighted_trip_length <- function(trip_gifts) {
  x=data.frame(Latitude=90,Longitude=0,Weight=0,r=1)
  x=rbind(x,data.frame(base_data[trip_gifts,2:4],r=2:(length(trip_gifts)+1)))
  x=rbind(x,data.frame(Latitude=90,Longitude=0,Weight=0,r=length(trip_gifts)+2))
  x=x %>%
    arrange(desc(r)) %>%
    mutate(TotalWeight=cumsum(Weight)+10) %>%
    arrange(r) %>%
    mutate(Latitude_next=lead(Latitude),Longitude_next=lead(Longitude),W=lead(TotalWeight)) %>%
    mutate(Distance=haversine(Latitude,Longitude,Latitude_next,Longitude_next)) %>%
    na.omit()
  x=sum(x$Distance*x$W)
  x
}

clstrs = 5280
gifts <- read.csv("../input/gifts.csv")

model <- kmeansCBI(gifts[, 2:3], k=clstrs, iter.max = 10000,
                   scaling=TRUE)

delivery <- data.frame(GiftId = gifts$GiftId, TripId = model$partition - 1)

weightedDelivery <- merge(delivery, gifts)

weightSubmission <- weightedDelivery[with(weightedDelivery, order(-Weight)), c('GiftId', 'TripId')]
orderedSubmission <- delivery

TSPsubmission <- data.frame(GiftId=integer(0), TripId=integer(0))
for (i in unique(delivery$TripId)) {
  if(nrow(gifts[delivery$TripId==i,]) == 1 || nrow(gifts[delivery$TripId==i,]) == 2) {
    TSPTrip <- data.frame(GiftId = gifts[delivery$TripId==i, c('GiftId')],
                          TripId = i)
    TSPsubmission <- rbind(TSPsubmission, TSPTrip)
    next
  }

  distMatrix <- dist(gifts[delivery$TripId==i, c('Longitude', 'Latitude')])
  atsp <- ATSP(distMatrix, labels=gifts[delivery$TripId==i, c('GiftId')])
  tour_atsp <- solve_TSP(atsp)
  TSPTrip <- data.frame(GiftId=as.integer(labels(tour_atsp)), TripId=i)
  TSPsubmission <- rbind(TSPsubmission, TSPTrip)
}

weightedDistances <- data.frame()
dist=0.0
for (i in unique(weightSubmission$TripId)) {
  weightedDist = weighted_trip_length(weightSubmission$GiftId[weightSubmission$TripId==i])
  weightedDistances <- rbind(weightedDistances, data.frame(TripID= i,WD=weightedDist))
  dist = dist + weightedDist
}
print(dist)

orderDistances <- data.frame()
orderedDist=0.0
for (i in unique(orderedSubmission$TripId)) {
  weightedDist = weighted_trip_length(orderedSubmission$GiftId[orderedSubmission$TripId==i])
  orderDistances <- rbind(orderDistances, data.frame(TripID= i,WD=weightedDist))
  orderedDist = orderedDist + weightedDist
}
print(orderedDist)

TSPDistances <- data.frame()
TSPDist=0.0
for (i in unique(TSPsubmission$TripId)) {
  weightedDist = weighted_trip_length(TSPsubmission$GiftId[TSPsubmission$TripId==i])
  TSPDistances <- rbind(TSPDistances, data.frame(TripID= i,WD=weightedDist))
  TSPDist = TSPDist + weightedDist
}
print(TSPDist)


allDistances <- merge(orderDistances, TSPDistances, by=c('TripID'), all = TRUE, 
                      suffixes = c(".ordered",".TSP"))
allDistances <- merge(allDistances, weightedDistances, by=c('TripID'), 
                      all = TRUE)
names(allDistances)[4] <- 'WD.weighted'

allDistances$Min <- with(allDistances, pmin(WD.ordered, WD.TSP, WD.weighted))


submission <- data.frame()
for (i in 1:nrow(allDistances)) {
  row <- allDistances[i, ]
  
  if(row$Min == row$WD.TSP) {
    submission <- rbind(submission, 
                        TSPsubmission[TSPsubmission$TripId == row$TripID, ])
  }
  else if(row$Min == row$WD.weighted) {
    submission <- rbind(submission, 
                        weightSubmission[weightSubmission$TripId == row$TripID, ])
  }
  else {
    submission <- rbind(submission, 
                        orderedSubmission[orderedSubmission$TripId == row$TripID, ])
  }
}

submissionDistances <- data.frame()
submissionDist=0.0
for (i in unique(TSPsubmission$TripId)) {
  weightedDist = weighted_trip_length(submission$GiftId[submission$TripId==i])
  submissionDistances <- rbind(submissionDistances, data.frame(TripID= i,WD=weightedDist))
  submissionDist = submissionDist + weightedDist
}
print(submissionDist)

write.csv(submission,file="../submissions/combined_models.csv",row.names=FALSE)
