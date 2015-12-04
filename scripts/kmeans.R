library(dplyr)
library(aspace)

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


##calculates total distance for each trip
##function input - vector of gift numbers
weighted_trip_length <- function(trip_gifts) {
  x=data.frame(Latitude=90,Longitude=0,Weight=0,r=1)
  x=rbind(x,data.frame(gifts[trip_gifts,2:4],r=2:(length(trip_gifts)+1)))
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

gifts <- read.csv("../input/gifts.csv")
system.time(model <- kmeans(gifts[,2:3],5000,iter.max =10000, algorithm="MacQueen"))
gifts$TripId <- model$cluster
submission <- gifts[order(gifts$Weight, decreasing=TRUE),c(1,5)]

##calculate all trip distances
dist=0.0
for (i in unique(submission$TripId)) {
  dist=dist+weighted_trip_length(submission$GiftId[submission$TripId==i])
}
print(dist)

trips <- merge(gifts, submission)
tripSums <- aggregate(Weight~TripId, data=trips, FUN=sum)
nrow(tripSums[tripSums$Weight > 990, ])

write.csv(submission,file="../submissions/kmeans_5000_10000.csv",row.names=FALSE)
