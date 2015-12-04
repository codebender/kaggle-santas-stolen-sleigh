library(readr)
library(fields)

# Load data
gifts <- read_csv("../input/gifts.csv")

TripId = 0
submission <- data.frame(GiftId= integer(0), TripId= integer(0))

while(nrow(submission) < 100000) {
  print(TripId)
  print(nrow(submission))
  # Sort by weight, take those straight to destination first
  sortedGifts <- gifts[with(gifts, order(-Weight)), ]

  mostHeavyGift <- sortedGifts[1,]
  submission <- rbind(submission,data.frame(GiftId=mostHeavyGift$GiftId, TripId=TripId))
  remainingGifts <- sortedGifts[-1,]

  sleighLoad = 10 + mostHeavyGift$Weight

  while(sleighLoad <= 1000) {
    if (nrow(remainingGifts) == 0) break
    #find gifts close to heavy gift
    distances <- data.frame(
      dist=rdist.earth(remainingGifts[, c('Longitude', 'Latitude')],
                  mostHeavyGift[, c('Longitude', 'Latitude')]))

    clostestGift <- remainingGifts[which.min(distances$dist),]

    if ((sleighLoad + clostestGift$Weight) > 1000) break

    submission <- rbind(submission,data.frame(GiftId=clostestGift$GiftId, TripId=TripId))
    remainingGifts <- remainingGifts[-which.min(distances$dist),]

    sleighLoad = sleighLoad + clostestGift$Weight
  }

  gifts <- remainingGifts
  TripId = TripId + 1
}

write_csv(submission, "../submissions/simple.csv")
