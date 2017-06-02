library(sedReview)

#load the data
load("data/testData.rda")

#Make it wide
wideData <- makeWideTable(testData)

#checkNWIS20
system.time({
  checkNWIS20_OUT <- checkNWIS20(testData)
})

#checkOSQ2001_03
system.time({
  checkOSW2001_03_OUT <- checkOSW2001_03(testData)
})

#checkQ
system.time({
checkQ_OUT <- checkQ(testData)
})

