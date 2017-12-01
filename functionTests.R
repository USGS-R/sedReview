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

data("exampleData", package = "sedReview")

x <- get_localNWIS('nwisco', STAIDS = '09163500',begin.date = '1992-01-01',end.date = '1995-01-01')
x2<-x[1:100,]
x2$SITE_NO <- '09163999'
x2$STATION_NM <- "Testing McTestersite"
x2$UID <- gsub("09163500", "0916399",x2$UID)
x <- rbind(x,x2)
exampleData2 <- rbind(x,exampleData)
save(exampleData2, file = "data/exampleData2.rda")
