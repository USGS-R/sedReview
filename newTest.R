staid <- c("09163500")
library(lubridate)
qwdata <- readNWISodbc(DSN = "NWISCO", STAIDS = staid)
# library(WQReview)
# qwdata <- WQReview::readNWISodbc(DSN = "NWISCO", STAIDS = staid)
x <- qwdata$PlotTable
#test again