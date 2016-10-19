# pull station data to test
staid <- c("09163500")
library(lubridate)
library(dplyr)
source("R/converTime.R")
source("R/readNWISodbc.R")
qwdata <- readNWISodbc(DSN = "NWISCO", STAIDS = staid)
x <- qwdata$PlotTable

# for testing function calls
source("R/sedOutliers.R")
sedOutliers(x)
source("R/sandSiltBreak.R")
sandSiltBreak(x)
source("R/checkSamPurp.R")
checkSamPurp(x)
