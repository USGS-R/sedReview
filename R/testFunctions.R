# pull station data to test
staid <- c("09163500")
library(lubridate)
library(dplyr)
library(dataRetrieval)
source("R/converTime.R")
source("R/nwisODBC.R")
qwdata <- nwisODBC(DSN = "nwisco", env.db = "01", qa.db = "02", STAIDS = staid)
x <- qwdata$PlotTable
# for testing function calls
source("R/sedOutliers.R")
sedOutliers(x)
source("R/sandSiltBreak.R")
sandSiltBreak(x)
source("R/checkSamPurp.R")
checkSamPurp(x)
source("R/sampleVertCheck.R")
# EDI samples
EDIlist <- sampleEI(x, 20, 4, 9)
missingEDI <- EDIlist[[1]]
lowEDIvert <- EDIlist[[2]]
hiEDIvert <- EDIlist[[3]]
# EWI samples
EWIlist <- sampleEI(x, 10, 10, 20)
missingEWI <- EWIlist[[1]]
lowEWIvert <- EWIlist[[2]]
hiEWIvert <- EWIlist[[3]]
