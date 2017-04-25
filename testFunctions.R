###Joe commented this out because all code in this directory needs to run as-is, so basically just functions
###If this needs to be retained it needs to be moved to the top level directory


# # pull station data to test
# staid <- c("09163500")
# library(lubridate)
# library(dplyr)
# library(dataRetrieval)
# source("R/converTime.R")
# source("R/nwisODBC.R")
# qwdata <- nwisODBC(DSN = "nwisco", env.db = "01", qa.db = "02", STAIDS = staid)
# x <- qwdata$PlotTable
# 
# 
# 
# # for testing function calls
# source("R/sampleVertCheck.R")
# sampleVertFlags <- sampleVertCheck(x)
# source("R/nwis20checks.R")
# nwis20Flags <- nwis20check(x)
# source("R/sedOutliers.R")
# sedOutlierFlags <- sedOutliers(x, 0.1, 0.9)
# source("R/checkSamPurp.R")
# sampPurpFlags <- checkSamPurp(x)
# 
# 
# # these ones need revamping:
# source("R/sandSiltBreak.R")
# sandSiltBreak(x)
# 

