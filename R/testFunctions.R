# pull station data to test
staid <- c("09163500")
library(lubridate)
qwdata <- readNWISodbc(DSN = "NWISCO", STAIDS = staid)
x <- qwdata$PlotTable

# for testing function calls
