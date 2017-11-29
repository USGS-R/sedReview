

data("exampleData", package = "sedReview")
x <- exampleData
x <- get_localNWIS('nwisco', STAIDS = '09163500')
timediff = 1 #hours

#SSC samples
SSC <- x[x$PARM_CD == "80154",]

#Samples with pump or grab method and corresponds to an SSC sample
methodPG <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(30,50,900,920) & x$UID %in% SSC$UID, 
              c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT")]

#Samples with pump or grab sampler type and corresponds to an SSC sample
samplerPG <- x[x$PARM_CD == "84164" & x$RESULT_VA %in% c(3070,4115) & x$UID %in% SSC$UID,]

#Samples with EWI,EWT,EDI method and corresponds to an SSC sample
methodE <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(10,15,20) & x$UID %in% SSC$UID, c("UID","SAMPLE_START_DT")]

#Internal function to find index of paired samples by input time difference. Time input in seconds via as.numeric(POSIXct)
#Basis for function borrowed from smwrBase::mergeNearest
  #Convert timediff to seconds
timediff <- timediff * 60 * 60
timeIndex <- function(time, pairTimes, maxdiff){
  diff <- abs(time - pairTimes)                  #get absolute value of difference between time and all times in compared dataset
  mindiff <- min(diff, na.rm = TRUE)             #find minimum difference
  if(mindiff > maxdiff){return(0)}               #if minimum difference < than max difference, no paired samples
  else{return(which(mindiff == diff)[1])}        #if pairs, return index of first pair (not sure there'd be more than 1??)
  }

#Indexes of methodPG/methodE and samplerPG/methodE pairs
methodPairs <- sapply(as.numeric(methodPG$SAMPLE_START_DT), timeIndex,
                      pairTimes = as.numeric(methodE$SAMPLE_START_DT), maxdiff = timediff)
samplerPairs <- sapply(as.numeric(samplerPG$SAMPLE_START_DT), timeIndex,
                       pairTimes = as.numeric(methodE$SAMPLE_START_DT), maxdiff = timediff)


pairs <- smwrBase::mergeNearest(left = methodPG, dates.left = "SAMPLE_START_DT", suffix.left = "PG",
                                right = methodE, dates.right = "SAMPLE_START_DT", suffix.right = "E",
                                Date.noon = FALSE, max.diff = "1 hours")

