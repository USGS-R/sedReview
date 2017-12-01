#' find_boxcoef
#' @description Function to find box coefficient pairs between SSC point samples (pumped or grab) and SSC cross-section samples (EWI,EDI,EWT) at a site.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param site_no Character of a site number in the x \code{dataframe} from \code{get_localNWIS}
#' @param timediff Number of hours to look before and after a point sample for a paired cross-section sample.
#' Default is 1 (ie. look for a paired sample 1 hour before and 1 hour after a point sample timestamp)
#' @details Returns a dataframe of paired samples at given site_no for SSC (P80154). A summary count of all box coefficient pairs for all sites in
#' \code{x} can be found using the related function \code{summary_boxcoef}. 
#' @details Point samples are defined as samples where sampling method (P82398) is 30 (single vertical), 50 (point sample), 900 (SS pumping),
#' or 920 (SS BSV DI att strctr). Or samples where sampler type (P84164) is 3070 (grab sample) or 4115 (auto-sampler).
#' @details Cross-section samples are defined as samples where sampling method (P82398) is 10 (EWI), 15 (multiple verticals EWT), or 20 (EDI)   
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' 
#' #find box coefficient sample pairs at site 05586300
#' boxcoef_05586300 <- find_boxcoef(x, site_no = "05586300")
#' 
#' #find box coefficient sample pairs at site 06934500 
#' #and expand paired sample window to +/- 2 hours from point sample
#' boxcoef_06934500 <- find_boxcoef(x, site_no = "06934500", timediff = 2)
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing sample pairs with SSC result values of point and cross-section sample, with date/time stamps.
#' @seealso \code{\link[sedReview]{get_localNWIS}}

find_boxcoef <- function(x, site_no, timediff = 1){
  #Convert timediff to seconds
  timediff <- timediff * 60 * 60
  
  #SSC samples
  SSC <- x[x$SITE_NO == site_no & x$PARM_CD == "80154",c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT","RESULT_VA")]
  SSC <- unique(SSC)
  #Samples with pump or grab method and corresponds to an SSC sample
  methodPG <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(30,50,900,920) & x$UID %in% SSC$UID, 
                c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT")]
  
  #Samples with pump or grab sampler type and corresponds to an SSC sample
  samplerPG <- x[x$PARM_CD == "84164" & x$RESULT_VA %in% c(3070,4115) & x$UID %in% SSC$UID,
                 c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT")]
  #Combine pump or grab samples
  PGsample <- rbind(methodPG, samplerPG)
  PGsample <- unique(PGsample)
  #Samples with EWI,EWT,EDI method and corresponds to an SSC sample. Call it x-section sample
  methodX <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(10,15,20) & x$UID %in% SSC$UID, c("UID","SAMPLE_START_DT")]
  methodX <- unique(methodX)
  if(nrow(PGsample) == 0 | nrow(methodX) == 0){
    sitePairs <- as.data.frame(matrix(nrow = 0,ncol = 8))
    names(sitePairs) <- c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT_point","RESULT_VA_point",
                          "RESULT_VA_xsection","SAMPLE_START_DT_xsection","flow_cfs")
    return(sitePairs)
  }
  #Internal function to find index of paired samples by input time difference. Time input in seconds via as.numeric(POSIXct)
  #Basis for function borrowed from smwrBase::mergeNearest
  
  timeIndex <- function(time, pairTimes, maxdiff){
    diff <- abs(time - pairTimes)                  #get absolute value of difference between time and all times in compared dataset
    mindiff <- min(diff, na.rm = TRUE)             #find minimum difference
    if(mindiff > maxdiff){return(0)}               #if minimum difference < than max difference, no paired samples
    else{return(which(mindiff == diff)[1])}        #if pairs, return index of first pair (not sure there'd be more than 1??)
  }
  
  #Indexes of PGsample/methodE pairs
  pairs <- sapply(as.numeric(PGsample$SAMPLE_START_DT), timeIndex,
                  pairTimes = as.numeric(methodX$SAMPLE_START_DT), maxdiff = timediff)
  if(length(pairs)==0){pairs <- 0}
  
  #dataframe of only pairs
  #remove unmatched
  PGsample <- PGsample[pairs > 0,]
  methodX <- methodX[pairs[pairs >0],]
  #SSC samples for pump/grab and EWI/EDI/EWT
  SSC_PG <- dplyr::left_join(PGsample,SSC[,c("UID","RESULT_VA")],by = c("UID" = "UID"))
  SSC_X <- dplyr::left_join(methodX,SSC[,c("RESULT_VA","UID")],by = c("UID" = "UID"))
  #reorder columns
  SSC_X <- SSC_X[,c("RESULT_VA","SAMPLE_START_DT","UID")]
  #rename columns and combine
  names(SSC_PG) <- c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT_point","RESULT_VA_point")
  names(SSC_X) <- c("RESULT_VA_xsection","SAMPLE_START_DT_xsection","UID_xsection")
  sitePairs <- cbind(SSC_PG,SSC_X)
  
  #Get flow data
  qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208"), c("UID","PARM_CD","RESULT_VA")]
  qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] * 35.3147 #cms to cfs
  qRecords <- qRecords[,c("UID","RESULT_VA")]
  names(qRecords) <- c("UID", "flow_cfs")
  #Join flow metadata to pairs
  sitePairs <- dplyr::left_join(sitePairs,qRecords, by = c("UID_xsection" = "UID"))
  sitePairs <- sitePairs[,c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT_point","RESULT_VA_point",
                            "RESULT_VA_xsection","SAMPLE_START_DT_xsection","flow_cfs")]
  return(sitePairs)
}

