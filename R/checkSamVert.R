#' checkSamVert
#' @description Function to check if number of verticals in EDI/EWI samples is correct
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE

#' @details function to test EDI sample collected correctly. Number of sampling verticals 4-9.
#' @details function to test EWI sample collected correctly. Number of verticals between 10 and 20.
#' @examples 
#' data("testData",package="sedReview")
#' x <- testData
#' sampleVertFlags <- checkSamVert(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
checkSamVert <- function(x, returnAll = FALSE){
  
  # Select all EDI and EWI samples
  EDI <- x[x$PARM_CD == "82398", ]
  EWI <- EDI[EDI$RESULT_VA == "10", ]
  EWI <- unique(EWI[c("UID")])
  EDI <- EDI[EDI$RESULT_VA == "20", ]
  EDI <- unique(EDI[c("UID")])
  # select number of verticals parameter (sampling points)
  verts <- x[x$PARM_CD == "00063", ]
  verts <- verts[c("UID", "RESULT_VA")]
  verts <- unique(verts[c("UID", "RESULT_VA")])
  #join number of verticals to EWI/EDI dataframes
  EWI <- dplyr::left_join(EWI, verts, by = "UID")
  EDI <- dplyr::left_join(EDI, verts, by = "UID")
  
  # set flags
  EWI$EWIvertflag[is.na(EWI$RESULT_VA)==TRUE] <- paste("flag # EWI verticals missing")
  EWI$EWIvertflag[EWI$RESULT_VA > 20 & is.na(EWI$RESULT_VA)==FALSE] <- paste("flag high # EWI verticals=", 
                                                                          EWI$RESULT_VA[EWI$RESULT_VA > 20 & is.na(EWI$RESULT_VA)==FALSE])
  EWI$EWIvertflag[EWI$RESULT_VA < 10 & is.na(EWI$RESULT_VA)==FALSE] <- paste("flag low # EWI verticals=",
                                                                          EWI$RESULT_VA[EWI$RESULT_VA < 10 & is.na(EWI$RESULT_VA)==FALSE])
  
  EDI$EDIvertflag[is.na(EDI$RESULT_VA)==TRUE] <- paste("flag # EDI verticals missing")
  EDI$EDIvertflag[EDI$RESULT_VA > 9 & is.na(EDI$RESULT_VA)==FALSE] <- paste("flag high # EDI verticals=", 
                                                                          EDI$RESULT_VA[EDI$RESULT_VA > 9 & is.na(EDI$RESULT_VA)==FALSE])
  EDI$EDIvertflag[EDI$RESULT_VA < 4 & is.na(EDI$RESULT_VA)==FALSE] <- paste("flag low # EDI verticals=",
                                                                          EDI$RESULT_VA[EDI$RESULT_VA < 4 & is.na(EDI$RESULT_VA)==FALSE])
  
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  flaggedSamples <- dplyr::left_join(flaggedSamples, EWI[c("UID", "EWIvertflag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, EDI[c("UID", "EDIvertflag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$EWIvertflag)==FALSE | 
                                       is.na(flaggedSamples$EDIvertflag)==FALSE, ]
  }
  
  return(flaggedSamples)
}