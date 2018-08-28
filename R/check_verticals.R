#' check_verticals. Check if number of verticals in EDI/EWI samples is correct
#' @description Function to check if number of verticals in EDI/EWI samples is correct
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Default is FALSE
#' @param reviewSummary logical, for center-level review, if \code{TRUE} a summary count of flags by site and water year is returned
#' instead of individual flagged samples.
#' @details function to test EDI sample collected correctly. Number of sampling verticals 4-9.
#' @details function to test EWI sample collected correctly. Number of verticals between 10 and 20.
#' @details Rejected samples are not included.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' check_verticalsOut <- check_verticals(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
check_verticals <- function(x, returnAll = FALSE, reviewSummary = FALSE){
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
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
  
  #get stream widths
  stream <- x[x$PARM_CD == '00004',]
  stream <- stream[c('UID','RESULT_VA')]
  stream <- unique(stream[c('UID','RESULT_VA')])
  names(stream) <- c('UID','stream_width_ft')
  
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
  # set center-level summary flags
  EWI$EWIsumflag[is.na(EWI$RESULT_VA)==TRUE] <- 0
  EWI$EWIsumflag[EWI$RESULT_VA > 20 & is.na(EWI$RESULT_VA)==FALSE] <- 1
  EWI$EWIsumflag[EWI$RESULT_VA < 10 & is.na(EWI$RESULT_VA)==FALSE] <- -1
  
  EDI$EDIsumflag[is.na(EDI$RESULT_VA)==TRUE] <- 0
  EDI$EDIsumflag[EDI$RESULT_VA > 9 & is.na(EDI$RESULT_VA)==FALSE] <- 1
  EDI$EDIsumflag[EDI$RESULT_VA < 4 & is.na(EDI$RESULT_VA)==FALSE] <- -1
  
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT","WY",
                               "MEDIUM_CD")])
  flaggedSamples <- dplyr::left_join(flaggedSamples, stream, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, EWI[c("UID", "EWIvertflag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, EDI[c("UID", "EDIvertflag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$EWIvertflag)==FALSE | 
                                       is.na(flaggedSamples$EDIvertflag)==FALSE, ]
  }
  
  if(reviewSummary == TRUE){
    
    flaggedSamples <- dplyr::left_join(flaggedSamples, EWI[c("UID", "EWIsumflag")], by = "UID")
    flaggedSamples <- dplyr::left_join(flaggedSamples, EDI[c("UID", "EDIsumflag")], by = "UID")

    flagSummary <- unique(x[c('SITE_NO',
                              'STATION_NM',
                              'WY')])
    missingEWI <- flaggedSamples[flaggedSamples$EWIsumflag == 0 & !is.na(flaggedSamples$EWIsumflag),]
    if(nrow(missingEWI)>0){
      missingEWI <- dplyr::summarise(dplyr::group_by(missingEWI,SITE_NO, STATION_NM, WY),
                                   EWI_missing_vert = length(EWIsumflag))
      flagSummary <- dplyr::left_join(flagSummary, missingEWI, by = c('SITE_NO','STATION_NM','WY'))
      }else{flagSummary$EWI_missing_vert <- NA}
    
    highEWI <- flaggedSamples[flaggedSamples$EWIsumflag == 1 & !is.na(flaggedSamples$EWIsumflag),]
    if(nrow(highEWI)>0){
      highEWI <- dplyr::summarise(dplyr::group_by(highEWI,SITE_NO, STATION_NM, WY),
                                  EWI_high_vert = length(EWIsumflag))
      flagSummary <- dplyr::left_join(flagSummary, highEWI, by = c('SITE_NO','STATION_NM','WY'))
    }else{flagSummary$EWI_high_vert <- NA}
    
    lowEWI <- flaggedSamples[flaggedSamples$EWIsumflag == -1 & !is.na(flaggedSamples$EWIsumflag),]
    if(nrow(lowEWI)>0){
      lowEWI <- dplyr::summarise(dplyr::group_by(lowEWI,SITE_NO, STATION_NM, WY),
                                  EWI_low_vert = length(EWIsumflag))
      flagSummary <- dplyr::left_join(flagSummary, lowEWI, by = c('SITE_NO','STATION_NM','WY'))
    }else{flagSummary$EWI_low_vert <- NA}
    
    missingEDI <- flaggedSamples[flaggedSamples$EDIsumflag == 0 & !is.na(flaggedSamples$EDIsumflag),]
    if(nrow(missingEDI)>0){
      missingEDI <- dplyr::summarise(dplyr::group_by(missingEDI,SITE_NO, STATION_NM, WY),
                                     EDI_missing_vert = length(EDIsumflag))
      flagSummary <- dplyr::left_join(flagSummary, missingEDI, by = c('SITE_NO','STATION_NM','WY'))
    }else{flagSummary$EDI_missing_vert <- NA}
    
    highEDI <- flaggedSamples[flaggedSamples$EDIsumflag == 1 & !is.na(flaggedSamples$EDIsumflag),]
    if(nrow(highEDI)>0){
      highEDI <- dplyr::summarise(dplyr::group_by(highEDI,SITE_NO, STATION_NM, WY),
                                  EDI_high_vert = length(EDIsumflag))
      flagSummary <- dplyr::left_join(flagSummary, highEDI, by = c('SITE_NO','STATION_NM','WY'))
    }else{flagSummary$EDI_high_vert <- NA}
    
    lowEDI <- flaggedSamples[flaggedSamples$EDIsumflag == -1 & !is.na(flaggedSamples$EDIsumflag),]
    if(nrow(lowEDI)>0){
      lowEDI <- dplyr::summarise(dplyr::group_by(lowEDI,SITE_NO, STATION_NM, WY),
                                 EDI_low_vert = length(EDIsumflag))
      flagSummary <- dplyr::left_join(flagSummary, lowEDI, by = c('SITE_NO','STATION_NM','WY'))
    }else{flagSummary$EDI_low_vert <- NA}
    
    flagSummary[is.na(flagSummary)] <- 0
    return(flagSummary)
  }
  
  
  return(flaggedSamples)
}
