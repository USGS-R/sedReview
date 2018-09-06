#' check_tss. Flag TSS results without an accompanying SSC result
#' @description Function to flag TSS results without an accompanying SSC result
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Default is FALSE
#' @param reviewSummary logical, for center-level review, if \code{TRUE} a summary count of flags by site and water year is returned
#' instead of individual flagged samples.
#' @details See OSW Technical Memo No. 2001.03 for more details and background information
#' @details Rejected samples are not included.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' check_tssOut <- check_tss(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
check_tss <- function(x, returnAll = FALSE, reviewSummary = FALSE){
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
  ### extract TSS results
  TSS <- x[x$PARM_CD == "00530",]
  TSS <- unique(TSS[c("UID", "RESULT_VA")])
  ### extract SSC results
  SSC <- x[x$PARM_CD == "80154",]
  SSC <- unique(SSC[c("UID", "RESULT_VA")])
  names(SSC) <- c("UID", "RESULT_VA_SSC")
  
  ### add SSC results to TSS and compare
  TSS <- dplyr::left_join(TSS, SSC, by = "UID")
  TSS$OSW2001_03Flag[is.na(TSS$RESULT_VA)==FALSE & is.na(TSS$RESULT_VA_SSC)==TRUE] <- paste("flag TSS without SSC")
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT","WY",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, TSS[c("UID", "OSW2001_03Flag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$OSW2001_03Flag)==FALSE, ]
  }
  
  if(reviewSummary == TRUE){
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$OSW2001_03Flag)==FALSE, ]
    flaggedSamples <- dplyr::summarise(dplyr::group_by(flaggedSamples,SITE_NO,STATION_NM,WY),
                                       OSW2001_03_flags = length(OSW2001_03Flag))
    flagSummary <- unique(x[c('SITE_NO',
                              'STATION_NM',
                              'WY')])
    flagSummary <- dplyr::left_join(flagSummary, flaggedSamples, by = c('SITE_NO','STATION_NM','WY'))
    flagSummary[is.na(flagSummary)] <- 0
    
    flagSummary <- flagSummary[flagSummary$OSW2001_03_flags != 0,]
    
    return(flagSummary)
  }
  
  return(flaggedSamples)
}

