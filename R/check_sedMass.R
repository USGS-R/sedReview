#' check_sedMass. Check and flag sediment mass (P91157) less than 2mg
#' @description Function to check and flag sediment mass (P91157) less than 2mg
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Default is FALSE
#' @param reviewSummary logical, for center-level review, if \code{TRUE} a summary count of flags by site and water year is returned
#' instead of individual flagged samples.
#' @details Checks parameter code 91157 (Suspended sediment, sediment mass recovered from whole water sample, dry weight, grams)
#' and flags samples < 2mg (< 0.002 g) with flag and result value.
#' @details Rejected samples are not included.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' check_sedMassOut <- check_sedMass(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

check_sedMass <- function(x, returnAll = FALSE, reviewSummary = FALSE){
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
  #get records for sediment mass pcode 91157
  sedMass <- x[x$PARM_CD == '91157',]
  sedMass <- unique(sedMass[c('UID','RESULT_VA')])
  
  # flag samples with sediment mass less than 2mg (0.002 g)
  sedMass$sedMassflag[sedMass$RESULT_VA < 0.002] <- paste0("flag, low (<2mg) sediment mass: ", sedMass$RESULT_VA[sedMass$RESULT_VA < 0.002], " g")
  
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT","WY",
                               "MEDIUM_CD")])
  flaggedSamples <- dplyr::left_join(flaggedSamples, sedMass[c("UID", "sedMassflag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$sedMassflag)==FALSE, ]
  }
  
  if(reviewSummary == TRUE){
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$sedMassflag)==FALSE, ]
    flaggedSamples <- dplyr::summarise(dplyr::group_by(flaggedSamples,SITE_NO,STATION_NM,WY),
                                    sed_mass_flags = length(sedMassflag))
    flagSummary <- unique(x[c('SITE_NO',
                              'STATION_NM',
                              'WY')])
    flagSummary <- dplyr::left_join(flagSummary, flaggedSamples, by = c('SITE_NO','STATION_NM','WY'))
    flagSummary[is.na(flagSummary)] <- 0
    
    return(flagSummary)
  }
  
  
  return(flaggedSamples)
}

