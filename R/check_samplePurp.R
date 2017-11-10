#' check_samplePurp
#' @description Function to flag uncommon sample purpose
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details Function determines most common sample purpose for all retrieved records and flags all sample purposes other than the most common.
#' @details There are of course valid reasons to have different sample purposes, this is just a check.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' samplePurpFlags <- check_samplePurp(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
check_samplePurp <- function(x, returnAll = FALSE){
  # sample purposes
  purp <- x[x$PARM_CD == "71999", ]
  purp <- unique(purp[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA")])
  
  if(nrow(purp)==0){
    stop("All pulled record numbers missing sample purpose (PARM_CD 71999)")
  }
  
  
  # table of unique sample purposes
  sampPurp <- as.data.frame(table(purp$RESULT_VA))
  names(sampPurp) <- c("SamplePurpose", "count")
  sampPurp$SamplePurpose <- as.character(sampPurp$SamplePurpose)
  # most common samp purpose
  mainPurp <- sampPurp$SamplePurpose[sampPurp$count %in% max(sampPurp$count)]
  
  # flag sample purposes other than most common
  purp$sampPurpFlag[purp$RESULT_VA != mainPurp] <- paste("flag uncommon sample purpose ", purp$RESULT_VA[purp$RESULT_VA != mainPurp])
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, purp[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA", "sampPurpFlag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$sampPurpFlag)==FALSE, ]
  }

  
  return(flaggedSamples)
}
