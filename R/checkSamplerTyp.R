#' checkSamplerTyp
#' @description Function to flag uncommon sampler type
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details Function determines most common sampler type from dataset or records and flags all others that are not most common.
#' @details Note, there are valid reasons for different sampler types, this is just a check.
#' @examples 
#' data("testData",package="sedReview")
#' x <- testData
#' samplerTypFlags <- checkSamplerTyp(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
checkSamplerTyp <- function(x, returnAll = FALSE){
  # sampler types
  sampler <- x[x$PARM_CD == "84164", ]
  sampler <- unique(sampler[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA")])
  
  # table of unique sampler types
  sampTyp <- as.data.frame(table(sampler$RESULT_VA))
  names(sampTyp) <- c("SamplerType", "count")
  sampTyp$SamplerType <- as.character(sampTyp$SamplerType)
  # most common sampler type
  mainType <- sampTyp$SamplerType[sampTyp$count %in% max(sampTyp$count)]
  
  # flag sampler types other than most common
  sampler$sampTypFlag[sampler$RESULT_VA != mainType] <- paste("flag uncommon sampler type ", sampler$RESULT_VA[sampler$RESULT_VA != mainType])
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, sampler[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA", "sampTypFlag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$sampTypFlag)==FALSE, ]
  }
  
  
  return(flaggedSamples)
}