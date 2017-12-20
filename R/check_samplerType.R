#' check_samplerType. Flag uncommon sampler type
#' @description Function to flag uncommon sampler type
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Default is FALSE
#' @details Function determines most common sampler type from dataset or records and flags all others that are not most common.
#' @details Note, there are valid reasons for different sampler types, this is just a check.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' check_samplerTypeOut <- check_samplerType(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
check_samplerType <- function(x, returnAll = FALSE){
  # sampler types
  sampler <- x[x$PARM_CD == "84164", ]
  sampler <- unique(sampler[c("UID","SITE_NO","PARM_CD", "PARM_NM", "RESULT_VA")])
  
  # table of unique sampler types
  sampTyp <- dplyr::summarise(dplyr::group_by(sampler, SITE_NO, RESULT_VA),
                               count = length(RESULT_VA))
  
  # most common sampler type
  mainType <- dplyr::summarise(dplyr::group_by(sampTyp,SITE_NO),
                               max = max(count))
  
  mainType <- dplyr::left_join(mainType, sampTyp, by = c('SITE_NO' = 'SITE_NO', 'max' = 'count'))
  mainType <- mainType[c('SITE_NO','RESULT_VA')]
  names(mainType) <- c('SITE_NO','mostCommonSampler')
  
  # flag sampler types other than most common
  sampler <- dplyr::left_join(sampler, mainType, by = 'SITE_NO')
  
  sampler$sampTypFlag[sampler$RESULT_VA != sampler$mostCommonSampler] <- paste("flag uncommon sampler type ", 
                                                                       sampler$RESULT_VA[sampler$RESULT_VA != sampler$mostCommonSampler])
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, 
                                     sampler[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA","mostCommonSampler","sampTypFlag")], 
                                     by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$sampTypFlag)==FALSE, ]
  }
  
  
  return(flaggedSamples)
}