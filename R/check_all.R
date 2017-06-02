#' Function to run all checks on a dataset
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAllTables Return all tables of flagged results
#' @details Runs all check_ functions and outputs a summary dataframe of flagged samples or a list of all flag results if \code{returnAllTables = TRUE}
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' allChecks <- check_all(x, returnAll = FALSE, returnAllTables = FALSE)
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr transmute
#' @export
#' @return A dataframe containing all samples with applicable flags

check_all <- function(x, returnAllTables = FALSE)
{
  
  #Bag IE
  bagIEFlags <- check_bagIE(x, returnAll = FALSE)
  
  #has Q
  hasQFlags <- check_hasQ(x, returnAll = FALSE)
  
  #Coding and meta data
  metaDataFlags <- check_metaData(x, returnAll = FALSE)
  
  #Sample purpose
  samplePurpFlags <- check_samplePurp(x, returnAll = FALSE)
  
  #Sampler type
  samplerTypeFlags <- check_samplerType(x, returnAll = FALSE)
  
  #TSS has SSC
  tssFlags <- check_tss(x, returnAll = FALSE)
  
  #Number of verticles
  verticlesFlags <- check_verticles(x, returnAll = FALSE)
  
  #Count sampling methods
  methodsBySite <- count_methodsBySite(x)
  
  #Count sample status
  sampleStatus <- count_sampleStatus(x,bySite = TRUE)
  
  #Find outliers
  outliers <- find_outliers(x, returnAll = FALSE)
  
  
  flaggedSamples <- x[x$DQI_CD %in% c("I","S","P"),
                      c("UID","RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","PARM_CD","PARM_NM","PARM_SEQ_GRP_CD","DQI_CD")]
  
  
  flaggedSamples <- unique(flaggedSamples)
  
  temp <- dplyr::summarize(group_by(flaggedSamples,UID,PARM_CD),
                           bagIEFlags = ifelse(UID %in% bagIEFlags$UID,TRUE,FALSE),
                           QFlags = ifelse(UID %in% hasQFlags$UID,TRUE,FALSE),
                           metaDataFlags = ifelse(UID %in% metaDataFlags$UID,TRUE,FALSE),
                           samplePurpFlags = ifelse(UID %in% samplePurpFlags$UID,TRUE,FALSE),
                           samplerTypeFlags = ifelse(UID %in% samplerTypeFlags$UID,TRUE,FALSE),
                           tssFlags = ifelse(UID %in% tssFlags$UID,TRUE,FALSE),
                           verticlesFlags = ifelse(UID %in% verticlesFlags$UID,TRUE,FALSE),
                           outliers = ifelse(UID %in% outliers$UID,TRUE,FALSE)
  )
  
  
  
  flaggedSamples <- dplyr::left_join(flaggedSamples,temp,by=c("UID","PARM_CD"))
  
  flaggedSamples <- filter(flaggedSamples,bagIEFlags |
                             QFlags |
                             metaDataFlags |
                             samplePurpFlags |
                             samplerTypeFlags |
                             tssFlags |
                             verticlesFlags |
                             outliers)
  
  
  if(returnAllTables == TRUE)
  {
    return(list(flaggedSamples=flaggedSamples,
                bagIEFlags = bagIEFlags,
                hasQFlags = hasQFlags,
                metaDataFlags = metaDataFlags,
                samplePurpFlags = samplePurpFlags,
                samplerTypeFlags = samplerTypeFlags,
                tssFlags = tssFlags,
                verticlesFlags = verticlesFlags,
                methodsBySite = methodsBySite,
                sampleStatus = sampleStatus,
                outliers = outliers))
  } else {
    return(flaggedSamples)
  }
  
  
}