#' Function to summarize samples that are not approved
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll Only return results that have been flagged somewhere
#' @details Summarizes automated check tables and displays records numbers for unapproved data (DQI of I, S, or P) and number of flags
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' flagSummaryOut <- flagSummary(x, returnAll = FALSE)
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr transmute
#' @export
#' @return A dataframe containing all samples with applicable flags

flagSummary <- function(x,returnAll = FALSE, returnAllTables = FALSE)
{
  
  #Bag IE
  bagIEFlags <- check_bagIE(x)
  
  #has Q
  hasQFlags <- check_hasQ(x,returnAll = FALSE)
  
  #Coding and meta data
  metaDataFlags <- check_metaData(x)
  
  #Sample purpose
  samplePurpFlags <- check_samplePurp(x)
  
  #Sampler type
  samplerTypeFlags <- check_samplerType(x)
  
  #TSS has SSC
  tssFlags <- check_tss(x)
  
  #Number of verticles
  verticlesFlags <- check_verticles(x)
  
  #Count sampling methods
  methodsBySite <- count_methodsBySite(x)
  
  #Count sample status
  sampleStatus <- count_sampleStatus(x,bySite = TRUE)
  
  #Find outliers
  outliers <- find_outliers(x)
  
  
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
                           methodsBySite = ifelse(UID %in% methodsBySite$UID,TRUE,FALSE),
                           sampleStatus = ifelse(UID %in% sampleStatus$UID,TRUE,FALSE),
                           outliers = ifelse(UID %in% outliers$UID,TRUE,FALSE)
  )
                           
                           

  flaggedSamples <- dplyr::left_join(flaggedSamples,temp,by=c("UID","PARM_CD"))
  
  if(returnAll == FALSE) {
    flaggedSamples <- filter(flaggedSamples,bagIEFlags |
                               QFlags |
                               metaDataFlags |
                               samplePurpFlags |
                               samplerTypeFlags |
                               tssFlags |
                               verticlesFlags |
                               methodsBySite |
                               sampleStatus |
                               outliers)
  } 
  
  ###Put in option to return all the flagged tables in a list
  
  return(flaggedSamples)
  
  
  
}