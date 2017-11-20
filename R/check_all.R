#' check_all
#' @description Function to run all checks on a dataset
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param qa.db A character string containing the database number of QA samples (for check_qaqcDB function), should be same QA DB used in \code{get_localNWIS}
#' @param returnAllTables Return all tables of flagged results
#' @details Runs all check_, count_, and find_ functions and outputs a summary dataframe of flagged samples or a list of all flag results if \code{returnAllTables = TRUE}
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' allChecks <- check_all(x, returnAllTables = FALSE)
#' allChecksList <- check_all(x, returnAllTables = TRUE)
#' \dontrun{
#' view(allChecksList$flaggedSamples)
#' }
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr transmute
#' @export
#' @return A dataframe containing all samples with applicable flags
#' @seealso \code{\link[sedReview]{check_bagIE}}, \code{\link[sedReview]{check_hasQ}}, \code{\link[sedReview]{check_metaData}},
#' \code{\link[sedReview]{check_samplePurp}}, \code{\link[sedReview]{check_samplerType}}, 
#' \code{\link[sedReview]{check_tss}}, \code{\link[sedReview]{check_verticals}}, \code{\link[sedReview]{check_qaqcDB}}
#' \code{\link[sedReview]{count_methodsBySite}}, \code{\link[sedReview]{count_sampleStatus}}, \code{\link[sedReview]{find_outliers}}

check_all <- function(x, qa.db = "02", returnAllTables = FALSE)
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
  
  #Number of verticals
  verticalsFlags <- check_verticals(x, returnAll = FALSE)
  
  #SSC/Bedload/Bedload mass in QAQC DB
  qaqcFlags <- check_qaqcDB(x, qa.db, returnAll = FALSE)
  
  #Count sampling methods
  methodsBySite <- count_methodsBySite(x)
  
  #Count sample status
  sampleStatus <- count_sampleStatus(x,bySite = TRUE)
  
  #Find outliers
  outliers <- find_outliers(x, returnAll = FALSE)
  
  #Calculate sand and fines concentration
  concSandFine <- calc_concSandFine(x, plotTime = FALSE, plotFlow = FALSE)
  
  
  flaggedSamples <- x[x$DQI_CD %in% c("I","S","P"),
                      c("UID","RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","MEDIUM_CD","PARM_CD","PARM_NM","PARM_SEQ_GRP_CD","DQI_CD")]
  
  
  flaggedSamples <- unique(flaggedSamples)
  
  temp <- dplyr::summarize(dplyr::group_by(flaggedSamples,UID,PARM_CD),
                           bagIEFlags = ifelse(UID %in% bagIEFlags$UID,TRUE,FALSE),
                           QFlags = ifelse(UID %in% hasQFlags$UID,TRUE,FALSE),
                           metaDataFlags = ifelse(UID %in% metaDataFlags$UID,TRUE,FALSE),
                           samplePurpFlags = ifelse(UID %in% samplePurpFlags$UID,TRUE,FALSE),
                           samplerTypeFlags = ifelse(UID %in% samplerTypeFlags$UID,TRUE,FALSE),
                           tssFlags = ifelse(UID %in% tssFlags$UID,TRUE,FALSE),
                           verticalsFlags = ifelse(UID %in% verticalsFlags$UID,TRUE,FALSE),
                           qaqcFlags = ifelse(UID %in% qaqcFlags$UID,TRUE,FALSE),
                           outliers = ifelse(UID %in% outliers$UID,TRUE,FALSE)
  )
  
  
  
  flaggedSamples <- dplyr::left_join(flaggedSamples,temp,by=c("UID","PARM_CD"))
  
  flaggedSamples <- dplyr::filter(flaggedSamples, 
                                  bagIEFlags == T | 
                                    QFlags == T | 
                                    metaDataFlags == T |
                                    samplePurpFlags == T |
                                    samplerTypeFlags == T |
                                    tssFlags == T |
                                    verticalsFlags == T |
                                    qaqcFlags == T |
                                    outliers == T)
  
  flaggedSamples <- unique(flaggedSamples[c("UID",
                            "RECORD_NO",
                            "SITE_NO",
                            "STATION_NM",
                            "SAMPLE_START_DT",
                            "MEDIUM_CD",
                            "bagIEFlags",
                            "QFlags",
                            "metaDataFlags",
                            "samplePurpFlags", 
                            "samplerTypeFlags",
                            "tssFlags",
                            "verticalsFlags",
                            "qaqcFlags",
                            "outliers")])
  flaggedSamples[flaggedSamples == FALSE] <- ""
  flaggedSamples[flaggedSamples == TRUE] <- "flags present"
  # sort summary flags by site, date/time
  flaggedSamples <- flaggedSamples[order(flaggedSamples$SITE_NO, flaggedSamples$SAMPLE_START_DT),]

  
  if(returnAllTables == TRUE)
  {
    return(list(flaggedSamples=flaggedSamples,
                bagIEFlags = bagIEFlags,
                hasQFlags = hasQFlags,
                metaDataFlags = metaDataFlags,
                samplePurpFlags = samplePurpFlags,
                samplerTypeFlags = samplerTypeFlags,
                tssFlags = tssFlags,
                verticalsFlags = verticalsFlags,
                qaqcFlags = qaqcFlags,
                methodsBySite = methodsBySite,
                sampleStatus = sampleStatus,
                outliers = outliers,
                concSandFine = concSandFine))
  } else {
    return(flaggedSamples)
  }
  
  
}