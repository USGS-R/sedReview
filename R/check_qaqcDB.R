#' check_qaqcDB.
#' Check if SSC, bedload, or bedload mass are coded as WSQ and in the QA/QC database
#' @description Function to check if SSC, bedload, or bedload mass are coded as WSQ and in the QA/QC database
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param qa.db A character string containing the database number of QA samples
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Default is FALSE
#' @param reviewSummary logical, for center-level review, if \code{TRUE} a summary count of flags by site and water year is returned
#' instead of individual flagged samples.
#' @details Function finds and flags SSC (P80154), bedload (P80225), and bedload mass (P91145) results
#' that are coded as medium_cd "WSQ" and stored in the user defined QA/QC database (default is qa.db = "02") downloaded with \code{get_localNWIS}
#' @examples 
#' \dontrun{
#' #NOTE: exampleData contains no QA/QC results and will not return any flagged samples
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' check_qaqcDBOut <- check_qaqcDB(x)
#' }
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr matches
#' @importFrom stringi stri_sub
#' @export
#' @return A dataframe containing all samples with applicable flags
#' @seealso \code{\link[sedReview]{get_localNWIS}}

check_qaqcDB <- function(x, 
                         qa.db = "02",
                         returnAll = FALSE, reviewSummary = FALSE)
{
  # extract SSC, Bedload, Bedload mass Pcodes and WSQ medium code
  qaqc <- x[x$PARM_CD %in% c("80154","80225","91145") & x$MEDIUM_CD == "WSQ", ]
  
  # add database number column to extracted data, extracted from UID string 
  qaqc$DB <- stringi::stri_sub(qaqc$UID, from = -2, to = -1)
  
  # select only samples in user defined QAQC database and flag
  qaqc$qaqcFlag[qaqc$DB == qa.db] <- "SED sample in QAQC DB"
  qaqc <- dplyr::select(qaqc, -dplyr::matches("DB"))
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",'WY',
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, qaqc[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA", "qaqcFlag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$qaqcFlag)==FALSE, ]
  }
  
  if(reviewSummary == TRUE){
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$qaqcFlag)==FALSE, ]
    flaggedSamples <- dplyr::summarise(dplyr::group_by(flaggedSamples,SITE_NO,STATION_NM,WY),
                                       QAQC_DB_flags = length(qaqcFlag))
    flagSummary <- unique(x[c('SITE_NO',
                              'STATION_NM',
                              'WY')])
    flagSummary <- dplyr::left_join(flagSummary, flaggedSamples, by = c('SITE_NO','STATION_NM','WY'))
    flagSummary[is.na(flagSummary)] <- 0
    
    return(flagSummary)
  }
  
  return(flaggedSamples)
  
}
