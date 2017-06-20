#' check_qaqcDB
#' @description Function to check if SSC, bedload, or bedload mass are coded as WSQ and in the QA/QC database
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param qa.db A character string containing the database number of QA samples
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details Function finds and flags SSC (P80154), bedload (P80225), and bedload mass (P91145) results
#' that are coded as medium_cd "WSQ" and stored in the user defined QA/QC database (default is qa.db = "02") downloaded with \code{get_localNWIS}
#' @details function to test EWI sample collected correctly. Number of verticals between 10 and 20.
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
                         returnAll = FALSE)
{
  # extract SSC, Bedload, Bedload mass Pcodes and WSQ medium code
  qaqc <- x[x$PARM_CD %in% c("80154","80225","91145") & x$MEDIUM_CD == "WSQ", ]
  
  # add database number column to extracted data, extracted from UID string 
  qaqc$DB <- stringi::stri_sub(qaqc$UID, from = -2, to = -1)
  
  # select only samples in user defined QAQC database
  qaqc <- qaqc[qaqc$DB == qa.db, ]
  
  # flag samples
  qaqc$qaqcFlag <- "SED sample in QAQC DB"
  qaqc <- dplyr::select(qaqc, -dplyr::matches("DB"))
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, qaqc[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA", "qaqcFlag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$qaqcFlag)==FALSE, ]
  }
  
  
  return(flaggedSamples)
  
}
