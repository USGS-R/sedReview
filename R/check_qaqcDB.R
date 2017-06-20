#' check_qaqcDB
#' @description Function to check if SSC, bedload, or bedload mass are coded as WSQ and in the QA/QC database
#' @param x A \code{dataframe} output from \code{get_localNWIS}
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
#' @export
#' @return A dataframe containing all samples with applicable flags
#' @seealso \code{\link[sedReview]{get_localNWIS}}

check_qaqcDB <- function(x, returnAll = FALSE)
{
  x <- x[x$PARM_CD %in% c("80154","80225","91145") & x$MEDIUM_CD == "WSQ", ]
  
  
  
}
