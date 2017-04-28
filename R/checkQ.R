#' checkQ
#' 
#' @description Searches for samples that were collected for sediment data but are missing discharge measurements
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll Logical. Return dataframe containing all samples if \code{TRUE} or only return samples missing discharge if \code{FALSE}. Defualt is \code{FALSE}
#' @return A data.frame of samples, what sediment data are available, and then a flag for missing discharge along with what discharge parameter are present
#' @examples
#' data("exampleData",package="sedReview")
#' x <- exampleData$longTable
#' checkQ(x,bySite = TRUE)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' 


checkQ <- function(x, returnAll = FALSE) {
  x <- x[c("RECORD_NO","SITE_NO","SAMPLE_START_DT","SAMPLE_END_DT", "MEDIUM_CD","PARM_CD","DQI_CD","RESULT_VA")]
  x <- unique(x)
  
  sedRecords <- x[x$PARM_CD %in% c("80254","80154","80155","80225","00496","00535"),]
  qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208", "30209", "50042", "72137", "72243", "99060", "99061"),]
  

  sedRecords <- reshape2::dcast(sedRecords,SITE_NO+RECORD_NO~PARM_CD,value.var="RESULT_VA")
  qRecords <- reshape2::dcast(qRecords,SITE_NO+RECORD_NO~PARM_CD,value.var="RESULT_VA")
  
  
  sedRecords$hasQ <- ifelse(sedRecords$RECORD_NO %in% qRecords$RECORD_NO,TRUE,FALSE)
  
  Qsum <- dplyr::left_join(sedRecords,qRecords,by=c("SITE_NO","RECORD_NO"))
  
  return(Qsum)
}
