#' check_hasQ. Searches for samples that were collected for sediment data but are missing discharge measurements
#' 
#' @description Searches for samples that were collected for sediment data but are missing discharge measurements
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param returnAll Logical. Return dataframe containing all samples if \code{TRUE} or only return samples missing discharge if \code{FALSE}. Default is \code{FALSE}
#' @return A data.frame of samples, what sediment data are available, and then a flag for missing discharge along with what discharge parameter are present
#' @examples
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' checkQOut <- check_hasQ(x,returnAll = FALSE)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom reshape2 dcast
#' @export
#' 


check_hasQ <- function(x, returnAll = FALSE) {
  x <- x[c("UID","RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT", "MEDIUM_CD","PARM_CD","DQI_CD","RESULT_VA")]
  x <- unique(x)
  
  sedRecords <- x[x$PARM_CD %in% c("80254","80154","80155","80225","00496","00535"),]
  qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208", "30209", "50042", "72137", "72243", "99060", "99061"),]
  
  if(nrow(sedRecords)==0){
    #print('Site contains no sediment specific records (Parameter codes: "80254","80154","80155","80225","00496","00535")')
    stop('Site contains no sediment specific records (Parameter codes: "80254","80154","80155","80225","00496","00535")')
  }
  
  sedRecords <- reshape2::dcast(sedRecords,UID+RECORD_NO+SITE_NO+STATION_NM+SAMPLE_START_DT+MEDIUM_CD~PARM_CD,value.var="RESULT_VA")
  qRecords <- reshape2::dcast(qRecords,UID~PARM_CD,value.var="RESULT_VA")
  
  
  sedRecords$hasQ <- ifelse(sedRecords$UID %in% qRecords$UID,TRUE,FALSE)
  
  Qsum <- dplyr::left_join(sedRecords,qRecords,by=c("UID"))
  
  if(returnAll == FALSE) {
    Qsum <- Qsum[Qsum$hasQ == FALSE,]
  }
  
  return(Qsum)
}
