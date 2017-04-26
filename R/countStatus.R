#' countStatus
#' 
#' @description Returns counts of sample status (DQI_CD) by parameter for each site or for multiple sites together
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param bySite Logical. Count by site if \code{TRUE} or for all sites if \code{FALSE}
#' @return A data.frame tabular summary of counts of sample status (DQI_CD) by parameter
#' @examples
#' data("exampleData",package="sedReview")
#' x <- exampleData$longTable
#' countStatus(x,bySite = TRUE)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' 


countStatus <- function(x, bySite = TRUE) {
  x <- x[c("RECORD_NO","SITE_NO","PARM_CD","DQI_CD","RESULT_VA")]
  x <- unique(x)
  
  if(bySite == TRUE){
  statusSum <- dplyr::summarise(dplyr::group_by(x,SITE_NO,PARM_CD,DQI_CD),
                                N = length(RESULT_VA)
  )
  statusSum <- reshape2::dcast(statusSum,SITE_NO+PARM_CD~DQI_CD,value.var = "N")
  
  } else {
    statusSum <- dplyr::summarise(dplyr::group_by(x,PARM_CD,DQI_CD),
                                  N = length(RESULT_VA)
    )
    statusSum <- reshape2::dcast(statusSum,PARM_CD~DQI_CD,value.var = "N")
  }
  
  statusSum[is.na(statusSum)] <- 0
  return(statusSum)
}
