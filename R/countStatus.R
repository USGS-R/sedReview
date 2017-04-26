#' countStatus
#' 
#' @description Returns counts of sample status (DQI_CD) by parameter for each site or for multiple sites together
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param bySite Logical. Count by site if \code{TRUE} or for all sites if \code{FALSE}
#' @return A data.frame tabular summary of counts of sample status (DQI_CD) by parameter
#' @examples
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' 

# data("exampleData",package="sedReview")
# x <- exampleData$longTable
# countStatus(x,bySite = TRUE)
countStatus <- function(x, bySite = TRUE) {
  
  
}