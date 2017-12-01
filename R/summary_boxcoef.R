#' summary_boxcoef
#' @description Function to summarise box coefficient pairs for multiple sites and years utilizing the find_boxcoef function
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param timediff Number of hours to look before and after a point sample for a paired cross-section sample.
#' Default is 1 (ie. look for a paired sample 1 hour before and 1 hour after a point sample timestamp)
#' @param returnAllTables Logical. If \code{TRUE}, return a list with the summary table and all individual site tables from the find_boxcoef function used internally.
#' Default is FALSE.
#' @details Function summarises box coefficient pairs by each site and WY. See documentation for \code{find_boxcoef} function for more details.
#' @examples 
#' data('exampleData2', package = "sedReview")
#' x <- exampleData2
#' boxCoef <- summary_boxcoef(x)
#' boxCoef_list <- summary_boxcoef(x, returnAllTables = TRUE)
#' \dontrun{
#' View(boxCoef_list$summary)
#' }
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @export
#' @return A dataframe of the summary counts of box coefficient pairs, or a list of the summary and individual site box_coef data
#' @seealso \code{\link[sedReview]{get_localNWIS}}, \code{\link[sedReview]{find_boxcoef}}

summary_boxcoef <- function(x,
                            timediff = 1,
                            returnAllTables = FALSE){
  #Get list of all site numbers in x dataframe from get_localNWIS
  sites <- unique(x$SITE_NO)
  
  #Run find_boxcoef function for each site
  allBC <- lapply(sites, function(y){
    sedReview::find_boxcoef(x, site_no = y, timediff = timediff)
  })
  names(allBC) <- sites
  
  #Summary dataframe of all sites and WY
  summary <- unique(x[c("SITE_NO","STATION_NM","WY")])
  
  #Function to summarise number of pairs in each WY and lapply on all site dataframes in allBC list
  countFun <- function(z){
    z$WY <- sedReview::waterYear(z$SAMPLE_START_DT_point, numeric = TRUE)
    pairCount <- dplyr::summarise(dplyr::group_by(z,SITE_NO,WY),
                                  numPairs = length(RESULT_VA_point))
  }
  sumallBC <- lapply(allBC,countFun)
  
  #Add summary counts to summary dataframe
  for(i in 1:length(sumallBC)){
    sitenum <- names(sumallBC[i])
    pairCountWY <- sumallBC[[i]]
    if(nrow(pairCountWY) > 0){
      summary$numPairs[summary$SITE_NO == sitenum & summary$WY %in% pairCountWY$WY] <- pairCountWY$numPairs
    }else{summary$numPairs[summary$SITE_NO == sitenum] <- 0}
  }
  summary$numPairs[is.na(summary$numPairs)] <- 0
  
  #Return all the box coefficient tables if TRUE
  if(returnAllTables == TRUE){
    summary <- modifyList(list(summary), allBC)
    names(summary) <- c("summary", names(allBC))
  }
  
  return(summary)
}

