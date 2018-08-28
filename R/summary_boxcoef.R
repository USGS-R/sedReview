#' summary_boxcoef. Summarise box coefficient pairs for multiple sites and years utilizing the find_boxcoef function
#' @description Function to summarise box coefficient pairs for multiple sites and years utilizing the find_boxcoef function
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param timediff Number of hours to look before and after a point sample for a paired cross-section sample.
#' Default is 1 (ie. look for a paired samples 1 hour before and 1 hour after a non-cross section/point sample timestamp)
#' @param methods_NX Vector of Sampling Method (P82398) values that define non-cross section/point samples of the box coefficient.
#' Default is:\cr\cr methods_NX = c(30,40,50,55,70,100,900,920,930,940,4033,4080)\cr\cr 
#' 30 (single vertical), 40 (multiple verticals), 50 (point sample), 55 (composite - multiple point samples), 
#' 60 (weighted bottle), 70 (grab sample - dip), 100 (Van Dorn), 900 (SS pumping), 
#' 920 (SS BSV DI att), 930 (SS partial depth), 940 (SS partial width), 4033 (suction lift peristaltic), 4080 (peristaltic pump).
#' @param methods_X Vector of Sampling Method (P82398) values that define cross section samples of the box coefficient.
#' Default is:\cr\cr methods_X = c(10,15,20)\cr\cr
#' 10 (EWI), 15 (multiple verticals non-isokinetic EWT), or 20 (EDI).
#' @param includeUV Logical. If \code{x} was returned from \code{get_UVflow}, 
#' run optional addition of available Approved Unit Value discharge to output table. Default is \code{FALSE}.
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
#' @seealso \code{\link[sedReview]{get_localNWIS}}, \code{\link[sedReview]{find_boxcoef}}, \code{\link[sedReview]{get_UVflow}}

summary_boxcoef <- function(x,
                            timediff = 1,
                            methods_NX = c(30,40,50,55,70,100,900,920,930,940,4033,4080),
                            methods_X = c(10,15,20),
                            includeUV = FALSE,
                            returnAllTables = FALSE){
  #Get list of all site numbers in x dataframe from get_localNWIS
  sites <- unique(x$SITE_NO)
  
  #Run find_boxcoef function for each site
  allBC <- lapply(sites, function(y){
    sedReview::find_boxcoef(x, site_no = y, timediff = timediff, methods_NX = methods_NX, methods_X = methods_X, includeUV = includeUV)
  })
  names(allBC) <- sites
  
  #Summary dataframe of all sites and WY
  summary <- unique(x[c("SITE_NO","STATION_NM","WY")])
  
  #Function to summarise number of pairs in each WY and lapply on all site dataframes in allBC list
  countFun <- function(z){
    z$WY <- sedReview::waterYear(z$SAMPLE_START_DT_nonXS, numeric = TRUE)
    pairCount <- dplyr::summarise(dplyr::group_by(z,SITE_NO,WY),
                                  numPairs = length(RESULT_VA_nonXS))
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
  
  #Count number of nonXS and XS samples in each WY
  #SSC samples
  SSC <- x[x$PARM_CD == "80154",c("UID","SITE_NO")]
  SSC <- unique(SSC)
  
  #Samples with point or non-cross-section method and corresponds to an SSC sample
  methodNX <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% methods_NX 
                & x$UID %in% SSC$UID, 
                c("UID","SITE_NO","WY")]
  methodNX <- unique(methodNX)
  summaryNX <- dplyr::summarise(dplyr::group_by(methodNX, SITE_NO, WY),
                                nonXS_samples = length(UID))
  
  #Samples with EWI,EWT,EDI method and corresponds to an SSC sample. Call it x-section sample
  methodX <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% methods_X & x$UID %in% SSC$UID, c("UID","SITE_NO","WY")]
  methodX <- unique(methodX)
  summaryX <- dplyr::summarise(dplyr::group_by(methodX,SITE_NO,WY),
                               XS_samples = length(UID))
  
  #Join number of nonXS and XS samples
  summary <- dplyr::left_join(summary, summaryNX, by = c('SITE_NO','WY'))
  summary <- dplyr::left_join(summary, summaryX, by = c('SITE_NO','WY'))
  summary[is.na(summary)] <- 0
  
  #Return all the box coefficient tables if TRUE
  if(returnAllTables == TRUE){
    summary <- modifyList(list(summary), allBC)
    names(summary) <- c("summary", names(allBC))
  }
  
  return(summary)
}

