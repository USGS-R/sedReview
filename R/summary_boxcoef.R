data('exampleData2', package = "sedReview")
boxCoef <- summary_boxcoef(x)
boxCoef_list <- summary_boxcoef(x, returnAllTables = TRUE)
View(boxCoef_list$summary)

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

