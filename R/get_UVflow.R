#' get_UVflow. Joins UV flow data from NWISweb to SED data from local NWIS database.
#' @description Joins UV flow data from NWISweb to SED data from local NWIS database. Note this function can take several minutes to run.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param max.diff the maximum allowable difference in time for a match. See \bold{Details}.
#' Default is "1 hour" (ie. look for a paired UV 1 hour before and 1 hour after a SED sample timestamp)
#' @details This function utilizes the \code{dataRetrieval::readSWISuv} to pull Approved unit value discharge values from NWISweb
#' and join them with SED records from the local NWIS data pull based on sampling time and unit value time.
#' @details Options utlizing the UV flow are contained in the \code{check_Q} and \code{find_boxcoef} functions.
#' @details NOTE: the \code{dataRetrieval::readWISuv} utilizes all sites and the maximum date range in 
#' dataframe \code{x} from \code{get_localNWIS}.
#' Large numbers of sites or long date ranges can take several minutes to complete (ex. 4 sites and 2 WY completed in 2.5 minutes)
#' @details The format for \code{max.diff} should be a numeric value followed by a description of the time span.
#' The time span must be one of "secs", "mins", "hours", "days", or "weeks" for seconds, minutes, hours, days, or weeks, respectively.
#' @details The merge of data by time borrows code from \code{smwr::Base}, but the package is not a dependency.
#' @examples 
#' \dontrun{
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' mergeUV <- get_UVflow(x, max.diff = "1 hours")
#' }
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dataRetrieval readNWISuv
#' @importFrom lubridate force_tzs
#' @export
#' @return A dataframe containing all data originally in dataframe \code{x},
#' with available Approved Unit Value discharge appended from NWISweb.
#' @seealso \code{\link[sedReview]{get_localNWIS}}, \code{\link[dataRetrieval]{readNWISuv}}

get_UVflow <- function(x, max.diff = "1 hours"){
  
  # get site numbers and date range from data set
  siteNumbers <- unique(x$SITE_NO)
  minDate <- as.Date(min(x$SAMPLE_START_DT))
  maxDate <- as.Date(max(x$SAMPLE_START_DT))
  
  # run UV dataRetrieval and filter to only Approved UVs
  print('Pulling data from NWISweb')
  UVflow <- dataRetrieval::readNWISuv(siteNumbers = siteNumbers, 
                                      parameterCd = '00060', 
                                      startDate = minDate, endDate = maxDate)
  if(nrow(UVflow)==0){stop("No UV flow values returned")}
  UVflow <- UVflow[substr(UVflow$X_00060_00000_cd,1,1) == 'A', c('site_no','dateTime','X_00060_00000')]
  
  # function to mergeNearest on every site
  print('Merging UV and SED data')
  
  mergeUVflow <- function(siteNumber, qw.data, flow, max.diff){
    qw.data <- qw.data[qw.data$SITE_NO == siteNumber,]
    flow <- flow[flow$site_no == siteNumber,]
    qw.data <- merge_Nearest(left = qw.data, dates.left = 'SAMPLE_START_DT_UTC', all.left = FALSE,
                             right = flow, dates.right = 'dateTime', Date.noon = FALSE,
                             max.diff = max.diff)
    qw.data <- unique(qw.data[,c('UID','X_00060_00000')])
    names(qw.data) <- c('UID', "UV_flow_cfs")
    return(qw.data)
  }
  
  output <- lapply(siteNumbers, mergeUVflow, qw.data = x, flow = UVflow, max.diff = max.diff)
  output2 <- dplyr::bind_rows(output)
  x <- dplyr::left_join(x, output2, by = c('UID'='UID'))
  
  return(x)
}


