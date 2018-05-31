
x <- siteData

# get site numbers and date range from data set
siteNumbers <- unique(x$SITE_NO)
minDate <- as.Date(min(x$SAMPLE_START_DT))
maxDate <- as.Date(max(x$SAMPLE_START_DT))

# run UV dataRetrieval and filter to only Approved UVs
UVflow <- dataRetrieval::readNWISuv(siteNumbers = siteNumbers, parameterCd = '00060', startDate = minDate, endDate = maxDate)
if(nrow(UVflow)==0){stop("No UV flow values returned")}
UVflow <- UVflow[substr(UVflow$X_00060_00000_cd,1,1) == 'A', c('site_no','dateTime','X_00060_00000')]

# convert QW data times to UTC for comparison to UV retrieval
x$TIME_UTC <- lubridate::force_tzs(x$SAMPLE_START_DT, tzones = x$SAMPLE_START_TZ_CD, tzone_out = 'UTC')

# function to mergeNearest on every site
max.diff <- "1 hours"
mergeUVflow <- function(siteNumber, qw.data, flow, max.diff){
  qw.data <- qw.data[qw.data$SITE_NO == siteNumber,]
  flow <- flow[flow$site_no == siteNumber,]
  qw.data <- merge_Nearest(left = qw.data, dates.left = 'TIME_UTC', all.left = FALSE,
                       right = flow, dates.right = 'dateTime', Date.noon = FALSE,
                       max.diff = max.diff)
  qw.data <- unique(qw.data[,c('UID','X_00060_00000')])
  names(qw.data) <- c('UID', "UV_flow_cfs")
  return(qw.data)
}

output <- lapply(siteNumbers, mergeUVflow, qw.data = x, flow = UVflow, max.diff = max.diff)
output2 <- dplyr::rbind_list(output)
x <- dplyr::left_join(x, output2, by = c('UID'='UID'))


