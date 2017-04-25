#' getSedOUtliers
#' @description Function to flag sediment outliers
#' @param qw.data A qw.data list generated from readNWISodbc
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details 
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData$PlotTable
#' sedOutlierFlags <- getSedOutliers(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
getSedOutliers <- function(x, lowThreshold = 0.1, highThreshold = 0.9, returnAll = FALSE){
  # extract SSC (80154) SSL (80155) and LOI (00496/00535)
  ## medium codes containing sediment parameters
  sedMedium <- c("WS ", "SS ", "SB ", "WSQ", "SSQ", "SBQ")
  x$flag <- NA
  ## suspended sediment concentration
  SSC <- x[x$PARM_CD == "80154" & x$MEDIUM_CD %in% sedMedium,]
  SSC <- unique(SSC[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  ## suspended sediment load
  SSL <- x[x$PARM_CD == "80155", ]
  SSL <- unique(SSL[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  ## bed sample loss on ignition
  bedLOI <- x[x$PARM_CD == "00496", ]
  bedLOI <- unique(bedLOI[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  ## water sample loss on ignition
  watLOI <- x[x$PARM_CD == "00535", ]
  watLOI <- unique(watLOI[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  
  ## closure function for determining outliers from thresholds
  f <- function(d)
  {
    if(nrow(d)>0)
    {
      percentiles <- quantile(d$RESULT_VA, probs = c(lowThreshold,highThreshold))
      outliers <- d[d$RESULT_VA < percentiles[1] |
                        d$RESULT_VA > percentiles[2],]
      outliers$flag[outliers$RESULT_VA < percentiles[1]] <- paste("flag low")
      outliers$flag[outliers$RESULT_VA > percentiles[2]] <- paste("flag high")
      d$flag <- NULL
      d <- left_join(d, outliers[c("RECORD_NO", "flag")], by = "RECORD_NO")
    } 
    else{}
    return(d)
  }
  # apply closure function to datasets
  SSC <- f(SSC)
  SSL <- f(SSL)
  bedLOI <- f(bedLOI)
  watLOI <- f(watLOI)
  
  # rename flag column
  names(SSC)[names(SSC) == "flag"] <- "SSC_flag"
  names(SSL)[names(SSL) == "flag"] <- "SSL_flag"
  names(bedLOI)[names(bedLOI) == "flag"] <- "bedLOI_flag"
  names(watLOI)[names(watLOI) == "flag"] <- "watLOI_flag"
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, SSC[c("RECORD_NO", "SSC_flag")], by = "RECORD_NO")
  flaggedSamples <- dplyr::left_join(flaggedSamples, SSL[c("RECORD_NO", "SSL_flag")], by = "RECORD_NO")
  flaggedSamples <- dplyr::left_join(flaggedSamples, bedLOI[c("RECORD_NO", "bedLOI_flag")], by = "RECORD_NO")
  flaggedSamples <- dplyr::left_join(flaggedSamples, watLOI[c("RECORD_NO", "watLOI_flag")], by = "RECORD_NO")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$SSC_flag)==FALSE | 
                                       is.na(flaggedSamples$SSL_flag)==FALSE |
                                       is.na(flaggedSamples$bedLOI_flag)==FALSE |
                                       is.na(flaggedSamples$watLOI_flag)==FALSE, ]
  }
  
  return(flaggedSamples)
}

