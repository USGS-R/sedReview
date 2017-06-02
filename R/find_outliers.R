#' find_outliers
#' @description Function to flag sediment outliers
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @param lowThreshold numeric value between 0 and 1 indicating the quantile threshold for a low value outlier.
#' @param highThreshold numeric value between 0 and 1 indicating the quantile threshold for a high value outlier.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' find_outliersOut <- find_outliers(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
find_outliers <- function(x, lowThreshold = 0.1, highThreshold = 0.9, returnAll = FALSE){
  # extract SSC (80154) SSL (80155) and LOI (00496/00535)
  ## medium codes containing sediment parameters
  sedMedium <- c("WS ", "SS ", "SB ", "WSQ", "SSQ", "SBQ")
  x$flag <- NA
  ## suspended sediment concentration
  SSC <- x[x$PARM_CD == "80154" & x$MEDIUM_CD %in% sedMedium,]
  SSC <- unique(SSC[c("UID", "PARM_CD", "RESULT_VA", "flag")])
  ## suspended sediment load
  SSL <- x[x$PARM_CD == "80155", ]
  SSL <- unique(SSL[c("UID", "PARM_CD", "RESULT_VA", "flag")])
  ## bed sediment loss on ignition
  bedLOI <- x[x$PARM_CD == "00496", ]
  bedLOI <- unique(bedLOI[c("UID", "PARM_CD", "RESULT_VA", "flag")])
  ## suspended sediment loss on ignition
  watLOI <- x[x$PARM_CD == "00535", ]
  watLOI <- unique(watLOI[c("UID", "PARM_CD", "RESULT_VA", "flag")])
  
  ## Sand/Silt break (P70331 Suspended sediment, sieve diamter, percent smaller than 0.0625mm)
  ssbreak <- x[x$PARM_CD == "70331", ]
  ssbreak <- unique(ssbreak[c("UID", "PARM_CD", "RESULT_VA", "flag")])
  
  ## closure function for determining outliers from thresholds
  f <- function(d)
  {
    if(nrow(d)>0)
    {
      percentiles <- stats::quantile(d$RESULT_VA, probs = c(lowThreshold,highThreshold), na.rm = TRUE)
      outliers <- d[d$RESULT_VA < percentiles[1] |
                        d$RESULT_VA > percentiles[2],]
      outliers$flag[outliers$RESULT_VA < percentiles[1]] <- paste("flag low")
      outliers$flag[outliers$RESULT_VA > percentiles[2]] <- paste("flag high")
      d$flag <- NULL
      d <- dplyr::left_join(d, outliers[c("UID", "flag")], by = "UID")
    } 
    else{}
    return(d)
  }
  # apply closure function to datasets
  SSC <- f(SSC)
  SSL <- f(SSL)
  bedLOI <- f(bedLOI)
  watLOI <- f(watLOI)
  ssbreak <- f(ssbreak)
  
  # rename flag column
  names(SSC)[names(SSC) == "flag"] <- "SSC_flag"
  names(SSL)[names(SSL) == "flag"] <- "SSL_flag"
  names(bedLOI)[names(bedLOI) == "flag"] <- "bedSedLOI_flag"
  names(watLOI)[names(watLOI) == "flag"] <- "susSedLOI_flag"
  names(ssbreak)[names(ssbreak) == "flag"] <- "SandSiltBreak_flag"
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, SSC[c("UID", "SSC_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, SSL[c("UID", "SSL_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, bedLOI[c("UID", "bedSedLOI_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, watLOI[c("UID", "susSedLOI_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, ssbreak[c("UID", "SandSiltBreak_flag")], by = "UID")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$SSC_flag)==FALSE | 
                                       is.na(flaggedSamples$SSL_flag)==FALSE |
                                       is.na(flaggedSamples$bedSedLOI_flag)==FALSE |
                                       is.na(flaggedSamples$susSedLOI_flag)==FALSE |
                                       is.na(flaggedSamples$SandSiltBreak_flag)==FALSE, ]
  }
  
  return(flaggedSamples)
}

