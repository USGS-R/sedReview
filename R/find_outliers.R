#' find_outliers. Flag sediment outliers based on threshold criteria.
#' @description Function to flag sediment outliers based on threshold criteria.
#' @description Flags for suspended sediment concentration (P80154), suspended sediment load (P80155), bed and suspended sediment loss on ignition (P00496/00535), and Sand/Silt break (P70331 Suspended sediment, sieve diamter, percent smaller than 0.0625mm)
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param site_no Character of a site number in the x \code{dataframe} from \code{get_localNWIS} if x contains more than one site. Default is NULL.
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @param lowThreshold numeric value between 0 and 1 indicating the quantile threshold for a low value outlier.
#' @param highThreshold numeric value between 0 and 1 indicating the quantile threshold for a high value outlier.
#' @details Rejected samples are not included.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' outliers_05586300 <- find_outliers(x, site_no = "05586300")
#' 
#' @importFrom dplyr left_join
#' @importFrom stats quantile
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
find_outliers <- function(x, site_no = NULL, lowThreshold = 0.1, highThreshold = 0.9, returnAll = FALSE){
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
  # check for site
  if(!(is.null(site_no))){
    site_no <- as.character(site_no)
    if((site_no %in% x$SITE_NO)==FALSE){
      stop("Site number not in input data.")
    }
    x <- x[x$SITE_NO == site_no,]
  }
  if(length(unique(x$SITE_NO))>1){
    stop("More than one site, please specify 'site_no'")}
  
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
      percentiles <- stats::quantile(d$RESULT_VA, probs = c(lowThreshold,highThreshold), na.rm = TRUE, names = FALSE)
      outliers <- d[which(d$RESULT_VA < percentiles[1] | d$RESULT_VA > percentiles[2]),]
      outliers$flag[outliers$RESULT_VA < percentiles[1]] <- paste("flag low, result_va = ", outliers$RESULT_VA[outliers$RESULT_VA < percentiles[1]])
      outliers$flag[outliers$RESULT_VA > percentiles[2]] <- paste("flag high, result_va = ", outliers$RESULT_VA[outliers$RESULT_VA > percentiles[2]])
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
  names(SSC)[names(SSC) == 'RESULT_VA'] <- "SSC_RESULT_VA"
  names(SSL)[names(SSL) == "flag"] <- "SSL_flag"
  names(SSL)[names(SSL) == 'RESULT_VA'] <- "SSL_RESULT_VA"
  names(bedLOI)[names(bedLOI) == "flag"] <- "bedSedLOI_flag"
  names(bedLOI)[names(bedLOI) == 'RESULT_VA'] <- "bedSedLOI_RESULT_VA"
  names(watLOI)[names(watLOI) == "flag"] <- "susSedLOI_flag"
  names(watLOI)[names(watLOI) == 'RESULT_VA'] <- "susSedLOI_RESULT_VA"
  names(ssbreak)[names(ssbreak) == "flag"] <- "SandSiltBreak_flag"
  names(ssbreak)[names(ssbreak) == 'RESULT_VA'] <- "SandSiltBreak_RESULT_VA"
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, SSC[c("UID","SSC_RESULT_VA","SSC_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, SSL[c("UID","SSL_RESULT_VA","SSL_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, bedLOI[c("UID","bedSedLOI_RESULT_VA","bedSedLOI_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, watLOI[c("UID","susSedLOI_RESULT_VA","susSedLOI_flag")], by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, ssbreak[c("UID","SandSiltBreak_RESULT_VA","SandSiltBreak_flag")], by = "UID")
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

