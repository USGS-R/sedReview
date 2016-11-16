# x is plotData from NWISodbc data pull
sedOutliers <- function(x, lowThreshold, highThreshold){
  # extract SSC (80154) SSL (80155) and LOI (00496/00535)
  sedMedium <- c("WS ", "SS ", "SB ", "WSQ", "SSQ", "SBQ")
  x$flag <- NA
  
  SSC <- subset(x, PARM_CD == "80154" & MEDIUM_CD %in% sedMedium)
  SSC <- unique(SSC[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  
  SSL <- x[x$PARM_CD == "80155", ]
  SSL <- unique(SSL[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  
  bedLOI <- x[x$PARM_CD == "00496", ]
  bedLOI <- unique(bedLOI[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  
  watLOI <- x[x$PARM_CD == "00535", ]
  watLOI <- unique(watLOI[c("RECORD_NO", "PARM_CD", "RESULT_VA", "flag")])
  
  # closure function for outliers
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
  
  
  
  
  return(quantiles)
}

