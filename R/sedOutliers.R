# x is plotData from NWISodbc data pull
sedOutliers <- function(x){
  # extract SSC (80154) SSL (80155) and LOI (00496/00535)
  SSC <- x[x$PARM_CD == 80154, ]
  SSL <- x[x$PARM_CD == 80155, ]
  bedLOI <- x[x$PARM_CD == 00496, ]
  watLOI <- x[x$PARM_CD == 00535, ]
  
  # quantiles
  quantiles <- data.frame(quantile = seq(0, 1, 0.1))
  quantiles$SSC <- quantile(SSC$RESULT_VA, probs = seq(0, 1, 0.1))
  quantiles$SSL <- quantile(SSL$RESULT_VA, probs = seq(0, 1, 0.1))
  quantiles$bedLOI <- quantile(bedLOI$RESULT_VA, probs = seq(0, 1, 0.1))
  quantiles$watLOI <- quantile(watLOI$RESULT_VA, probs = seq(0, 1, 0.1))
  
  
  # closure function for printing plot
  f <- function(d) {
    if(nrow(d) > 0){
      title <- paste(unique(d$PARM_CD), unique(d$PARM_NM), sep = " ")
      boxplot(d$RESULT_VA)
      title(title)
    } else{}
  }
  lapply(list(SSC, SSL, bedLOI, watLOI), f)
  return(quantiles)
}

