# x is plotData from NWISodbc data pull
sedOutliers <- function(x){
  # extract SSC (80154) SSL (80155) and LOI (00496/00535)
  SSC <- x[x$PARM_CD == 80154, ]
  SSL <- x[x$PARM_CD == 80155, ]
  bedLOI <- x[x$PARM_CD == 00496, ]
  watLOI <- x[x$PARM_CD == 00535, ]
  
  # closure function for printing plot and percentiles
  f <- function(d) {
    if(nrow(d) > 0){
      title <- paste(unique(d$PARM_CD), unique(d$PARM_NM), sep = " ")
      bp <- boxplot(d$RESULT_VA)
      title(title)
      print(title)
      quantile(d$RESULT_VA, probs = seq(0, 1, 0.1), na.rm = TRUE)
    } else{
      print(paste("No Samples"))
    }
  }
}

