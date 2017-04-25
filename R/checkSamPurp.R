checkSamPurp <- function(x, returnAll = FALSE){
  # sample purposes
  purp <- x[x$PARM_CD == "71999", ]
  purp <- unique(purp[c("RECORD_NO", "RESULT_VA")])
  
  # table of unique sample purposes
  sampPurp <- as.data.frame(table(purp$RESULT_VA))
  names(sampPurp) <- c("SamplePurpose", "count")
  sampPurp$SamplePurpose <- as.character(sampPurp$SamplePurpose)
  # most common samp purpose
  mainPurp <- sampPurp$SamplePurpose[sampPurp$count %in% max(sampPurp$count)]
  
  # flag sample purposes other than most common
  purp$sampPurpFlag[purp$RESULT_VA != mainPurp] <- paste("flag uncommon sample purpose")
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, purp[c("RECORD_NO", "sampPurpFlag")], by = "RECORD_NO")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$sampPurpFlag)==FALSE, ]
  }

  
  return(flaggedSamples)
}
