# function to test EDI sample collected correctly. Number of sampling verticals 4-9.
# function to test EWI sample collected correctly. Number of verticals between 10 and 20

sampleVertCheck <- function(x, returnAll = FALSE){
  
  # Select all EDI and EWI samples
  EDI <- x[x$PARM_CD == "82398", ]
  EWI <- EDI[EDI$RESULT_VA == "10", ]
  EWI <- unique(EWI[c("RECORD_NO")])
  EDI <- EDI[EDI$RESULT_VA == "20", ]
  EDI <- unique(EDI[c("RECORD_NO")])
  # select number of verticals parameter (sampling points)
  verts <- x[x$PARM_CD == "00063", ]
  verts <- verts[c("RECORD_NO", "RESULT_VA")]
  verts <- unique(verts[c("RECORD_NO", "RESULT_VA")])
  #join number of verticals to EWI/EDI dataframes
  EWI <- left_join(EWI, verts, by = "RECORD_NO")
  EDI <- left_join(EDI, verts, by = "RECORD_NO")
  
  # set flags
  EWI$EWIvertflag[is.na(EWI$RESULT_VA)==TRUE] <- paste("flag # EWI verticals missing")
  EWI$EWIvertflag[EWI$RESULT_VA > 20 & is.na(EWI$RESULT_VA)==FALSE] <- paste("flag high # EWI verticals=", 
                                                                          EWI$RESULT_VA[EWI$RESULT_VA > 20 & is.na(EWI$RESULT_VA)==FALSE])
  EWI$EWIvertflag[EWI$RESULT_VA < 10 & is.na(EWI$RESULT_VA)==FALSE] <- paste("flag low # EWI verticals=",
                                                                          EWI$RESULT_VA[EWI$RESULT_VA < 10 & is.na(EWI$RESULT_VA)==FALSE])
  
  EDI$EDIvertflag[is.na(EDI$RESULT_VA)==TRUE] <- paste("flag # EDI verticals missing")
  EDI$EDIvertflag[EDI$RESULT_VA > 9 & is.na(EDI$RESULT_VA)==FALSE] <- paste("flag high # EDI verticals=", 
                                                                          EDI$RESULT_VA[EDI$RESULT_VA > 9 & is.na(EDI$RESULT_VA)==FALSE])
  EDI$EDIvertflag[EDI$RESULT_VA < 4 & is.na(EDI$RESULT_VA)==FALSE] <- paste("flag low # EDI verticals=",
                                                                          EDI$RESULT_VA[EDI$RESULT_VA < 4 & is.na(EDI$RESULT_VA)==FALSE])
  
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  flaggedSamples <- dplyr::left_join(flaggedSamples, EWI[c("RECORD_NO", "EWIvertflag")], by = "RECORD_NO")
  flaggedSamples <- dplyr::left_join(flaggedSamples, EDI[c("RECORD_NO", "EDIvertflag")], by = "RECORD_NO")
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$EWIvertflag)==FALSE | 
                                       is.na(flaggedSamples$EDIvertflag)==FALSE, ]
  }
  
  return(flaggedSamples)
}