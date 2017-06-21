
plot_concSandFine <- function(x, plotTime = FALSE, plotFlow = FALSE)
{
  # get suspended sediment concentration
  SSC <- x[x$PARM_CD == "80154", ]
  SSC <- unique(SSC[c("UID",
                      "RECORD_NO",
                      "SITE_NO",
                      "STATION_NM",
                      "SAMPLE_START_DT",
                      "RESULT_VA")])
  
  # get sand/fines split (% finer than 62.5 microns)
  split <- x[x$PARM_CD == "70331", ]
  split <- unique(split[c("UID", "RESULT_VA")])
  
  # get flow data
  qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208", "30209", "50042", "72137", "72243", "99060", "99061"), ]
  # convert all flow data to common units
  qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] * 35.3147 #cms to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "30209"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "30209"] * 35.3147 #cms to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "50042"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "50042"] * 0.133681 / 60 #gal/min to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "72243"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "72243"] / 24 / 60 / 60 #cfd to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "99060"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "99060"] * 35.3147 #cms to cfs
  qRecords$RESULT_VA[qRecords$PARM_CD == "99061"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "99061"] * 35.3147 #cms to cfs
  qRecords <- unique(qRecords[c("UID", "RESULT_VA")])
  
  # limit to only samples with both SSC and sand/fines split
  SSC <- SSC[SSC$UID %in% split$UID, ]
  SSC <- dplyr::left_join(SSC, split, by = "UID")
  SSC <- dplyr::left_join(SSC, qRecords, by = "UID")
  names(SSC) <- c("UID",
                  "RECORD_NO",
                  "SITE_NO",
                  "STATION_NM",
                  "SAMPLE_START_DT",
                  "SSC", "split", "cfs")
  # calculate sand and fines concentrations
  SSC$sandConc <- SSC$SSC * ((100 - SSC$split) / 100)
  SSC$fineConc <- SSC$SSC * (SSC$split / 100)
  
  return(SSC)
}



