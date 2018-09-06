#' calc_concSandFine. calculate sand/fine concentration based on SSC and sand/fine split value
#' 
#' @description Function to calculate sand/fine concentration based on SSC and sand/fine split value
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @details Sand and Fine concentration (mg/L) is calculated for sites with SSC (P80154) and % finer than 62.5 microns (P70331)
#' @details Rejected samples are not included.
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' concSandFine <- calc_concSandFine(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with complete SSC, sand/fine split, and calculated sand and fine concentrations

calc_concSandFine <- function(x)
{
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
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
  
  # # plot timeseries of concentrations
  # if(plotTime == TRUE)
  # {
  #   # get list of sites
  #   sites <- unique(SSC$SITE_NO)
  #   # loop through and output plots
  #   for( i in 1:length(sites))
  #   {
  #     temp <- SSC[SSC$SITE_NO == sites[i], ]
  #     plot(x = temp$SAMPLE_START_DT, y = temp$sandConc, xlab = "Date", ylab = "Sand Conc mg/L", 
  #          main = paste(unique(temp$SITE_NO), unique(temp$STATION_NM)))
  #     plot(x = temp$SAMPLE_START_DT, y = temp$fineConc, xlab = "Date", ylab = "Fine Conc mg/L", 
  #          main = paste(unique(temp$SITE_NO), unique(temp$STATION_NM)))
  #   }
  # }
  # # plot concentration v. flow 
  # if(plotFlow == TRUE)
  # {
  #   # remove records that don't have flow data
  #   temp1 <- SSC[is.na(SSC$cfs) == FALSE, ]
  #   # get list of sites
  #   sites <- unique(temp1$SITE_NO)
  #   if(length(sites) > 0)
  #   {
  #     for( i in 1:length(sites))
  #     {
  #       temp <- temp1[temp1$SITE_NO == sites[i], ]
  #       plot(x = temp$cfs, y = temp$sandConc, xlab = "Discharge cfs", ylab = "Sand Conc mg/L", 
  #            main = paste(unique(temp$SITE_NO), unique(temp$STATION_NM)))
  #       plot(x = temp$cfs, y = temp$fineConc, xlab = "Discharge cfs", ylab = "Fine Conc mg/L", 
  #            main = paste(unique(temp$SITE_NO), unique(temp$STATION_NM)))
  #     }
  #   } else(print("No sites with flow data to plot"))
  # 
  # }
  
  return(SSC)
}



