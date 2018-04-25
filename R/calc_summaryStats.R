#' calc_summaryStats. Calculate summary stats and tally non-detections and NA result values.
#' @description Calculates summary statistics, and tallys non-detects and NA result values, grouped by site, parameter, and water year.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param pcodes A character vector of parameter codes of interest. Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
#' @details Calculates number of samples, minimum, maximum, median, mean, and standard deviation (if applicable). Non-detects (REMARK_CD = "<") and Averages (REMARK_CD = "A") are not included in calculations.
#' @details A count of the number of non-detect samples and samples where no result value (RESULT_VA = \code{NA}) is included in the summary. 
#' Summary value "n" does not include non-detects or NA result samples.
#' @details Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
#' @details Rejected samples are not included.
#' @examples
#' data('exampleData', package = "sedReview")
#' x <- exampleData
#' summaryStats <- calc_summaryStats(x)
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @export
#' @return A dataframe of summary statistics

calc_summaryStats <- function(x, pcodes = c("80154",
                                            "70331",
                                            "00530",
                                            "80155",
                                            "80225",
                                            "91145"))
{
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
  #limit data to pcodes of interest, no blanks, and no non-detect or averages, and no samples where no result value reported
  statSamples <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & !(x$REMARK_CD %in% c("<","A")) & !(is.na(x$RESULT_VA)), ]
  
  #group by site, by pcode, by WY and calc stats
  temp <- dplyr::summarise(dplyr::group_by(statSamples,SITE_NO,STATION_NM,PARM_CD,PARM_NM,WY),
                           n = length(RESULT_VA),
                           min = min(RESULT_VA, na.rm = T),
                           max = max(RESULT_VA, na.rm = T),
                           median = median(RESULT_VA, na.rm = T),
                           mean = mean(RESULT_VA, na.rm = T),
                           stdev = sd(RESULT_VA, na.rm = T))
  temp$mean <- format(temp$mean, scientific = FALSE)
  
  #count number of non-detect samples and join to output
  nonDet <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & x$REMARK_CD == "<", ]
  nonDet <- dplyr::summarise(dplyr::group_by(nonDet,SITE_NO,PARM_CD,WY),
                             non_detects = length(REMARK_CD))
  temp <- dplyr::left_join(temp,nonDet, by = c("SITE_NO"="SITE_NO","PARM_CD"="PARM_CD","WY"="WY"))
  temp$non_detects[is.na(temp$non_detects)] <- 0
  
  #count number of NA result samples and join to output
  naRes <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & is.na(x$RESULT_VA), ]
  naRes <- dplyr::summarise(dplyr::group_by(naRes,SITE_NO,PARM_CD,WY),
                             NA_result_va = length(RESULT_VA))
  temp <- dplyr::left_join(temp,naRes, by = c("SITE_NO"="SITE_NO","PARM_CD"="PARM_CD","WY"="WY"))
  temp$NA_result_va[is.na(temp$NA_result_va)] <- 0
  
  return(temp)
  
}



