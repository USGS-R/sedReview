#' calc_summaryStats. Calculate summary stats and non-detections.
#' @description Calculates summary statistics and tallys non-detects grouped by site, parameter, and water year.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param pcodes A character vector of parameter codes of interest. Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
#' @details Calculates minimum, maximum, median, mean, and standard deviation (if applicable). Non-detects (REMARK_CD = "<") and Averages (REMARK_CD = "A") are not included in calculations.
#' @details A count of the number of non-detect samples is included in the summary.
#' @details Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
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
  #limit data to pcodes of interest, no blanks, and no non-detect or averages
  statSamples <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & !(x$REMARK_CD %in% c("<","A")), ]
  
  #group by site, by pcode, by WY and calc stats
  temp <- dplyr::summarise(dplyr::group_by(statSamples,SITE_NO,STATION_NM,PARM_CD,PARM_NM,WY),
                           min = min(RESULT_VA),
                           max = max(RESULT_VA),
                           median = median(RESULT_VA),
                           mean = mean(RESULT_VA),
                           stdev = sd(RESULT_VA, na.rm = T))
  temp$mean <- format(temp$mean, scientific = FALSE)
  
  #count number of non-detect samples and join to output
  nonDet <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & x$REMARK_CD == "<", ]
  nonDet <- dplyr::summarise(dplyr::group_by(nonDet,SITE_NO,PARM_CD,WY),
                             non_detects = length(REMARK_CD))
  temp <- dplyr::left_join(temp,nonDet, by = c("SITE_NO"="SITE_NO","PARM_CD"="PARM_CD","WY"="WY"))
  temp$non_detects[is.na(temp$non_detects)] <- 0
  
  return(temp)
  
}



