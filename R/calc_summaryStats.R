#' calc_summaryStats. Calculate summary stats and tally non-detections and samples with no result values reported.
#' @description Calculates summary statistics, and tallys non-detects and NA result values, grouped by site, parameter, and water year.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param pcodes A character vector of parameter codes of interest. 
#' Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
#' @details Calculates number of samples, minimum, maximum, median, mean, and standard deviation (if applicable). 
#' Non-detects (REMARK_CD = "<") and Averages (REMARK_CD = "A") are not included in calculations. Rejected samples are not included.
#' @details A count of the number of non-detect samples and samples where no result value was reported (RESULT_VA = \code{NA})
#'  is included in the summary. Summary value "n" does not include non-detects or no result reported samples.
#' @details Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), 
#' Bedload (80225), Bedload mass (91145)
#' @details Statistics are separated by site, parameter, year (WY or user defined), and sample method group.
#' @details Sample method groups. Cross section (X-section) is defined as sample method 10 (EWI), 15 (EWT), or 20 (EDI).
#' Point or non-cross section (Pt/non-X-section) is defined as sample method 30 (single vertical), 40 (multiple verticals), 
#' 50 (point sample), 55 (composite - multiple point samples), 60 (weighted bottle), 70 (grab sample - dip), 100 (Van Dorn), 
#' 900 (SS pumping), 920 (SS BSV DI att), 930 (SS partial depth), 940 (SS partial width), 
#' 4033 (suction lift peristaltic), 4080 (peristaltic pump). Samples with missing or other coded method are grouped together.
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
  
  # identify xsection and non-xsection samples
  xsection <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(10,15,20),]
  nonxsection <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(30,40,50,55,60,70,100,900,920,930,940,4033,4080),]
  
  x$xsection[x$UID %in% xsection$UID] <- "X-Section"
  x$xsection[x$UID %in% nonxsection$UID] <- "Pt/non-X-section"
  x$xsection[is.na(x$xsection)] <- "not coded or other"
  
  #limit data to pcodes of interest, no blanks, and no non-detect or averages, and no samples where no result value reported
  statSamples <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & !(x$REMARK_CD %in% c("<","A")) & !(is.na(x$RESULT_VA)), ]
  
  #group by site, by pcode, by WY and calc stats
  temp <- dplyr::summarise(dplyr::group_by(statSamples,SITE_NO,STATION_NM,PARM_CD,PARM_NM,WY,xsection),
                           n = length(RESULT_VA),
                           min = min(RESULT_VA, na.rm = T),
                           max = max(RESULT_VA, na.rm = T),
                           median = median(RESULT_VA, na.rm = T),
                           mean = mean(RESULT_VA, na.rm = T),
                           stdev = sd(RESULT_VA, na.rm = T))
  temp$mean <- format(temp$mean, scientific = FALSE)
  
  #count number of non-detect samples and join to output
  nonDet <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & x$REMARK_CD == "<", ]
  nonDet <- dplyr::summarise(dplyr::group_by(nonDet,SITE_NO,PARM_CD,WY,xsection),
                             non_detects = length(REMARK_CD))
  temp <- dplyr::left_join(temp,nonDet, by = c("SITE_NO"="SITE_NO","PARM_CD"="PARM_CD","WY"="WY","xsection"="xsection"))
  temp$non_detects[is.na(temp$non_detects)] <- 0
  
  #count number of NA result samples and join to output
  naRes <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes & is.na(x$RESULT_VA), ]
  naRes <- dplyr::summarise(dplyr::group_by(naRes,SITE_NO,PARM_CD,WY,xsection),
                             no_result_reported = length(RESULT_VA))
  temp <- dplyr::left_join(temp,naRes, by = c("SITE_NO"="SITE_NO","PARM_CD"="PARM_CD","WY"="WY","xsection"="xsection"))
  temp$no_result_reported[is.na(temp$no_result_reported)] <- 0
  
  return(temp)
  
}



