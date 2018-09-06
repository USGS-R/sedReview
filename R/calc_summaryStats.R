#' calc_summaryStats. Calculate summary stats and tally non-detections and samples with no result values reported.
#' @description Calculates summary statistics, and tallys non-detects and NA result values, grouped by site, parameter, 
#' and water year (or user-defined calendar year period).
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param pcodes A character vector of parameter codes of interest. 
#' Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), 
#' Bedload (80225), Bedload mass (91145)
#' @param startMonth Numeric starting month (ex. 1 for January) for user-defined calendar year periods. Default is NULL.
#' @param endMonth Numeric ending month (ex. 12 for December) for user-defined calendar year periods. Default is NULL. 
#' @details Calculates number of samples, minimum, maximum, median, mean, and standard deviation (if applicable). 
#' Non-detects (REMARK_CD = "<") and Averages (REMARK_CD = "A") are not included in calculations. Rejected samples are not included.
#' @details A count of the number of non-detect samples and samples where no result value was reported (RESULT_VA = \code{NA})
#'  is included in the summary. Summary value "n" does not include non-detects or no result reported samples.
#' @details Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), 
#' Bedload (80225), Bedload mass (91145)
#' @details Statistics are separated by site, parameter, year (WY or user defined CY), and sample method group.
#' @details User-defined calendar year. If \code{startMonth} and \code{endMonth} are specified, statistics and counts 
#' will be limited to calendar year samples within the months specified. Ex. If \code{startMonth = 9} and \code{endMonth = 11},
#' only samples from September to November will be included in summary stats and counts, and they will be grouped by calendar year.
#' @details Sample method groups. Cross section (X-section) is defined as sample method 10 (EWI), 15 (EWT), or 
#' 20 (EDI), 40 (multiple verticals).
#' Point or non-cross section (Pt/non-X-section) is defined as sample method 30 (single vertical), 
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
                                            "91145"),
                              startMonth = NULL, endMonth  = NULL)
{
  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
  # identify xsection and non-xsection samples
  xsection <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(10,15,20,40),]
  nonxsection <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% c(30,50,55,60,70,100,900,920,930,940,4033,4080),]
  
  x$xsection[x$UID %in% xsection$UID] <- "X-Section"
  x$xsection[x$UID %in% nonxsection$UID] <- "Pt/non-X-section"
  x$xsection[is.na(x$xsection)] <- "not coded or other"
  
  #split up by user defined calendar year period, calendar year column heading renamed below
  if(!is.null(startMonth) & !is.null(endMonth)){
    x$month <- lubridate::month(x$SAMPLE_START_DT)
    if(startMonth <= endMonth){
      x <- x[x$month >= startMonth & x$month <= endMonth,]
      x$WY[x$month >= 10] <- x$WY[x$month >=10] - 1
    }
    if(startMonth > endMonth){
      stop("Start Month must be less than End Month")
    }
  }
  if(!is.null(startMonth) & is.null(endMonth)){warning("endMonth not set, reverting to stats by WY")}
  if(is.null(startMonth) & !is.null(endMonth)){warning("startMonth not set, reverting to stats by WY")}

  
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
  #temp$mean <- format(temp$mean, scientific = FALSE)
  
  # default rounding array for P80154: 0011233331
  temp$mean[temp$PARM_CD == '80154' & temp$mean < 0.1] <- round(signif(temp$mean[temp$PARM_CD == '80154' & temp$mean < 0.1], digits = 0), digits = 0)
  temp$mean[temp$PARM_CD == '80154' & temp$mean >= 0.1 & temp$mean < 10] <- round(signif(
    temp$mean[temp$PARM_CD == '80154' & temp$mean >= 0.1 & temp$mean < 10], 1), digits = 0)
  temp$mean[temp$PARM_CD == '80154' & temp$mean >= 10 & temp$mean < 100] <- round(signif(
    temp$mean[temp$PARM_CD == '80154' & temp$mean >= 10 & temp$mean < 100], 2), digits = 0)
  temp$mean[temp$PARM_CD == '80154' & temp$mean >= 100] <- round(signif(
    temp$mean[temp$PARM_CD == '80154' & temp$mean >= 100], 3), digits = 0)
  
  temp$stdev[temp$PARM_CD == '80154' & temp$stdev < 0.1 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80154' & temp$stdev < 0.1 & !is.na(temp$stdev)], digits = 0), digits = 0)
  temp$stdev[temp$PARM_CD == '80154' & temp$stdev >= 0.1 & temp$stdev < 10 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80154' & temp$stdev >= 0.1 & temp$stdev < 10 & !is.na(temp$stdev)], 1), digits = 0)
  temp$stdev[temp$PARM_CD == '80154' & temp$stdev >= 10 & temp$stdev < 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80154' & temp$stdev >= 10 & temp$stdev < 100 & !is.na(temp$stdev)], 2), digits = 0)
  temp$stdev[temp$PARM_CD == '80154' & temp$stdev >= 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80154' & temp$stdev >= 100 & !is.na(temp$stdev)], 3), digits = 0)
  
  # default rounding array for P70331: 0012333331
  temp$mean[temp$PARM_CD == '70331' & temp$mean < 0.1] <- round(signif(temp$mean[temp$PARM_CD == '70331' & temp$mean < 0.1], digits = 0), digits = 0)
  temp$mean[temp$PARM_CD == '70331' & temp$mean >= 0.1 & temp$mean < 1] <- round(signif(
    temp$mean[temp$PARM_CD == '70331' & temp$mean >= 0.1 & temp$mean < 1], 1), digits = 1)
  temp$mean[temp$PARM_CD == '70331' & temp$mean >= 1 & temp$mean < 10] <- round(signif(
    temp$mean[temp$PARM_CD == '70331' & temp$mean >= 1 & temp$mean < 10], 2), digits = 1)
  temp$mean[temp$PARM_CD == '70331' & temp$mean >= 10] <- round(signif(
    temp$mean[temp$PARM_CD == '70331' & temp$mean >= 10], 3), digits = 1)
  
  temp$stdev[temp$PARM_CD == '70331' & temp$stdev < 0.1 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '70331' & temp$stdev < 0.1 & !is.na(temp$stdev)], digits = 0), digits = 0)
  temp$stdev[temp$PARM_CD == '70331' & temp$stdev >= 0.1 & temp$stdev < 1 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '70331' & temp$stdev >= 0.1 & temp$stdev < 1 & !is.na(temp$stdev)], 1), digits = 1)
  temp$stdev[temp$PARM_CD == '70331' & temp$stdev >= 1 & temp$stdev < 10 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '70331' & temp$stdev >= 1 & temp$stdev < 10 & !is.na(temp$stdev)], 2), digits = 1)
  temp$stdev[temp$PARM_CD == '70331' & temp$stdev >= 10 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '70331' & temp$stdev >= 10 & !is.na(temp$stdev)], 3), digits = 1)
  
  # default rounding array for P00530: 0011233331
  temp$mean[temp$PARM_CD == '00530' & temp$mean < 0.1] <- round(signif(temp$mean[temp$PARM_CD == '00530' & temp$mean < 0.1], digits = 0), digits = 0)
  temp$mean[temp$PARM_CD == '00530' & temp$mean >= 0.1 & temp$mean < 10] <- round(signif(
    temp$mean[temp$PARM_CD == '00530' & temp$mean >= 0.1 & temp$mean < 10], 1), digits = 0)
  temp$mean[temp$PARM_CD == '00530' & temp$mean >= 10 & temp$mean < 100] <- round(signif(
    temp$mean[temp$PARM_CD == '00530' & temp$mean >= 10 & temp$mean < 100], 2), digits = 0)
  temp$mean[temp$PARM_CD == '00530' & temp$mean >= 100] <- round(signif(
    temp$mean[temp$PARM_CD == '00530' & temp$mean >= 100], 3), digits = 0)
  
  temp$stdev[temp$PARM_CD == '00530' & temp$stdev < 0.1 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '00530' & temp$stdev < 0.1 & !is.na(temp$stdev)], digits = 0), digits = 0)
  temp$stdev[temp$PARM_CD == '00530' & temp$stdev >= 0.1 & temp$stdev < 10 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '00530' & temp$stdev >= 0.1 & temp$stdev < 10 & !is.na(temp$stdev)], 1), digits = 0)
  temp$stdev[temp$PARM_CD == '00530' & temp$stdev >= 10 & temp$stdev < 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '00530' & temp$stdev >= 10 & temp$stdev < 100 & !is.na(temp$stdev)], 2), digits = 0)
  temp$stdev[temp$PARM_CD == '00530' & temp$stdev >= 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '00530' & temp$stdev >= 100 & !is.na(temp$stdev)], 3), digits = 0)
  
  # default rounding array for P80155: 0222233332
  temp$mean[temp$PARM_CD == '80155' & temp$mean < 0.01] <- round(signif(temp$mean[temp$PARM_CD == '80155' & temp$mean < 0.01], digits = 0), digits = 0)
  temp$mean[temp$PARM_CD == '80155' & temp$mean >= 0.01 & temp$mean < 100] <- round(signif(
    temp$mean[temp$PARM_CD == '80155' & temp$mean >= 0.01 & temp$mean < 100], 2), digits = 2)
  temp$mean[temp$PARM_CD == '80155' & temp$mean >= 100] <- round(signif(
    temp$mean[temp$PARM_CD == '80155' & temp$mean >= 100], 3), digits = 0)
  
  temp$stdev[temp$PARM_CD == '80155' & temp$stdev < 0.01 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80155' & temp$stdev < 0.01 & !is.na(temp$stdev)], digits = 0), digits = 0)
  temp$stdev[temp$PARM_CD == '80155' & temp$stdev >= 0.1 & temp$stdev < 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80155' & temp$stdev >= 0.1 & temp$stdev < 100 & !is.na(temp$stdev)], 2), digits = 2)
  temp$stdev[temp$PARM_CD == '80155' & temp$stdev >= 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80155' & temp$stdev >= 100 & !is.na(temp$stdev)], 3), digits = 0)
  
  # default rounding array for P80225: 0222233442
  temp$mean[temp$PARM_CD == '80225' & temp$mean < 0.01] <- round(signif(temp$mean[temp$PARM_CD == '80225' & temp$mean < 0.01], digits = 0), digits = 0)
  temp$mean[temp$PARM_CD == '80225' & temp$mean >= 0.01 & temp$mean < 100] <- round(signif(
    temp$mean[temp$PARM_CD == '80225' & temp$mean >= 0.01 & temp$mean < 100], 2), digits = 2)
  temp$mean[temp$PARM_CD == '80225' & temp$mean >= 100 & temp$mean < 10000] <- round(signif(
    temp$mean[temp$PARM_CD == '80225' & temp$mean >= 100 & temp$mean < 10000], 3), digits = 0)
  temp$mean[temp$PARM_CD == '80225' & temp$mean >= 10000] <- round(signif(
    temp$mean[temp$PARM_CD == '80225' & temp$mean >= 10000], 4), digits = 0)
  
  temp$stdev[temp$PARM_CD == '80225' & temp$stdev < 0.01 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80225' & temp$stdev < 0.01 & !is.na(temp$stdev)], digits = 0), digits = 0)
  temp$stdev[temp$PARM_CD == '80225' & temp$stdev >= 0.01 & temp$stdev < 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80225' & temp$stdev >= 0.01 & temp$stdev < 100 & !is.na(temp$stdev)], 2), digits = 2)
  temp$stdev[temp$PARM_CD == '80225' & temp$stdev >= 100 & temp$stdev < 10000 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80225' & temp$stdev >= 100 & temp$stdev < 10000 & !is.na(temp$stdev)], 3), digits = 0)
  temp$stdev[temp$PARM_CD == '80225' & temp$stdev >= 10000 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '80225' & temp$stdev >= 10000 & !is.na(temp$stdev)], 4), digits = 0)
  
  # default rounding array for P91145: 0012345551
  temp$mean[temp$PARM_CD == '91145' & temp$mean < 0.1] <- round(signif(temp$mean[temp$PARM_CD == '91145' & temp$mean < 0.1], digits = 0), digits = 0)
  temp$mean[temp$PARM_CD == '91145' & temp$mean >= 0.1 & temp$mean < 1] <- round(signif(
    temp$mean[temp$PARM_CD == '91145' & temp$mean >= 0.1 & temp$mean < 1], 1), digits = 1)
  temp$mean[temp$PARM_CD == '91145' & temp$mean >= 1 & temp$mean < 10] <- round(signif(
    temp$mean[temp$PARM_CD == '91145' & temp$mean >= 1 & temp$mean < 10], 2), digits = 1)
  temp$mean[temp$PARM_CD == '91145' & temp$mean >= 10 & temp$mean < 100] <- round(signif(
    temp$mean[temp$PARM_CD == '91145' & temp$mean >= 10 & temp$mean < 100], 3), digits = 1)
  temp$mean[temp$PARM_CD == '91145' & temp$mean >= 100 & temp$mean < 1000] <- round(signif(
    temp$mean[temp$PARM_CD == '91145' & temp$mean >= 100 & temp$mean < 1000], 4), digits = 1)
  temp$mean[temp$PARM_CD == '91145' & temp$mean >= 1000] <- round(signif(
    temp$mean[temp$PARM_CD == '91145' & temp$mean >= 1000], 5), digits = 1)
  
  temp$stdev[temp$PARM_CD == '91145' & temp$stdev < 0.1 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '91145' & temp$stdev < 0.1 & !is.na(temp$stdev)], digits = 0), digits = 0)
  temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 0.1 & temp$stdev < 1 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 0.1 & temp$stdev < 1 & !is.na(temp$stdev)], 1), digits = 1)
  temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 1 & temp$stdev < 10 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 1 & temp$stdev < 10 & !is.na(temp$stdev)], 2), digits = 1)
  temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 10 & temp$stdev < 100 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 10 & temp$stdev < 100 & !is.na(temp$stdev)], 3), digits = 1)
  temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 100 & temp$stdev < 1000 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 100 & temp$stdev < 1000 & !is.na(temp$stdev)], 4), digits = 1)
  temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 1000 & !is.na(temp$stdev)] <- round(signif(
    temp$stdev[temp$PARM_CD == '91145' & temp$stdev >= 1000 & !is.na(temp$stdev)], 5), digits = 1)
  
  #ouput table not in scientific notation
  temp$min <- round(temp$min, digits = 2)
  temp$max <- round(temp$max, digits = 2)
  temp$median <- round(temp$median, digits = 2)
  temp$mean <- round(temp$mean, digits = 2)
  temp$stdev <- round(temp$stdev, digits = 2)
  
  
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
  
  if(!is.null(startMonth) & !is.null(endMonth)){
    names(temp)[names(temp)=='WY'] <- 'CY'
  }
  
  return(temp)
  
}



