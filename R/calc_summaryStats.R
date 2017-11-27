#' calc_summaryStats
#' @description Calculates summary statistics grouped by site, parameter, and water year.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param pcodes A character vector of parameter codes of interest. Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
#' @details Calculates minimum, maximum, median, mean, and standard deviation (if applicable).
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
  #limit data to pcodes of interest and remove blanks ******ASK MOLLY HOW TO HANDLE REMARKS*****
  x <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes, ]
  
  #group by site, by pcode, by WY and calc stats
  temp <- dplyr::summarise(dplyr::group_by(x,SITE_NO,STATION_NM,PARM_CD,PARM_NM,WY),
                           min = min(RESULT_VA),
                           max = max(RESULT_VA),
                           median = median(RESULT_VA),
                           mean = mean(RESULT_VA),
                           stdev = sd(RESULT_VA, na.rm = T))
  temp$mean <- format(temp$mean, scientific = FALSE)
  
  return(temp)
  
}



