#' check_commentsNoResult. Review unique sample and result comments, and pcodes with no result reported.
#' @description Get unique sample and result comments, and pcodes with no result reported.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param pcodes A character vector of parameter codes of interest. Default pcodes are SSC (80154), Sand/silt break on suspended (70331), TSS (00530), SSL (80155), Bedload (80225), Bedload mass (91145)
#' @details Returns unique sample and result comments (\code{SAMPLE_CM_TX} and \code{RESULT_CM_TX} from NWIS) by site and record number.
#' @details Returns samples with defined parameter codes where no result was reported (\code{RESULT_VA = NA}) and joins them to comments.
#' @examples
#' data('exampleData', package = "sedReview")
#' checkComments <- check_commentsNoResult(exampleData)
#' @importFrom dplyr full_join
#' @export
#' @return A dataframe of sample info, comments, and flagged samples with no results reported.
#' 

check_commentsNoResult <- function(x, pcodes = c("80154","70331","00530","80155","80225","91145"))
{
  x <- x[x$PARM_CD %in% pcodes,]
  
  # get unique sample comments
  sample_cm <- x[!is.na(x$SAMPLE_CM_TX),]
  sample_cm <- unique(sample_cm[,c('UID','RECORD_NO','SITE_NO','STATION_NM','SAMPLE_START_DT','SAMPLE_CM_TX')])
  
  # get unique result comments
  result_cm <- x[!is.na(x$RESULT_CM_TX),]
  result_cm <- unique(result_cm[,c('UID','RECORD_NO','SITE_NO','STATION_NM','SAMPLE_START_DT','RESULT_CM_TX')])
  
  # join all comments
  comments <- dplyr::full_join(sample_cm, result_cm, by = c('UID','RECORD_NO','SITE_NO','STATION_NM','SAMPLE_START_DT'))
  comments <- comments[order(comments$SITE_NO, comments$SAMPLE_START_DT),]
  
  # get SED samples with no result reported and flag them
  naRes <- x[is.na(x$RESULT_VA),]
  naRes$flag[is.na(naRes$RESULT_VA)] <- paste0(naRes$PARM_CD[is.na(naRes$RESULT_VA)],' no result value reported')
  naRes <- naRes[c('UID','flag')]
  
  # join samples with no result reported. full_join will repeat samples if more than 1 pcode is missing result_va
  comments <- dplyr::full_join(comments, naRes, by = 'UID')
  
  return(comments)
}

