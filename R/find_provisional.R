#' find_provisional
#' @description Find sediment parameter code records with DQI code of "S" and output record number files for QWDATA.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param env.db A character string containing the database number of environmental samples
#' @param qa.db A character string containing the database number of QA samples
#' @param pcodes A character vector of parameter codes of interest. Default pcodes are SSC (80154), Sand/silt break on suspended (70331), Suspended sediment mass(91157) TSS (00530), Bedload mass (91145)
#' @param env.fileout A character string of the filename for environmental database records to be output. Default is NULL, if NULL a file will not be output.
#' @param qa.fileout A character string of the filename for QA database records to be output. Default is NULL, if NULL a file will not be output.
#' @param view A logical vector. If TRUE, view tabls will automatically open in RStudio for the environmental and QA database records. 
#' @details In RStudio the function will automatically show the environmental and QA database records in view tabs when view = TRUE, which is the default
#' @details Default pcodes are SSC (80154), Sand/silt break on suspended (70331), Suspended sediment mass(91157) TSS (00530), Bedload mass (91145)
#' @examples
#' data('exampleData', package = "sedReview")
#' x <- exampleData
#' provisional.recs <- find_provisional(x, view = FALSE)
#' \dontrun{
#' view(provisional.recs$env.provisional)}
#' @export
#' @return A list of dataframes for the environmental and QA database records. List names are env.provisional and qa.provisional


find_provisional <- function(x, env.db = "01", qa.db = "02",
                             pcodes = c("80154",
                                        "70331",
                                        "91157",
                                        "00530",
                                        "91145"),
                             env.fileout = NULL, qa.fileout = NULL, view = TRUE)
{
  # get sediment samples with DQI of S
  x <- x[x$DQI_CD == 'S' & x$PARM_CD %in% pcodes,]
  
  # convert db numbers to character if necessary
  if(is.numeric(env.db)){
    env.db <- formatC(env.db, width = 2, format = "d", flag = "0")
  }
  if(is.numeric(qa.db)){
    qa.db <- formatC(qa.db, width = 2, format = "d", flag = "0")
  }
  
  #split records into env and qa database
  env.recs <- grep(pattern = paste0("_",env.db), x$RECORD_NO, value = TRUE, fixed = TRUE)
  env.provisional <- x[x$RECORD_NO %in% env.recs,]
  
  qa.recs <- grep(pattern = paste0("_",qa.db), x$RECORD_NO, value = TRUE, fixed = TRUE)
  qa.provisional <- x[x$RECORD_NO %in% qa.recs,]
  
  #viewable table of results
  env.provisional <- env.provisional[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT",
                   "MEDIUM_CD","PARM_CD","PARM_NM","REMARK_CD","RESULT_VA","DQI_CD","SAMPLE_CM_TX")]
  
  qa.provisional <- qa.provisional[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT",
                 "MEDIUM_CD","PARM_CD","PARM_NM","REMARK_CD","RESULT_VA","DQI_CD","SAMPLE_CM_TX")]
  if(view == TRUE){
    View(qa.provisional)
    View(env.provisional)
  }
  
  
  
  #output record number files if filenames specified. ****NEED TO TEST THAT FILE FORMAT WORKS IN QWDATA****
  if(!is.null(env.fileout)){
    env.provisional$RECORD_NO <- sub(pattern = paste0("_",env.db), replacement = "", env.provisional$RECORD_NO)
    write.table(env.provisional, file = env.fileout, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  if(!is.null(qa.fileout)){
    qa.provisional$RECORD_NO <- sub(pattern = paste0("_",qa.db), replacement = "", qa.provisional$RECORD_NO)
    write.table(qa.provisional, file = qa.fileout, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  
  sed.provisional <- list(env.provisional, qa.provisional)
  names(sed.provisional) <- c("env.provisional","qa.provisional")
  return(sed.provisional)
}

