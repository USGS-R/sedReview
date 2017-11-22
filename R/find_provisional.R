
data('exampleData', package = "sedReview")
x <- exampleData


find_provisional <- function(x, env.db = "01", qa.db = "02",
                             pcodes = c("80154",
                                        "70331",
                                        "00530",
                                        "80155",
                                        "80225",
                                        "91145"),
                             env.fileout = NULL, qa.fileout = NULL)
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
  View(env.provisional)
  
  qa.provisional <- qa.provisional[c("RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT",
                 "MEDIUM_CD","PARM_CD","PARM_NM","REMARK_CD","RESULT_VA","DQI_CD","SAMPLE_CM_TX")]
  View(qa.provisional)
  
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

