
data('exampleData', package = "sedReview")
x <- exampleData

env.db = "01"
qa.db = "02"

# get sediment samples with DQI of S
x <- x[x$DQI_CD == 'S' & x$PARM_CD %in% c("NEED LIST OF PCODES FROM MOLLY"),]

# convert db numbers to character if necessary
if(is.numeric(env.db)){
  env.db <- formatC(env.db, width = 2, format = "d", flag = "0")
}
if(is.numeric(qa.db)){
  qa.db <- formatC(qa.db, width = 2, format = "d", flag = "0")
}

#split records into env and qa database
env.recs <- grep(pattern = paste0("_",env.db), x$RECORD_NO, value = TRUE, fixed = TRUE)
env.x <- x[x$RECORD_NO %in% env.recs,]

qa.recs <- grep(pattern = paste0("_",qa.db), x$RECORD_NO, value = TRUE, fixed = TRUE)
qa.x <- x[x$RECORD_NO %in% qa.recs,]

