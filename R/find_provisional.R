
env.db = "01"
qa.db = "02"

# get samples with DQI of S
x <- x[x$DQI_CD == 'S',]

#split records into env and qa database
if(is.numeric(env.db)){
  env.db <- formatC(env.db, width = 2, format = "d", flag = "0")
}
if(is.numeric(qa.db)){
  qa.db <- formatC(qa.db, width = 2, format = "d", flag = "0")
}

env.recs <- grep(pattern = paste0("_",env.db), x$RECORD_NO, value = TRUE, fixed = TRUE)
env.x <- x[x$RECORD_NO %in% env.recs,]

qa.recs <- grep(pattern = paste0("_",qa.db), x$RECORD_NO, value = TRUE, fixed = TRUE)
qa.x <- x[x$RECORD_NO %in% qa.recs,]
