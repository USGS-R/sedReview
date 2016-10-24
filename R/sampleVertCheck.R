# function to test EDI sample collected correctly. Number of sampling verticles 4-9.
# function to test EWI sample collected correctly. Number of verticles between 10 and 20

sampleVertCheck <- function(x, method, lowVert, highVert){
  #select EI sample methods
  EI <- x[x$PARM_CD == "82398", ] %>%
    filter(RESULT_VA == method)
  #select verticles parameter
  verts <- x[x$PARM_CD == "00063", ]
  #join verticles to EI samples by sample date/time
  EI <- left_join(EI, verts, by = "SAMPLE_START_DT") %>%
    select(SAMPLE_START_DT, contains("RESULT_VA"))
  
  # EI with missing verticle info
  missingEIvert <- EI[is.na(EI$RESULT_VA.y) == TRUE, ] %>%
    select(SAMPLE_START_DT, RESULT_VA.x, RESULT_VA.y) %>%
    rename(SampleMethod = RESULT_VA.x, NumVerticles = RESULT_VA.y) %>%
    filter(is.na(SAMPLE_START_DT) == FALSE) %>%
    arrange(SAMPLE_START_DT)
  # EDI with verticles outside of target - MANY NEED TWEEKING WITH OSW INPUT ON TARGET NUM VERTS
  lowEIvert <- EI[EI$RESULT_VA.y < lowVert, ] %>%
    select(SAMPLE_START_DT, RESULT_VA.x, RESULT_VA.y) %>%
    rename(SampleMethod = RESULT_VA.x, NumVerticles = RESULT_VA.y) %>%
    filter(is.na(SAMPLE_START_DT) == FALSE) %>%
    arrange(SAMPLE_START_DT)
  highEIvert <- EI[EI$RESULT_VA.y > highVert, ] %>%
    select(SAMPLE_START_DT, RESULT_VA.x, RESULT_VA.y) %>%
    rename(SampleMethod = RESULT_VA.x, NumVerticles = RESULT_VA.y) %>%
    filter(is.na(SAMPLE_START_DT) == FALSE) %>%
    arrange(SAMPLE_START_DT)
  return(list(missingEIvert, lowEIvert, highEIvert))
}