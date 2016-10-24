# function to test EDI sample collected correctly. Number of sampling verticles 4-9.

sampleEDI <- function(x){
  #select EDI sample methods
  EDI <- x[x$PARM_CD == "82398", ] %>%
    filter(RESULT_VA == 10)
  #select verticles parameter
  verts <- x[x$PARM_CD == "00063", ]
  #join verticles to EDI samples by sample date/time
  EDI <- left_join(EDI, verts, by = "SAMPLE_START_DT") %>%
    select(SAMPLE_START_DT, contains("RESULT_VA"))
  
  # EDI with missing verticle info
  missingEDIvert <- EDI[is.na(EDI$RESULT_VA.y) == TRUE, ] %>%
    select(SAMPLE_START_DT, RESULT_VA.x, RESULT_VA.y) %>%
    rename(SampleMethod = RESULT_VA.x, NumVerticles = RESULT_VA.y) %>%
    filter(is.na(SAMPLE_START_DT) == FALSE) %>%
    arrange(SAMPLE_START_DT)
  # EDI with verticles outside of target - MANY NEED TWEEKING WITH OSW INPUT ON TARGET NUM VERTS
  lowEDIvert <- EDI[EDI$RESULT_VA.y < 4, ] %>%
    select(SAMPLE_START_DT, RESULT_VA.x, RESULT_VA.y) %>%
    rename(SampleMethod = RESULT_VA.x, NumVerticles = RESULT_VA.y) %>%
    filter(is.na(SAMPLE_START_DT) == FALSE)
  highEDIvert <- EDI[EDI$RESULT_VA.y >9, ] %>%
    select(SAMPLE_START_DT, RESULT_VA.x, RESULT_VA.y) %>%
    rename(SampleMethod = RESULT_VA.x, NumVerticles = RESULT_VA.y) %>%
    filter(is.na(SAMPLE_START_DT) == FALSE)
  return(list(missingEDIvert, lowEDIvert, highEDIvert))
}