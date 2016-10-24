checkSamPurp <- function(x){
  purp <- x[x$PARM_CD == "71999", ] %>%
    group_by(RESULT_VA) %>%
    summarise(count = n()) %>%
    rename( SamplePurpose = RESULT_VA)
  
  return(as.data.frame(purp))
}