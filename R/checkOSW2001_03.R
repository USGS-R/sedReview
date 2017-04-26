#' checkOSW2001_03
#' @description Function to flag TSS results without an accompanying SSC result
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @details See OSW Technical Memo No. 2001.03 for more details and background information
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData$longTable
#' checkOSW2001_03Flags <- checkOSW2001_03(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
checkOSW2001_03 <- function(x, returnAll = FALSE){
  ### extract TSS results
  TSS <- x[x$PARM_CD == "00530",]
  TSS <- unique(TSS[c("RECORD_NO", "RESULT_VA")])
  ### extract SSC results
  SSC <- x[x$PARM_CD == "80154",]
  SSC <- unique(SSC[c("RECORD_NO", "RESULT_VA")])
  names(SSC) <- c("RECORD_NO", "RESULT_VA_SSC")
  
  ### add SSC results to TSS and compare
  TSS <- dplyr::left_join(TSS, SSC, by = "RECORD_NO")
  TSS$OSW2001_03Flag[is.na(TSS$RESULT_VA)==FALSE & is.na(TSS$RESULT_VA_SSC)==TRUE] <- paste("flag TSS without SSC")
  