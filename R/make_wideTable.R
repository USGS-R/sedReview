#' make_wideTable
#' 
#' @description Reformats teh longTable output (by result) from getLocalNWIS into wide format (by sample)
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @return A wide format data.frame 
#' @details The wide format data frame format is primarily for visual inspection of data.
#' A number of important metadata are missing from this table and thus it is not recommended to use these data in analyis
#' @examples
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' make_wideTableOut <- make_wideTable(x)
#' @importFrom dplyr left_join
#' @importFrom reshape2 dcast
#' @export
#' 


make_wideTable <- function(x) {
  
  ###Get wide format table
  longTable <- x
  
  wideTable <- longTable[!(duplicated(longTable[c("UID","PARM_CD")])),]
  wideTable <- reshape2::dcast(wideTable, UID ~ PARM_CD,value.var = "Val_qual")
  
  #rename pcodes to parm names
  parmNames <- as.data.frame(names(wideTable),stringsAsFactors=FALSE)
  names(parmNames) <- "PARM_CD"
  parmNames <- dplyr::left_join(parmNames,unique(longTable[c("PARM_CD","PARM_NM")]),by="PARM_CD")
  parmNames$PARM_NM <- make.unique(make.names(parmNames$PARM_NM))
  names(wideTable) <- c("UID", parmNames$PARM_NM[2:length(parmNames$PARM_NM)])
  
  #fill in record number meta data (station ID, name, date, time, etc)
  metaData <- unique(longTable[c("UID","RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","SAMPLE_END_DT","MEDIUM_CD",
                                 "SAMP_TYPE_CD","SITE_TP_CD","AGENCY_CD","PROJECT_CD","AQFR_CD","LAB_NO","HYD_EVENT_CD",
                                 "SAMPLE_CR","SAMPLE_CN","SAMPLE_MD","SAMPLE_MN",
                                 "SAMPLE_START_SG","SAMPLE_START_TZ_CD","SAMPLE_START_LOCAL_TM_FG",
                                 "SAMPLE_END_SG","SAMPLE_END_TZ_CD","SAMPLE_END_LOCAL_TM_FG","SAMPLE_ID",
                                 "TM_DATUM_RLBLTY_CD","ANL_STAT_CD","HYD_COND_CD",
                                 "TU_ID","BODY_PART_ID","COLL_ENT_CD","SIDNO_PARTY_CD")])
  wideTable <- dplyr::left_join(wideTable,metaData,
                                by="UID")
  
  
  #reorder columns so meta data is at front
  parmcols <- seq(from =2, to =ncol(wideTable)-ncol(metaData)+1)
  metacols <- seq(from = ncol(wideTable)-(ncol(metaData)-2), to =ncol(wideTable))
  wideTable <- wideTable[c(1,metacols[1:9],parmcols,metacols[10:length(metacols)])]
  
  return(wideTable)
}