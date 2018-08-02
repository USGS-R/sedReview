#' For higher level review. Retrieves all stream site sediment samples in a given date range via an ODBC connection to an internal NWIS server,
#' and returns a summary table by site, water year, and number of samples in each analysis.
#' 
#' @description For higher level review. Retrieves all stream site sediment samples in a given date range via an ODBC connection to an internal NWIS server,
#' and returns a summary table by site, water year, and number of samples in each analysis.
#' @param DSN A character string containing the DSN for your local server
#' @param env.db A character string containing the database number of environmental samples
#' @param begin.date Character string containing beginning date of data pull (yyyy-mm-dd)
#' @param end.date Character string containing ending date of data pull (yyyy-mm-dd)
#' @details For review at the center level. Function retrieves all stream site (SITE_TYP_CD = 'ST') sediment 
#' (SSC (80154), sand/silt break (70331), TSS (00530), and bedload (80225)) samples in the defined date range of the users internal NWIS database.
#' A summary table of sample counts by site, water year, and parameter is returned. 
#' @examples 
#' \dontrun{
#' #Will not run unless connected to NWISCO
#' activeSed <- count_activeSed(DSN = 'nwisco',
#'                              env.db = '01',
#'                              begin.date = '2015-10-01',
#'                              end.date = '2017-09-30')}
#'                              
#' @import RODBC
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @export
#' @return A summary dataframe



count_activeSed <- function(DSN,
                            env.db = "01",
                            begin.date = NA,
                            end.date = NA){
  ##Check that the 32 bit version of r is running
  if(Sys.getenv("R_ARCH") != "/i386"){
    print("You are not running 32 bit R. This function requires R be run in 32 bit mode for the ODBC driver to function properly. Please restart R in 32bit mode.")
    stop("You are not running 32 bit R. This function requires R be run in 32 bit mode for the ODBC driver to function properly. Please restart R in 32bit mode.")
  }
  
  ###Check for valid inputs
  if(is.null(DSN)){
    print("A valid datasource name must be entered for the ODBC connection")
    stop("A valid datasource name must be entered for the ODBC connection")
  }
  if(is.na(begin.date)){
    print("Please specify begin.date 'YYYY-MM-DD")
    stop("Please specify begin.date 'YYYY-MM-DD")
  }
  if(is.na(end.date)){
    print("Please specify end.date 'YYYY-MM-DD")
    stop("Please specify end.date 'YYYY-MM-DD")
  }
  begin.date <- as.POSIXct(begin.date, tz = 'GMT')
  end.date <- as.POSIXct(end.date, tz = 'GMT')
  
  RODBC::odbcCloseAll()
  
  #############################################################################
  Chan1 <- RODBC::odbcConnect(DSN)###Start of ODBC connection
  #############################################################################
  if(Chan1 == -1L)
  {
    stop("ODBC connection failed. Check DSN name and ODBC connection settings")
  }
  ##################
  ###Env Database###
  ##################
  # First get the site info--need column SITE_ID. Limit to only stream (ST) sites.
  Query <- paste("select * from ", DSN, ".SITEFILE_",env.db," where site_tp_cd IN ('ST')",sep="")
  SiteFile <- RODBC::sqlQuery(Chan1, Query, as.is=T)
  
  if(length(grep("table or view does not exist",SiteFile)) > 0)
  {
    stop("Incorrect database number entered for env.db")
  }
  
  #Change to a list that SQL can understand. SQL requires a parenthesized list of expressions, so must look like c('05325000', '05330000') for example
  STAID.list <- paste("'", SiteFile$SITE_NO[1:1000], "'", sep="", collapse=",")
  
  #get the QWSample file by date range specified and limit to only samples in the SiteFile site numbers
  Query <- paste("select * from ", DSN, ".QW_SAMPLE_",env.db,
                 " where sample_start_dt >= TO_DATE ('",begin.date," 00:00:00', 'YYYY-MM-DD HH24:MI:SS') AND sample_start_dt <= TO_DATE ('",
                 end.date," 00:00:00', 'YYYY-MM-DD HH24:MI:SS')",sep="")
  Samples <- RODBC::sqlQuery(Chan1, Query, as.is=T)
  Samples <- Samples[Samples$SITE_NO %in% SiteFile$SITE_NO,]
  
  ##Check if samples were pulled and quit if no
  if(nrow(Samples) == 0) {
    #print("No samples exist in your local NWIS database for site number specified, check data criteria")
    warning("No qw_samples exist in your local environmantal NWIS database for date range specified")
  }
  
  #get the QWResult file using the record numbers and limit to sediment 
  #parameter codes SSC (80154), sand/silt break (70331), TSS (00530), and bedload (80225)
  ##SQL is limited to 1000 entries in query
  ##Get the number of 1000 bins in query
  parmcd.list <- paste0("('80154','70331','00530','80225')")
  breaks <- ceiling(length(Samples$RECORD_NO)/1000)
  
  ##Run SQL queries
  for(i in 1:breaks)
  {
    j <- i-1
    ###Get the 1st 1000
    if(i == 1)
    {
      ####Get the results
      records.list <- paste("'", Samples$RECORD_NO[1:1000], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".QW_RESULT_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      Results <- RODBC::sqlQuery(Chan1, Query, as.is=T)
      ####Get result level commments
      Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      resultComments <- RODBC::sqlQuery(Chan1, Query, as.is=T)
      ####Get sample level comments
      Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      sampleComments <- RODBC::sqlQuery(Chan1, Query, as.is=T)
      ####Get qualifier codes
      Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      resultQualifiers <- RODBC::sqlQuery(Chan1, Query, as.is=T)
    } else if(i > 1 & (j*1000+1000) < length(Samples$RECORD_NO))
      ###Get the middle ones
    {
      j <- j * 1000+1
      records.list <- paste("'", Samples$RECORD_NO[j:(j+999)], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".QW_RESULT_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      Results <- rbind(Results,RODBC::sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      resultComments <- rbind(resultComments,RODBC::sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      sampleComments <- rbind(sampleComments,RODBC::sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      resultQualifiers <- rbind(resultQualifiers,RODBC::sqlQuery(Chan1, Query, as.is=T))
    } else if (i > 1 && (j*1000+1000) > length(Samples$RECORD_NO))
    {
      ###Get the last ones
      j <- j * 1000+1
      records.list <- paste("'", Samples$RECORD_NO[j:length(Samples$RECORD_NO)], "'", sep="", collapse=",")
      Query <- paste("select * from ", DSN, ".QW_RESULT_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      Results <- rbind(Results,RODBC::sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_RESULT_CM_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      resultComments <- rbind(resultComments,RODBC::sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_SAMPLE_CM_",env.db," where record_no IN (", records.list, ")", sep="")
      sampleComments <- rbind(sampleComments,RODBC::sqlQuery(Chan1, Query, as.is=T))
      Query <- paste("select * from ", DSN, ".QW_VAL_QUAL_",env.db," where record_no IN (", records.list, ") and parm_cd IN ", parmcd.list, sep="")
      resultQualifiers <- rbind(resultQualifiers,RODBC::sqlQuery(Chan1, Query, as.is=T))
    } else{}
  }
  ###Join comments to results
  Results <- dplyr::left_join(Results,sampleComments,by="RECORD_NO")
  Results <- dplyr::left_join(Results,resultComments,by=c("RECORD_NO","PARM_CD"))
  Results <- dplyr::left_join(Results,resultQualifiers,by=c("RECORD_NO","PARM_CD"))
  
  Results$Val_qual <- paste(Results$REMARK_CD,Results$RESULT_VA, sep = " ")
  Results$Val_qual <- gsub("NA","",Results$Val_qual)
  
  ###Get parameter info
  Query <- paste("select * from ", DSN, ".PARM where PARM_CD IN ", parmcd.list, sep="")
  parms <- RODBC::sqlQuery(Chan1, Query, as.is=T)
  
  #############################################################################
  RODBC::odbcClose(Chan1)###End of ODBC connection
  #############################################################################
  
  ####Join all info
  Samples <- dplyr::left_join(Samples, SiteFile, by = c("SITE_NO"="SITE_NO","AGENCY_CD"="AGENCY_CD"))
  Results <- dplyr::left_join(Results, parms, by = "PARM_CD")
  Results <- dplyr::left_join(Results, Samples, by = "RECORD_NO")
  Results$WY <- sedReview::waterYear(Results$SAMPLE_START_DT, numeric = TRUE)
  
  Table <- dplyr::summarise(dplyr::group_by(Results,SITE_NO,STATION_NM,WY),
                            SSC_80154 = length(RECORD_NO[PARM_CD == "80154"]),
                            SandSilt_70331 = length(RECORD_NO[PARM_CD == "70331"]),
                            TSS_00530 = length(RECORD_NO[PARM_CD == "00530"]),
                            bedload_80225 = length(RECORD_NO[PARM_CD == "80225"]))
  ###Remove trailing spaces on site IDs
  
  Table$SITE_NO <- gsub(" ","",Table$SITE_NO)
  
  return(Table)
}
