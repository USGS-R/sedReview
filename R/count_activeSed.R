

DSN <- 'nwisco'
env.db = "01"
begin.date = NA
end.date = NA

begin.date <- as.POSIXct('2015-01-01')
end.date <- as.POSIXct(Sys.Date())
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
unique(SiteFile$SITE_TP_CD)
