#Function to get all active sediment sites

#here is how to get the whole site file 
#Query <- "select * from NWISCO.SITEFILE_01"
#Then just subset it to SAMPLE_MD in the last 5 years

#activeSTAIDS <- get_activeSites(DSN="NWISCO",env.db="01)
#sedData <- get_localNWIS(STAIDS = activeSTAIDS)

DSN <- "NWISCO"
env.db = "01"


Query <- paste("select * from ", DSN, ".QW_SAMPLE_",env.db,sep="")
Samples <- RODBC::sqlQuery(Chan1, Query, as.is=T)

#Need to subset by date in the SQL querry
Query <- paste0("select * from ", DSN, ".QW_SAMPLE_",env.db," where sample_start_dt between '",
                format(Sys.Date() - 365*5,"%Y-%m-%d %H:%M:%S"),
                "' and '",
                format(Sys.Date()+1,"%Y-%m-%d %H:%M:%S"),"'")
Samples <- RODBC::sqlQuery(Chan1, Query, as.is=T)


Query <- paste0("select * from NWISCO.QW_SAMPLE_01 where sample_start_dt between cast('2010-01-01' as datetime) and cast('2015-01-01' as datetime)")

