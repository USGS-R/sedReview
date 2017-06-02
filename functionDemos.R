library(sedReview)
# 
# #Various. Idaho - 12308000, 12308500, 12310100; 
# #Illinois - 05586300, 06807000 (this one is in NE but I think data is in IL's DB); 
# #Missouri - 06934500; 
# #Oregon - 14246900; 
# #Texas - 08067252)
# 
# data_IL <- getLocalNWIS(DSN = "NWISIL", 
#                         env.db = "01", 
#                         STAIDS = c("05586300", "06807000"), 
#                         begin.date = NA, 
#                         end.date = NA,
#                         projectCd = NULL, 
#                         resultAsText = FALSE)
# 
# data_ID <- getLocalNWIS(DSN = "NWISID", 
#                         env.db = "01", 
#                         STAIDS = c("12308000", "12308500", "12310100"), 
#                         begin.date = NA, 
#                         end.date = NA,
#                         projectCd = NULL, 
#                         resultAsText = FALSE)
# 
# data_MO <- getLocalNWIS(DSN = "NWISMO", 
#                         env.db = "01", 
#                         STAIDS = c("06934500"), 
#                         begin.date = NA, 
#                         end.date = NA,
#                         projectCd = NULL, 
#                         resultAsText = FALSE)
# 
# data_OR <- getLocalNWIS(DSN = "NWISOR", 
#                         env.db = "01", 
#                         STAIDS = c("14246900"), 
#                         begin.date = NA, 
#                         end.date = NA,
#                         projectCd = NULL, 
#                         resultAsText = FALSE)
# 
# exampleData <- dplyr::bind_rows(data_ID,data_IL,data_MO,data_OR)

#Load an example dataset
data("exampleData",package="sedReview")

##############
#Run checks
##############

#Bag IE
bagIEFlags <- check_bagIE(exampleData)

#has Q
hasQFlags <- check_hasQ(exampleData,returnAll = FALSE)

#Coding and meta data
metaDataFlags <- check_metaData(exampleData)

#Sample purpose
samplePurpFlags <- check_samplePurp(exampleData)

#Sampler type
samplerTypeFlags <- check_samplerType(exampleData)

#TSS has SSC
tssFlags <- check_tss(exampleData)

#Number of verticles
verticlesFlags <- check_verticles(exampleData)

#Count sampling methods
methodsBySite <- count_methodsBySite(exampleData)

#Count sample status
sampleStatus <- count_sampleStatus(exampleData,bySite = TRUE)

#Find outliers
outliers <- find_outliers(exampleData)

#Get a simple data table
wideDataTable <- make_wideTable(exampleData)

