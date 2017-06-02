library(sedReview)

#Various. Idaho - 12308000, 12308500, 12310100; 
#Illinois - 05586300, 06807000 (this one is in NE but I think data is in IL's DB); 
#Missouri - 06934500; 
#Oregon - 14246900; 
#Texas - 08067252)

data_IL <- getLocalNWIS(DSN = "NWISIL", 
                        env.db = "01", 
                        STAIDS = c("05586300", "06807000"), 
                        begin.date = NA, 
                        end.date = NA,
                        projectCd = NULL, 
                        resultAsText = FALSE)

data_ID <- getLocalNWIS(DSN = "NWISID", 
                        env.db = "01", 
                        STAIDS = c("12308000", "12308500", "12310100"), 
                        begin.date = NA, 
                        end.date = NA,
                        projectCd = NULL, 
                        resultAsText = FALSE)

data_MO <- getLocalNWIS(DSN = "NWISMO", 
                        env.db = "01", 
                        STAIDS = c("06934500"), 
                        begin.date = NA, 
                        end.date = NA,
                        projectCd = NULL, 
                        resultAsText = FALSE)

data_OR <- getLocalNWIS(DSN = "NWISOR", 
                        env.db = "01", 
                        STAIDS = c("14246900"), 
                        begin.date = NA, 
                        end.date = NA,
                        projectCd = NULL, 
                        resultAsText = FALSE)

testData <- dplyr::bind_rows(data_ID,data_IL,data_MO,data_OR)

save(testData,file="data/exampleData.rda")
