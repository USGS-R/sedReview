

data('exampleData', package = "sedReview")
x <- exampleData

pcodes = c("80154",
           "70331",
           "00530",
           "80155",
           "80225",
           "91145")

#limit data to pcodes of interest and remove blanks ******ASK MOLLY HOW TO HANDLE REMARKS*****
x <- x[!(x$MEDIUM_CD == "OAQ") & x$PARM_CD %in% pcodes, ]

#group by site, by pcode, by WY and calc stats
temp <- dplyr::summarise(dplyr::group_by(x,SITE_NO,STATION_NM,PARM_CD,PARM_NM,WY),
                         min = min(RESULT_VA),
                         max = max(RESULT_VA),
                         median = median(RESULT_VA),
                         mean = mean(RESULT_VA),
                         stdev = sd(RESULT_VA, na.rm = T))
