

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

#group by site, by WY, by pcode and calc stats
temp <- dplyr::summarise(dplyr::group_by(x,SITE_NO,WY,PARM_CD),
                         mean = mean(x$RESULT_VA))
