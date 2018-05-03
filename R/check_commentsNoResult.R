
x <- siteData
pcodes = c("80154",
           "70331",
           "00530",
           "80155",
           "80225",
           "91145")

x <- x[x$PARM_CD %in% pcodes,]

# get unique sample comments
sample_cm <- x[!is.na(x$SAMPLE_CM_TX),]
sample_cm <- unique(sample_cm[,c('UID','RECORD_NO','SITE_NO','STATION_NM','SAMPLE_START_DT','SAMPLE_CM_TX')])

# get unique result comments
result_cm <- x[!is.na(x$RESULT_CM_TX),]
result_cm <- unique(result_cm[,c('UID','RECORD_NO','SITE_NO','STATION_NM','SAMPLE_START_DT','RESULT_CM_TX')])

# join all comments
comments <- dplyr::full_join(sample_cm, result_cm, by = c('UID','RECORD_NO','SITE_NO','STATION_NM','SAMPLE_START_DT'))
comments <- comments[order(comments$SITE_NO, comments$SAMPLE_START_DT),]

# get SED samples with no result reported and flag them
naRes <- x[is.na(x$RESULT_VA),]
naRes$flag <- paste0(naRes$PARM_CD,' no result value reported')
naRes <- naRes[c('UID','flag')]

# join samples with no result reported. full_join will repeat samples if more than 1 pcode is missing result_va
comments <- dplyr::full_join(comments, naRes, by = 'UID')

return(comments)
