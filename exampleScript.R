# to run individual sections highlight and hit Run, or Ctrl+Enter

## load the sedReview package
library(sedReview)

## load data from 2 sites
siteData <- get_localNWIS(DSN = 'nwisco',                             #Colorado NWIS server 
                          env.db = '01',
                          qa.db = '02',
                          STAIDS = c(
                            '402114105350101',     # BigT below Moraine Park - WQ site
                            '09163500',            # CO River at UT state line - WQ site w/ sediment
                            '06741510',            # BigT at Loveland - WQ site w/ sediment?
                            '401723105400000'      # Andrews Creek RMNP - WQ site w/ no sediment
                            ),
                          begin.date = '2017-07-01',
                          end.date = '2017-09-30')

# run the check_all function to output a summary of flags
allFlagsSummary <- check_all(siteData) 
View(allFlagsSummary)

# run the check_all function and output all flagged samples
allFlags <- check_all(siteData, returnAllTables = TRUE)
# view the metaData flagged samples
View(allFlags$metaDataFlags)

# view dqi code status of samples
View(allFlags$sampleStatus)

# these two things produce the same results, viewing the EWI/EDI verticals
View(allFlags$verticalsFlags)
vertFlags <- check_verticals(siteData)
View(vertFlags)

# write csv file to the D drive of vertical flags that can be opened in excel
write.table(vertFlags, file = 'D:/verticalFlags.csv', quote = TRUE, sep = ",",
            row.names = FALSE, col.names = TRUE, na = "")
