# load the sedReview R package
library(sedReview)
data("exampleData2", package = "sedReview")

########################################
## Project Chief Review
## for review of a single site or several sites operated by a project chief
########################################

# import data for your site(s) using get_localNwis.
siteData <- get_localNWIS(DSN = 'nwisco',            # Colorado NWIS server 
                          env.db = '01',
                          qa.db = '02',
                          STAIDS = c(
                            '07104905',              # Monument Creek blw Bijou St., Colo.Springs
                            '09163500',              # CO River at UT state line
                            '06741510',              # BigT at Loveland
                            '07106500'),             # Fountain Creek at Pueblo
                          begin.date = '2015-10-01', # WY 2016-2017
                          end.date = '2017-09-30')

# run check_all function to see which sites/samples have flags
checkAll <- check_all(siteData)

### now go through each project level review function to view individual calculations, checks, counts, finds, and plots

# 


