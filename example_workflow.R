# load the sedReview R package
library(sedReview)
data("exampleData2", package = "sedReview")

########################################
## Project Chief Review
## for review of a single site or several sites operated by a project chief
## for any created dataframe (table of data or results), click in the upper left Environment tab on the dataset to view it.
## to see the help page for any function, type ?sedReview:: then the funtion of interest into the console.
## to run section of code, highlight and hit Run, or Ctrl+Enter.
## to run an individual line, make sure the cursor is anywhere in that line and hit Ctrl+Enter or Run.
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

# calculate sand and fines concentrations and output quick plots. Use arrows in Plots tab to scroll through output plots
concSandFines <- calc_concSandFine(siteData, plotTime = TRUE, plotFlow = TRUE)

# calculate summary statistics table using default sediment parameters
sumStats <- calc_summaryStats(siteData)

# check bag sampler intake efficiency. There are no bag samples in my siteData so a dataframe of 0 observations was returned
bagIE <- check_bagIE(siteData)

# check and flag samples that don't have associated discharge
hasQ <- check_hasQ(siteData)

# check metadata and flag samples that are missing NWIS 20.xx level checks
metaData <- check_metaData(siteData)

# check for sediment samples that are in the QAQC database (for NWISCO that is DB 02)
qaqc <- check_qaqcDB(siteData, qa.db = '02')

# check and flag samples not collected with most common purpose
purp <- check_samplePurp(siteData)

# check and flag samples not collected with most common sampler type




