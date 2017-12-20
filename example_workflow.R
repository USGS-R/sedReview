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

# to run checks for only one site, rerun get_localNWIS with only one STAIDS, or subset your data in R with.
# subset siteData to only Fountain Creek
fountain <- siteData[siteData$SITE_NO == '07106500', ]
monument <- siteData[siteData$SITE_NO == '07104905', ]

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
sampler <- check_samplerType(siteData)

# check and flag sediment mass samples < 2mg
sedMass <- check_sedMass(siteData)

# check and flag TSS samples without paired SSC sample
unpairedTSS <- check_tss(siteData)

# check and flag EWI/EDI samples low/high/missing verticals
verticals <- check_verticals(siteData)

# count of SSC sampling method, sampler type and bedload method
methods <- count_methodsBySite(siteData)

# count DQI code status
status <- count_sampleStatus(siteData)

# find box coefficient pairs at individual sites
boxcoef_09163500 <- find_boxcoef(siteData, site_no = '09163500')
boxcoef_07106500 <- find_boxcoef(siteData, site_no = '07106500') # or you can run the function on just the fountain data set
boxcoef_fountain <- find_boxcoef(fountain)

# find outliers at individual sites
outliers_09163500 <- find_outliers(siteData, site_no = '09163500')
outliers_07106500 <- find_outliers(siteData, site_no = '07106500') # or you can run the function on just the fountain data set
outliers_fountain <- find_outliers(fountain)

# find provisional record numbers and output tab delimited files to D: drive. 
# Output file can be opened in excel, or used as input to QWDATA (this part needs testing)
provisional <- find_provisional(siteData,
                                env.db = '01',
                                qa.db = '02',
                                env.fileout = 'D:/ex_sediment_records_env.txt')

# plot sediment versus streamflow 
sedFlow_09163500 <- plot_sedFlow(siteData, siteSelect = '09163500')
sedFlow_fountain <- plot_sedFlow(fountain)
###### to view a plot select from the plot list using $ operator
sedFlow_09163500$SSC
sedFlow_09163500$ssbreak
sedFlow_fountain$SSC
sedFlow_fountain$ssbreak
################## or output to a PDF file with all available plots
plot_sedFlow(siteData, siteSelect = '09163500', PDFout = 'D:/ex_09163500_sedFlow.pdf')
plot_sedFlow(fountain, PDFout = 'D:/ex_fountain_sedFlow.pdf')

# plot timeseries of sediment data
sedTS_09163500 <- plot_sedTS(siteData, siteSelect = '09163500')
sedTS_09163500$SSC
sedTS_09163500$ssbreak
plot_sedTS(siteData, siteSelect = '09163500', PDFout = 'D:/ex_09163500_TS.pdf')

sedTS_fountain <- plot_sedTS(fountain)
sedTS_fountain$SSC
sedTS_fountain$ssbreak
plot_sedTS(fountain, PDFout = 'D:/ex_fountain_TS.pdf')

# plot SSC and TSS boxplots and scatterplot
ssctss_09163500 <- plot_SSCTSS(siteData, siteSelect = '09163500')
ssctss_09163500$SSC
plot_SSCTSS(siteData, siteSelect = '09163500', PDFout = 'D:/ex_09163500_ssctss.pdf')

ssctss_monument <- plot_SSCTSS(monument)
ssctss_monument$SSC
ssctss_monument$TSS
ssctss_monument$combined
ssctss_monument$scatter
plot_SSCTSS(monument, PDFout = 'D:/ex_monument_ssctss.pdf')

# plot turb versus SSC
turbSSC_09163500 <- plot_turbSSC(siteData, siteSelect = '09163500')
turbSSC_09163500$Turbidity_63676

turbSSC_monument <- plot_turbSSC(monument)
turbSSC_monument$Turbidity_63680
turbSSC_monument$Turbidity_63676
plot_turbSSC(monument, PDFout = 'D:/ex_monument_turbSSC.pdf')

########################################
## Center Level Review
## for review of mulitiple sediment sites/projects in a WSC. Some overlapping functions with Project Chief Review.
## for any created dataframe (table of data or results), click in the upper left Environment tab on the dataset to view it.
## to see the help page for any function, type ?sedReview:: then the funtion of interest into the console.
## to run section of code, highlight and hit Run, or Ctrl+Enter.
## to run an individual line, make sure the cursor is anywhere in that line and hit Ctrl+Enter or Run.
########################################

# get a count of all active sediment sites/samples in a database in a given time frame
activeSed <- count_activeSed(DSN = 'nwisco',
                             env.db = '01',
                             begin.date = '2015-10-01',
                             end.date = '2017-09-30')

# extract a list of all the site numbers containing WY2016-2017 sediment data from activeSed
# use this to pull NWIS data for these sites. this could take several minutes depending on how many sites, timespan, and network connection
sedSites <- unique(activeSed$SITE_NO)
centerData <- get_localNWIS(DSN = 'nwisco',
                            env.db = '01',
                            qa.db = '02',
                            STAIDS = sedSites,
                            begin.date = '2015-10-01',
                            end.date = '2017-09-30')

# get summary of boxcoefficient pairs at all the sites
boxcoefSum <- summary_boxcoef(centerData)
#### you can also output the boxcoefficient summary and the data for all sites as an R list. Access list elements using $ operator
boxcoefSum_all <- summary_boxcoef(centerData, returnAllTables = TRUE)
fountain_boxcoef <- boxcoefSum_all$`07106300`

# check EWI/EDI sample verticals
verticals2 <- check_verticals(centerData)

# check bag intake efficiency
bagIE2 <- check_bagIE(centerData)

# check discharge accompanies sediment sample
hasQ2 <- check_hasQ(centerData)

# check for TSS samples without SSC per OSW memo 01.03
unpairedTSS2 <- check_tss(centerData)

# count methods and total number of samples
methods2 <- count_methodsBySite(centerData)

# check for sediment samples that have been coded into the QAQC database
qaqc2 <- check_qaqcDB(centerData, qa.db = '02')


#########################################
# any dataframe can be output as a csv or tab delimited file using write.table

# write unpaired TSS samples to a csv
write.table(unpairedTSS2, file = 'D:/ex_centerRev_unpairedTSS.csv', quote = FALSE, sep = ",",
            row.names = FALSE, col.names = TRUE)

# write verticals check to tab-delimited file
write.table(verticals2, file = 'D:/ex_centerRev_verticals.txt', quote = FALSE, sep = "\t",
            row.names = FALSE, col.names = TRUE)


