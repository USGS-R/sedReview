
library(sedReview)

# view active sediment sites from WY 2015-2017
COWSCsed <- count_activeSed(DSN = 'nwisco', begin.date = '2014-10-01', end.date = '2017-09-30')

# pull all qw data for Fountain Creek at Pueblo 07106500, WY2015-2017
fountain <- get_localNWIS(DSN = 'nwisco',
                          env.db = '01',
                          qa.db = '02',
                          STAIDS = c('07106500'),
                          begin.date = '2014-10-01',
                          end.date = '2017-09-30')

# calculate summary statistics
sumStats <- calc_summaryStats(fountain)

# lot of SSC samples in 2015, let's check that year out
fountain15 <- fountain[fountain$WY == 2015,]

# were any collected differently than the most common sampler type that year?
sampler15 <- check_samplerType(fountain15)

# were cross section verticals Okay?
verts15 <- check_verticals(fountain15) # no flags so all must be good, what about all three years?
verts <- check_verticals(fountain) # oh no, several samples with missing verticals in WY2017

# what sed samples are still in provisional?
prov <- find_provisional(fountain,
                         env.db = '01',
                         qa.db = '02')
# let's say I reviewed all those samples and they're ready to flip the DQI code,
# output the record numbers to a tab-delim file I can open and manipulate in excel for QWDATA. File name and path specified in function...
prov <- find_provisional(fountain,
                         env.db = '01',
                         qa.db = '02',
                         env.fileout = 'D:/Fountain_Creek_Provisional_SSC.txt',
                         view = FALSE)

# pull all data for Monument Creek blw Bijou St., Colo.Springs
monumentBijou <- get_localNWIS(DSN = 'nwisco',
                          env.db = '01',
                          qa.db = '02',
                          STAIDS = c('07104905'))
sumStatsMon <- calc_summaryStats(monumentBijou)

# make some plots
monumentTS <- plot_sedTS(monumentBijou)
monumentBox <- plot_ssctssBoxplot(monumentBijou)

# view a plot
monumentBox$combined
monumentTS$SSC

# output PDF docs to the D drive
plot_sedTS(monumentBijou, PDFout = "D:/Timeseries.pdf")
plot_ssctssBoxplot(monumentBijou, PDFout = "D:/Boxplots.pdf")












