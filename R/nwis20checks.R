
### NWIS check 20.41 - unexpected medium code for bed sediment Pcode(s)
# Need pcodes, not in LIMS anymore sch2420 <- 

#list of schedule 2501 Pcodes
sch2501list <- c("49319","49332","49338","49275","49339","49322","49320","49316",
                 "49349","49324","49331","49335","49341","49342","49343","49344",
                 "49345","49348","49325","49327","49329","49347","49318","49326",
                 "49328","49330","49346","49460","49459","99853","99824","49351","49321","49317","49350")
sch2501 <- subset(x, PARM_CD %in% sch2501list)
#flag samples with incorrect medium
sch2501$check_20.41_sch2501[sch2501$MEDIUM_CD != "SB " & sch2501$MEDIUM_CD != "SBQ"] <- paste("flag medium_cd=", 
                                                                                              sch2501$MEDIUM_CD[sch2501$MEDIUM_CD != "SB " & sch2501$MEDIUM_CD != "SBQ"])
#list of schedule 2502 Pcodes
sch2502list <- c("49438","49439","49403","49441","49442","49404","49398","49410",
                 "49388","49391","49405","49415","49416","49417","49418","49395",
                 "49406","49396","49407","49467","49948","49279","49435","49420",
                 "49421","49419","49454","49422","49455","49423","49411","49429",
                 "49428","49430","49434","49437","49443","49436","49389","49458",
                 "49468","49408","49397","49457","49401","49456","49426","49427",
                 "49424","49449","49450","49381","49382","49461","49452","49383",
                 "49384","49466","49399","49343","49448","49489","49453","49390",
                 "49400","49394","49431","49433","49402","49444","49280","49451",
                 "49460","49446","49425","49409","49393","49413","49387","49392","99854","99825","49278")
sch2502 <- subset(x, PARM_CD %in% sch2502list)
sch2502$check_20.41_sch2502[sch2502$MEDIUM_CD != "SB " & sch2502$MEDIUM_CD != "SBQ"] <- paste("flag medium_cd=",
                                                                                              sch2502$MEDIUM_CD[sch2502$MEDIUM_CD != "SB " & sch2502$MEDIUM_CD != "SBQ"])


### NWIS check 20.42 - Medium code conflicts with site type
#need readNWISodbc to pull the site type information from nwis

### NWIS check 20.43 - ENV sample with QC-Blank pcodes
#list of ENV sample codes
ENVlist <- c("WS ","WG ","WW ","WI ","WA ","WM ","WL ","WF ",
             "WU ","WE ","WD ","WT ","WB ","WH ","WC ","WP ",
             "SS ","SB ","ST ","SC ","SU ","SO ","SL ","SF ",
             "SD ","BA ","BP ","BH ","BY ","BE ","BI ","BD ",
             "AA ","AS ","OB ")
blankPcodes <- subset(x, PARM_CD %in% c("99100", "99101", "99102"))
blankPcodes$check_20.43[blankPcodes$MEDIUM_CD %in% ENVlist] <- paste("flag medium_cd=", 
                                                                                  blankPcodes$MEDIUM_CD[blankPcodes$MEDIUM_CD %in% ENVlist])

### NWIS check 20.46 - ENV sample with QC-spike pcodes
spikePcodes <- subset(x, PARM_CD %in% c("99106", "99107", "99108"))
spikePcodes$check_20.46[spikePcodes$MEDIUM_CD %in% ENVlist] <- paste("flag medium_cd=", 
                                                                                  spikePcodes$MEDIUM_CD[spikePcodes$MEDIUM_CD %in% ENVlist])

### NWIS check 20.47 - ENV sample with QC-reference-material or spike lot pcodes
qcrefPcodes <- subset(x, PARM_CD %in% c("99103", "99104", "99150", "99151", "99152", "99153", "99154"))
qcrefPcodes$check_20.47[qcrefPcodes$MEDIUM_CD %in% ENVlist] <- paste("flag medium_cd=", 
                                                                                  qcrefPcodes$MEDIUM_CD[qcrefPcodes$MEDIUM_CD %in% ENVlist])
### NWIS check 20.44 - ENV sample with non-ENV sample type
ENVsampType <- c("A", "H", "5", "7", "9")
nonENVsampType <- c("1", "2", "3", "4", "6", "8", "B", "K")
check2044 <- subset(x, MEDIUM_CD %in% ENVlist)
check2044$check_20.44[check2044$SAMP_TYPE_CD %in% nonENVsampType] <- paste("flag samp_type_cd=",
                                                                           check2044$SAMP_TYPE_CD[check2044$SAMP_TYPE_CD %in% nonENVsampType])
### NWIS check 20.45 - QC sample with illogical sample type
check2045 <- subset(x, MEDIUM_CD %in% c("OAQ", "WSQ", "WGQ"))
check2045$check_20.45[check2045$SAMP_TYPE_CD == 9] <- paste("flag samp_type_cd=", check2045$SAMP_TYPE_CD[check2045$SAMP_TYPE_CD == 9])
check2045$check_20.45[check2045$MEDIUM_CD != "OAQ" & check2045$SAMP_TYPE_CD == "2"] <- paste("flag samp_type_cd=",
                                                                                             check2045$SAMP_TYPE_CD[check2045$MEDIUM_CD != "OAQ" 
                                                                                                                    & check2045$SAMP_TYPE_CD == "2"])
check2045$check_20.45[check2045$MEDIUM_CD != "OAQ" & check2045$SAMP_TYPE_CD == "3"] <- paste("flag samp_type_cd=",
                                                                                             check2045$SAMP_TYPE_CD[check2045$MEDIUM_CD != "OAQ" 
                                                                                                                    & check2045$SAMP_TYPE_CD == "3"])
