
# NWIS check 20.41 - unexpected medium code for bed sediment Pcode(s)
# Need pcodes, not in LIMS anymore sch2420 <- 

sch2501 <- subset(x, PARM_CD %in% c("49319","49332","49338","49275","49339","49322","49320","49316",
                                    "49349","49324","49331","49335","49341","49342","49343","49344",
                                    "49345","49348","49325","49327","49329","49347","49318","49326",
                                    "49328","49330","49346","49460","49459","99853","99824","49351","49321","49317","49350"))
sch2501$check_20.41_sch2501[sch2501$MEDIUM_CD != "SB " & sch2501$MEDIUM_CD != "SBQ"] <- paste("flag medium_cd=", sch2501$MEDIUM_CD)

sch2502 <- subset(x, PARM_CD %in% c("49438","49439","49403","49441","49442","49404","49398","49410",
                                    "49388","49391","49405","49415","49416","49417","49418","49395",
                                    "49406","49396","49407","49467","49948","49279","49435","49420",
                                    "49421","49419","49454","49422","49455","49423","49411","49429",
                                    "49428","49430","49434","49437","49443","49436","49389","49458",
                                    "49468","49408","49397","49457","49401","49456","49426","49427",
                                    "49424","49449","49450","49381","49382","49461","49452","49383",
                                    "49384","49466","49399","49343","49448","49489","49453","49390",
                                    "49400","49394","49431","49433","49402","49444","49280","49451",
                                    "49460","49446","49425","49409","49393","49413","49387","49392","99854","99825","49278"))
sch2502$check_20.41_sch2502[sch2502$MEDIUM_CD != "SB " & sch2502$MEDIUM_CD != "SBQ"] <- paste("flag medium_cd=", sch2502$MEDIUM_CD)


# NWIS check 20.42 - Medium code conflicts with site type
#need readNWISodbc to pull the site type information from nwis

# NWIS check 20.43 - ENV sample with QC-Blank pcodes
blankPcodes <- subset(x, PARM_CD %in% c("99100", "99101", "99102"))
blankPcodes$check_20.43[blankPcodes$MEDIUM_CD %in% c("WS ","WG ","WW ","WI ","WA ","WM ","WL ","WF ",
                                                     "WU ","WE ","WD ","WT ","WB ","WH ","WC ","WP ",
                                                     "SS ","SB ","ST ","SC ","SU ","SO ","SL ","SF ",
                                                     "SD ","BA ","BP ","BH ","BY ","BE ","BI ","BD ",
                                                     "AA ","AS ","OB ")] <- paste("flag medium_cd=", blankPcodes$MEDIUM_CD)

#NWIS check 20.46

#NWIS check 20.47


