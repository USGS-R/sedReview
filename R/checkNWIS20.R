#' checkNWIS20
#' @description Function to perform NWIS 20.xx checks
#' @param x A \code{longTable} dataframe output from \code{getLocalNWIS}
#' @param returnAll logical, return dataframe containing all results or only return flagged samples. Defualt is FALSE
#' @examples 
#' data("testData",package="sedReview")
#' x <- testData
#' nwis20flags <- checkNWIS20(x)
#' 
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing all samples with applicable flags

# x is plotData from NWISodbc data pull
checkNWIS20 <- function(x, returnAll = FALSE)
{
  ### NWIS check 20.41 - unexpected medium code for bed sediment Pcode(s)
  # list of schedule 2420 Pcodes - note schedule 2420 not in LIMS anymore, older schedule
  sch2420list <- c("34950", "34970", "49267", "49269", "49266")
  sch2420 <- x[x$PARM_CD %in% sch2420list,]
  # flag samples with incorrect medium
  sch2420$check_20.41_sch2420[!(sch2420$MEDIUM_CD %in% c("SB ", "SBQ", "SC ", "SCQ"))] <- paste("flag medium_cd=", 
                                                                                              sch2420[!(sch2420$MEDIUM_CD %in% c("SB ", "SBQ", "SC ", "SCQ"))])
  sch2420 <- unique(sch2420[c("UID", "check_20.41_sch2420")])
  
  # list of schedule 2501 Pcodes
  sch2501list <- c("49319","49332","49338","49275","49339","49322","49320","49316",
                   "49349","49324","49331","49335","49341","49342","49343","49344",
                   "49345","49348","49325","49327","49329","49347","49318","49326",
                   "49328","49330","49346","49460","49459","99853","99824","49351","49321","49317","49350")
  sch2501 <- x[x$PARM_CD %in% sch2501list,]
  # flag samples with incorrect medium
  sch2501$check_20.41_sch2501[!(sch2501$MEDIUM_CD %in% c("SB ", "SBQ"))] <- paste("flag medium_cd=", 
                                                                                                sch2501$MEDIUM_CD[!(sch2501$MEDIUM_CD %in% c("SB ", "SBQ"))])
  sch2501 <- unique(sch2501[c("UID", "check_20.41_sch2501")])
  # list of schedule 2502 Pcodes
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
  sch2502 <- x[x$PARM_CD %in% sch2502list,]
  sch2502$check_20.41_sch2502[!(sch2502$MEDIUM_CD %in% c("SB ", "SBQ"))] <- paste("flag medium_cd=",
                                                                                                sch2502$MEDIUM_CD[!(sch2502$MEDIUM_CD %in% c("SB ", "SBQ"))])
  sch2502 <- unique(sch2502[c("UID", "check_20.41_sch2502")])
  
  ### NWIS check 20.42 - Medium code conflicts with site type
  # list of mediums for each site type
  STlist <- c("SS ","SSQ","WS ","WSQ","WI ","WIQ","SB ","SBQ","BA ","BAQ","BP ","BPQ","SO ","SOQ","BE ","BEQ",
              "BD ","BDQ","BY ","BYQ","BH ","BHQ","BI ","BIQ","WW ","WWQ","AA ","AAQ","WT ","WTQ","WH ","WHQ","OA ","OAQ")
  GWSBlist <- c("WG ","WGQ","AS ","ASQ","SO ","SOQ","SC ","SCQ","WI ","WIQ","WH ","WHQ","WT ","WTQ","OA ","OAQ")
  # extract and compare medium code with site types
  siteType <- x[,c("UID", "MEDIUM_CD","SITE_TP_CD")]
  siteType <- unique(siteType[c("UID", "MEDIUM_CD", "SITE_TP_CD")])
  siteType$check_20.42[siteType$SITE_TP_CD == "ST" & !(siteType$MEDIUM_CD %in% STlist)] <- 
    paste("flag medium_cd conflicts with site_tp_cd")
  siteType$check_20.42[siteType$SITE_TP_CD %in% c("GW","SB") & !(siteType$MEDIUM_CD %in% GWSBlist)] <- 
    paste("flag medium_cd conflicts with site_tp_cd")
  siteType <- siteType[,c("UID", "check_20.42")]
  
  ### NWIS check 20.43 - ENV sample with QC-Blank pcodes
  # list of ENV sample codes
  ENVlist <- c("WS ","WG ","WW ","WI ","WA ","WM ","WL ","WF ",
               "WU ","WE ","WD ","WT ","WB ","WH ","WC ","WP ",
               "SS ","SB ","ST ","SC ","SU ","SO ","SL ","SF ",
               "SD ","BA ","BP ","BH ","BY ","BE ","BI ","BD ",
               "AA ","AS ","OB ")
  blankPcodes <- x[x$PARM_CD %in% c("99100", "99101", "99102"),]
  blankPcodes$check_20.43[blankPcodes$MEDIUM_CD %in% ENVlist] <- paste("flag medium_cd=", 
                                                                       blankPcodes$MEDIUM_CD[blankPcodes$MEDIUM_CD %in% ENVlist])
  blankPcodes <- unique(blankPcodes[c("UID", "check_20.43")])
  
  ### NWIS check 20.46 - ENV sample with QC-spike pcodes
  spikePcodes <- x[x$PARM_CD %in% c("99106", "99107", "99108"),]
  spikePcodes$check_20.46[spikePcodes$MEDIUM_CD %in% ENVlist] <- paste("flag medium_cd=", 
                                                                       spikePcodes$MEDIUM_CD[spikePcodes$MEDIUM_CD %in% ENVlist])
  spikePcodes <- unique(spikePcodes[c("UID", "check_20.46")])
  
  ### NWIS check 20.47 - ENV sample with QC-reference-material or spike lot pcodes
  qcrefPcodes <- x[x$PARM_CD %in% c("99103", "99104", "99150", "99151", "99152", "99153", "99154"),]
  qcrefPcodes$check_20.47[qcrefPcodes$MEDIUM_CD %in% ENVlist] <- paste("flag medium_cd=", 
                                                                       qcrefPcodes$MEDIUM_CD[qcrefPcodes$MEDIUM_CD %in% ENVlist])
  qcrefPcodes <- unique(qcrefPcodes[c("UID", "check_20.47")])
  
  ### NWIS check 20.44 - ENV sample with non-ENV sample type
  ENVsampType <- c("A", "H", "5", "7", "9")
  nonENVsampType <- c("1", "2", "3", "4", "6", "8", "B", "K")
  check2044 <- x[x$MEDIUM_CD %in% ENVlist,]
  check2044$check_20.44[check2044$SAMP_TYPE_CD %in% nonENVsampType] <- paste("flag samp_type_cd=",
                                                                             check2044$SAMP_TYPE_CD[check2044$SAMP_TYPE_CD %in% nonENVsampType])
  check2044 <- unique(check2044[c("UID", "check_20.44")])
  
  ### NWIS check 20.45 - QC sample with illogical sample type
  check2045 <- x[x$MEDIUM_CD %in% c("OAQ", "WSQ", "WGQ"),]
  check2045$check_20.45[check2045$SAMP_TYPE_CD == 9] <- paste("flag samp_type_cd=", check2045$SAMP_TYPE_CD[check2045$SAMP_TYPE_CD == 9])
  check2045$check_20.45[check2045$MEDIUM_CD != "OAQ" & check2045$SAMP_TYPE_CD == "2"] <- paste("flag samp_type_cd=",
                                                                                               check2045$SAMP_TYPE_CD[check2045$MEDIUM_CD != "OAQ" 
                                                                                                                      & check2045$SAMP_TYPE_CD == "2"])
  check2045$check_20.45[check2045$MEDIUM_CD != "OAQ" & check2045$SAMP_TYPE_CD == "3"] <- paste("flag samp_type_cd=",
                                                                                               check2045$SAMP_TYPE_CD[check2045$MEDIUM_CD != "OAQ" 
                                                                                                                      & check2045$SAMP_TYPE_CD == "3"])
  check2045 <- unique(check2045[c("UID", "check_20.45")])
  
  ### NWIS check 20.61 - Missing sample purpose (P71999)
  sampPurp <- x[x$PARM_CD == "71999",]
  sampPurp <- unique(sampPurp$RECORD_NO)
  missingPurp <- x[!(x$RECORD_NO %in% sampPurp) & x$SAMPLE_START_DT > as.POSIXct("2001-09-30"),]
  missingPurp$check_20.61 <- paste("flag missing P71999 (Sample Purpose")
  missingPurp <- unique(missingPurp[c("UID", "check_20.61")])
  
  ### NWIS check 20.62 - SW or GW sample missing sampler-type (P84164)
  sampTyp <- x[x$PARM_CD == "84164",]
  sampTyp <- unique(sampTyp$RECORD_NO)
  missingTyp <- x[!(x$RECORD_NO %in% sampTyp) & x$MEDIUM_CD %in% c("WS ", "WSQ", "WG ", "WGQ") & x$SAMPLE_START_DT > as.POSIXct("2001-09-30"),]
  missingTyp$check_20.62 <- paste("flag missing P84164 (Sampler Type)")
  missingTyp <- unique(missingTyp[c("UID", "check_20.62")])
  
  ### NWIS check 20.63 - SW or GW sample missing sampling-method (P82398)
  sampMeth <- x[x$PARM_CD == "82398",]
  sampMeth <- unique(sampMeth$RECORD_NO)
  missingMeth <- x[!(x$RECORD_NO %in% sampMeth) & x$MEDIUM_CD %in% c("WS ", "WSQ", "WG ", "WGQ") & x$SAMPLE_START_DT > as.POSIXct("2001-09-30"),]
  missingMeth$check_20.63 <- paste("flag missing P82398 (Sampling Method)")
  missingMeth <- unique(missingMeth[c("UID", "check_20.63")])
  
  ### NWIS check 20.64 - ENV sample missing Type-of-QA associated with sample (P99111)
  typeQA <- x[x$PARM_CD == "99111",]
  typeQA <- unique(typeQA$RECORD_NO)
  missingTypQA <- x[!(x$RECORD_NO %in% typeQA) & x$MEDIUM_CD %in% ENVlist & x$SAMPLE_START_DT > as.POSIXct("2001-09-30"),]
  missingTypQA$check_20.64 <- paste("flag missing P99111 (Type-of-QA)")
  missingTypQA <- unique(missingTypQA[c("UID", "check_20.64")])
  
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT",
                               "MEDIUM_CD")])
  flaggedSamples <- dplyr::left_join(flaggedSamples, sch2420, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, sch2501, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, sch2502, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, siteType, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, blankPcodes, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, spikePcodes, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, qcrefPcodes, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, check2044, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, check2045, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, missingPurp, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, missingTyp, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, missingMeth, by = "UID")
  flaggedSamples <- dplyr::left_join(flaggedSamples, missingTypQA, by = "UID")
  flaggedSamples <- unique(flaggedSamples)
  
  if(returnAll == FALSE)
  {
    flaggedSamples <- flaggedSamples[is.na(flaggedSamples$check_20.41_sch2420)==FALSE |
                                       is.na(flaggedSamples$check_20.41_sch2501)==FALSE | 
                                       is.na(flaggedSamples$check_20.41_sch2502)==FALSE |
                                       is.na(flaggedSamples$check_20.42)==FALSE |
                                       is.na(flaggedSamples$check_20.43)==FALSE |
                                       is.na(flaggedSamples$check_20.46)==FALSE |
                                       is.na(flaggedSamples$check_20.47)==FALSE |
                                       is.na(flaggedSamples$check_20.44)==FALSE |
                                       is.na(flaggedSamples$check_20.45)==FALSE |
                                       is.na(flaggedSamples$check_20.61)==FALSE |
                                       is.na(flaggedSamples$check_20.62)==FALSE |
                                       is.na(flaggedSamples$check_20.63)==FALSE |
                                       is.na(flaggedSamples$check_20.64)==FALSE, ]
  }
  return(flaggedSamples)
}

