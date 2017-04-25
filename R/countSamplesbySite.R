# Master queries:
#   Ability to retrieve total number of samples and names of sites sampled for:
#   SSC (80154)
# Bedload (80225)
# Bedload mass (91145)
# Ability to retrieve counts (number of samples) on sample method code, particularly:
#   If SSC (80154) value is present:
#   Sample method code (82398) = 10, 20, or 60 (provide counts for each code)
# Sampler type (84164) = 3060, 3070, 3071 (provide counts for each code)
# If bedload (80225) value is present:
#   Sample method code (82398) = 1000, 1010, 1020 (provide counts for each code)

#' countSamplesbySite
#' 
#' @description Returns counts of samples by site, sampling method, and sampler type. See details
#' @param x A list generated from \code{getLocalNWIS}
#' @return A data.frame tabular summary of counts of samples by site and method
#' @examples
#' x <- data("exampleData",package="sedReview")
#' countSamplesbySite(x)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
countSamplesbySite <- function(x) {
  
  #Get only required columns
  x <- x[c("RECORD_NO","SITE_NO","STATION_NM","PARM_CD","METH_CD","RESULT_VA")]
  x <- unique(x)
  
  #Do basic summary of counts by site
  sumBySite <- dplyr::summarise(dplyr::group_by(x,SITE_NO,STATION_NM),
                           SSC_80154 = length(RECORD_NO[PARM_CD == "80154"]),
                           bedload_80225 = length(RECORD_NO[PARM_CD == "80225"]),
                           bedloadMass_91145 = length(RECORD_NO[PARM_CD == "91145"])
                           )
  
  ##############################
  #Summarize methods used for SSC
  
  ##Just get the results for teh SSC parm code and SSC sampling method
  sscData <- x[x$PARM_CD %in% c("80154","82398"),]
  
  ##Group-wise filter of records that contain both SSC data (80154) AND a sampling method (82398)
  sscData <- filter(group_by(sscData,RECORD_NO),
                 all(c("80154","82398") %in% PARM_CD)
  )
  
  ##Ungroup the data for full filter
  sscData <- ungroup(sscData)
  
  ##Filter to just the sampling method result
  sscData <- sscData[sscData$PARM_CD == "82398",]
  
  ##summarize counts grouped by site
  sumByMethod_SSC <- summarise(group_by(sscData,SITE_NO),
                           SSC_method_10 = length(RESULT_VA[RESULT_VA == 10]),
                           SSC_method_20 = length(RESULT_VA[RESULT_VA == 20]),
                           SSC_method_60 = length(RESULT_VA[RESULT_VA == 60])
                           )
  
  ##Check for no data and fill with NAs
  if(nrow(sumByMethod_SSC) == 0) {
    sumByMethod_SSC <- data.frame(SITE_NO = unique(x$SITE_NO),
                                  SSC_method_10 = NA,
                                  SSC_method_20 = NA,
                                  SSC_method_60 = NA,
                                  stringsAsFactors = F)
  }
  
  ##########################
  #Summarize sampler type used for SSC
  
  ##Just get the results for teh SSC parm code and SSC sampling method
  sscData <- x[x$PARM_CD %in% c("80154","84164"),]
  
  ##Group-wise filter of records that contain both SSC data (80154) AND a sampler type (84164)
  sscData <- filter(group_by(sscData,RECORD_NO),
                    all(c("80154","84164") %in% PARM_CD)
  )
  
  ##Ungroup the data for full filter
  sscData <- ungroup(sscData)
  
  ##Filter to just the sampler type result
  sscData <- sscData[sscData$PARM_CD == "84164",]
  
  ##summarize counts grouped by site
  sumBySampler_SSC <- summarise(group_by(sscData,SITE_NO),
                           SSC_sampler_3060 = length(RESULT_VA[RESULT_VA == 3060]),
                           SSC_sampler_3070 = length(RESULT_VA[RESULT_VA == 3070]),
                           SSC_sampler_3071 = length(RESULT_VA[RESULT_VA == 3071])
  )
  
  ##Check for no data and fill with NAs
  if(nrow(sumBySampler_SSC) == 0) {
    sumBySampler_SSC <- data.frame(SITE_NO = unique(x$SITE_NO),
                                   SSC_sampler_3060 = NA,
                                   SSC_sampler_3070 = NA,
                                   SSC_sampler_3071 = NA,
                                   stringsAsFactors = F)
  }
  
  ##############################
  #Summarize methods used for bedload
  
  ##Just get the results for teh SSC parm code and SSC sampling method
  bedloadData <- x[x$PARM_CD %in% c("80225","82398"),]
  
  ##Group-wise filter of records that contain both SSC data (80225) AND a sampling method (82398)
  bedloadData <- filter(group_by(bedloadData,RECORD_NO),
                    all(c("80225","82398") %in% PARM_CD)
  )
  
  ##Ungroup the data for full filter
  bedloadData <- ungroup(bedloadData)
  
  ##Filter to just the sampling method result
  bedloadData <- bedloadData[bedloadData$PARM_CD == "82398",]
  
  ##summarize counts grouped by site
  sumByMethod_bedload <- summarise(group_by(bedloadData,SITE_NO),
                           bedload_method_1000 = length(RESULT_VA[RESULT_VA == 1000]),
                           bedload_method_1010 = length(RESULT_VA[RESULT_VA == 1010]),
                           bedload_method_1020 = length(RESULT_VA[RESULT_VA == 1020])
  )
  
  ##Check for no data and fill with NAs
  if(nrow(sumByMethod_bedload) == 0) {
    sumByMethod_bedload <- data.frame(SITE_NO = unique(x$SITE_NO),
                                      bedload_method_1000 = NA,
                                      bedload_method_1010 = NA,
                                      bedload_method_1020 = NA,
                                      stringsAsFactors = F)
    }
  
  
  ###########################
  #Join all summaries together by site
  
  sumOut <- dplyr::left_join(sumBySite,sumByMethod_SSC,by="SITE_NO")
  sumOut <- dplyr::left_join(sumOut,sumBySampler_SSC,by="SITE_NO")
  sumOut <- dplyr::left_join(sumOut,sumByMethod_bedload,by="SITE_NO")


  
  return(sumOut)
                             
}