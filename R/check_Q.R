#' check_Q. Searches for samples that were collected for sediment data but are missing discharge measurements
#' 
#' @description Searches for samples that were collected for sediment data but are missing discharge measurements.
#' Additional option flags Instantaneous Discharge (P00061) in NWIS that are >10\% different from Approved Unit Value discharge.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param includeUV Logical. If \code{x} was returned from \code{get_UVflow}. 
#' Run optional flagging for records with P00061 >10\% different from Approved UV flow. Default is \code{FALSE}.
#' @param returnAll Logical. Return dataframe containing all samples if \code{TRUE} or only return samples missing discharge if \code{FALSE}. Default is \code{FALSE}
#' @param reviewSummary logical, for center-level review, if \code{TRUE} a summary count of flags by site and water year is returned
#' instead of individual flagged samples.
#' @return A data.frame of samples, what sediment data are available, and then a flag for missing discharge along with what discharge parameter are present
#' @examples
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' checkQOut <- check_Q(x,includeUV = FALSE, returnAll = FALSE)
#' @importFrom dplyr left_join
#' @export
#' 


check_Q <- function(x, includeUV = FALSE, returnAll = FALSE, reviewSummary = FALSE) {
  # if Unit Values have been added, include that column, otherwise don't
  if(includeUV == TRUE){
    x <- x[c("UID","RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","WY","MEDIUM_CD","PARM_CD","PARM_NM","DQI_CD","RESULT_VA","UV_flow_cfs")]
  }else{
  x <- x[c("UID","RECORD_NO","SITE_NO","STATION_NM","SAMPLE_START_DT","WY","MEDIUM_CD","PARM_CD","PARM_NM","DQI_CD","RESULT_VA")]
  }
  x <- unique(x)
  
  sedRecords <- x[x$PARM_CD %in% c("80254","80154","80155","80225","00496","00535"),]
  qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208", "30209", "50042", "72137", "72243", "99060", "99061"),]
  
  if(nrow(sedRecords)==0){
    #print('Site contains no sediment specific records (Parameter codes: "80254","80154","80155","80225","00496","00535")')
    stop('Site contains no sediment specific records (Parameter codes: "80254","80154","80155","80225","00496","00535")')
  }
  
  # if Unit Values have been added, flag QW records with P00061 +/-10% diff from Approved UV
  if(includeUV == TRUE){
    #instQ <- qRecords[qRecords$PARM_CD == '00061' & !is.na(qRecords$UV_flow_cfs),]
    instQ <- qRecords[qRecords$PARM_CD == '00061',]
    
    instQ$UV_flag[(instQ$RESULT_VA - instQ$UV_flow_cfs) > (0.10*instQ$UV_flow_cfs) & !is.na(instQ$UV_flow_cfs)] <- paste("P00061",
                                                                                             instQ$RESULT_VA[(instQ$RESULT_VA - instQ$UV_flow_cfs) > (0.10*instQ$UV_flow_cfs)
                                                                                                             & !is.na(instQ$UV_flow_cfs)], 
                                                                                             "cfs >10% above Approved UV",
                                                                                             instQ$UV_flow_cfs[(instQ$RESULT_VA - instQ$UV_flow_cfs) > (0.10*instQ$UV_flow_cfs)
                                                                                                               & !is.na(instQ$UV_flow_cfs)], 
                                                                                             "cfs")
    
    instQ$UV_flag[(instQ$RESULT_VA - instQ$UV_flow_cfs) < -(0.10*instQ$UV_flow_cfs) & !is.na(instQ$UV_flow_cfs)] <- paste("P00061",
                                                                                              instQ$RESULT_VA[(instQ$RESULT_VA - instQ$UV_flow_cfs) < -(0.10*instQ$UV_flow_cfs)
                                                                                                              & !is.na(instQ$UV_flow_cfs)], 
                                                                                              "cfs >10% below Approved UV",
                                                                                              instQ$UV_flow_cfs[(instQ$RESULT_VA - instQ$UV_flow_cfs) < -(0.10*instQ$UV_flow_cfs)
                                                                                                                & !is.na(instQ$UV_flow_cfs)], 
                                                                                              "cfs")
    instQ$UV_flag[is.na(instQ$UV_flow_cfs)] <- "no Approved UV flow"

  }
  
  #not sure why Joe did it this way...
  # sedRecords <- reshape2::dcast(sedRecords,UID+RECORD_NO+SITE_NO+STATION_NM+SAMPLE_START_DT+MEDIUM_CD~PARM_CD,value.var="RESULT_VA")
  # qRecords <- reshape2::dcast(qRecords,UID~PARM_CD,value.var="RESULT_VA")
  
  # flag records that are missing associated discharge
  sedRecords$hasQ_flag <- ifelse(sedRecords$UID %in% qRecords$UID,TRUE,FALSE)
  sedRecords$hasQ_flag[sedRecords$hasQ_flag == TRUE] <- NA
  sedRecords$hasQ_flag[sedRecords$hasQ_flag == FALSE] <- 'Missing Q'
  
  # list of flagged samples
  ### data frame of all samples with flags
  flaggedSamples <- unique(x[c("UID",
                               "RECORD_NO",
                               "SITE_NO",
                               "STATION_NM",
                               "SAMPLE_START_DT","WY",
                               "MEDIUM_CD",
                               "PARM_CD",
                               "PARM_NM",
                               "RESULT_VA")])
  # append flags
  flaggedSamples <- dplyr::left_join(flaggedSamples, 
                                     sedRecords[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA", "hasQ_flag")], 
                                     by = c("UID", "PARM_CD", "PARM_NM","RESULT_VA"))
  if(includeUV == TRUE){
    flaggedSamples <- dplyr::left_join(flaggedSamples,
                                       instQ[c("UID", "PARM_CD", "PARM_NM", "RESULT_VA","UV_flag")],
                                       by = c("UID", "PARM_CD", "PARM_NM","RESULT_VA"))
  }
  if(returnAll == FALSE)
  {
    if(includeUV == TRUE){
      flaggedSamples <- flaggedSamples[is.na(flaggedSamples$hasQ_flag)==FALSE |
                                         is.na(flaggedSamples$UV_flag)==FALSE, ]
    }else{flaggedSamples <- flaggedSamples[is.na(flaggedSamples$hasQ_flag)==FALSE, ]}
  }
  
  # review summary
  if(reviewSummary == TRUE){
    flagSummary <- unique(x[c('SITE_NO',
                              'STATION_NM',
                              'WY')])
    
    if(includeUV == TRUE){
      #instQ$UV_flag[is.na(instQ$UV_flow_cfs)] <- paste('No Approved UV flow')
      instQ$sumUV_flag[is.na(instQ$UV_flow_cfs)] <- 1
      instQ$sumUV_flag[!is.na(instQ$UV_flag) & is.na(instQ$sumUV_flag)] <- -1
      
      flaggedSamples <- flaggedSamples[is.na(flaggedSamples$hasQ_flag)==FALSE |
                                         is.na(flaggedSamples$UV_flag)==FALSE, ]
      flaggedSamples <- dplyr::left_join(flaggedSamples, instQ[,c('UID','sumUV_flag')], by = 'UID')
      
      missingQ <- flaggedSamples[!is.na(flaggedSamples$hasQ_flag),]
      missingQ <- dplyr::summarise(dplyr::group_by(missingQ, SITE_NO, STATION_NM, WY),
                                   missing_Q = length(hasQ_flag))
      missingUV <- flaggedSamples[flaggedSamples$sumUV_flag == 1 & !is.na(flaggedSamples$sumUV_flag),]
      missingUV <- dplyr::summarise(dplyr::group_by(missingUV, SITE_NO, STATION_NM, WY),
                                    missing_App_UV = length(sumUV_flag))
      diffUV <- flaggedSamples[flaggedSamples$sumUV_flag == -1 & !is.na(flaggedSamples$sumUV_flag),]
      diffUV <- dplyr::summarise(dplyr::group_by(diffUV, SITE_NO, STATION_NM, WY),
                                 diff_10perc_from_App_UV = length(sumUV_flag))
      flagSummary <- dplyr::left_join(flagSummary, missingQ, by = c('SITE_NO','STATION_NM','WY'))
      flagSummary <- dplyr::left_join(flagSummary, missingUV, by = c('SITE_NO','STATION_NM','WY'))
      flagSummary <- dplyr::left_join(flagSummary, diffUV, by = c('SITE_NO','STATION_NM','WY'))
      
      flagSummary[is.na(flagSummary)] <- 0
      
      flagSummary <- flagSummary[flagSummary$missing_Q != 0 |
                                   flagSummary$missing_App_UV != 0 |
                                   flagSummary$diff_10perc_from_App_UV != 0,]
      
      return(flagSummary)
      
    }else{
      flaggedSamples <- flaggedSamples[is.na(flaggedSamples$hasQ_flag)==FALSE, ]
      missingQ <- flaggedSamples[!is.na(flaggedSamples$hasQ_flag),]
      missingQ <- dplyr::summarise(dplyr::group_by(missingQ, SITE_NO, STATION_NM, WY),
                                   missing_Q = length(hasQ_flag))
      flagSummary <- dplyr::left_join(flagSummary, missingQ, by = c('SITE_NO','STATION_NM','WY'))
      flagSummary[is.na(flagSummary)] <- 0
      
      flagSummary <- flagSummary[flagSummary$missing_Q != 0,]
      
      return(flagSummary)
    }
  }
  
  return(flaggedSamples)
}
