#' count_methodsBySite. Counts of samples by site, WY, sampling method, and sampler type.
#' 
#' @description Returns counts of samples by site, WY, sampling method, and sampler type.
#' @description Returns total counts of SSC(P80154), Bedload(P80225), Bedload mass (P91145), and the following specifics:
#' @description counts of SSC methode code (P82398)
#' @description counts of SSC sampler type (P84164)
#' @description counts of Bedload method code (P82398)
#' @description Note: Counts include rejected and no-results-reported samples. See \code{check_metaData}
#' for samples missing method or sampler coding.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @return A data.frame tabular summary of counts of samples by site, water year, and method
#' @examples
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' count_methodsBySiteOut <- count_methodsBySite(x)
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom reshape2 dcast
#' @export
#' @seealso \code{\link[sedReview]{check_metaData}}
count_methodsBySite <- function(x) {
  
  #Get only required columns
  x <- x[c("UID","RECORD_NO","SITE_NO","STATION_NM","PARM_CD","METH_CD","RESULT_VA","WY")]
  x <- unique(x)
  
  #Do basic summary of counts by site and WY
  sumBySite <- dplyr::summarise(dplyr::group_by(x,SITE_NO,STATION_NM, WY),
                           SSC_80154 = length(RECORD_NO[PARM_CD == "80154"]),
                           bedload_80225 = length(RECORD_NO[PARM_CD == "80225"]),
                           bedloadMass_91145 = length(RECORD_NO[PARM_CD == "91145"])
                           )
  
  ##############################
  #Summarize methods used for SSC
  
  ##Just get the results for the SSC parm code and SSC sampling method
  sscData <- x[x$PARM_CD %in% c("80154","82398"),]
  
  ##Group-wise filter of records that contain both SSC data (80154) AND a sampling method (82398)
  sscData <- dplyr::filter(dplyr::group_by(sscData,RECORD_NO),
                 all(c("80154","82398") %in% PARM_CD)
  )
  
  ##Ungroup the data for full filter
  sscData <- dplyr::ungroup(sscData)
  
  ##Filter to just the sampling method result
  sscData <- sscData[sscData$PARM_CD == "82398",]
  
  ##summarize counts grouped by site
  sumByMethod_SSC <- dplyr::summarise(dplyr::group_by(sscData,SITE_NO, WY, RESULT_VA),
                                      n = length(RESULT_VA))
  
  ##dast results for all methods by site and WY, rename columns
  if(nrow(sumByMethod_SSC)>0){
    sumByMethod_SSC <- reshape2::dcast(sumByMethod_SSC, SITE_NO + WY ~ RESULT_VA, value.var = 'n')
    names(sumByMethod_SSC) <- paste("SSC_method", names(sumByMethod_SSC), sep = "_")
  }else{
    warning("No SSC and Sample Method pairs")
    sumByMethod_SSC <- NULL}

  ##########################
  #Summarize sampler type used for SSC
  
  ##Just get the results for the SSC parm code and SSC sampler type
  sscData <- x[x$PARM_CD %in% c("80154","84164"),]
  
  ##Group-wise filter of records that contain both SSC data (80154) AND a sampler type (84164)
  sscData <- dplyr::filter(dplyr::group_by(sscData,UID),
                    all(c("80154","84164") %in% PARM_CD)
  )
  
  ##Ungroup the data for full filter
  sscData <- dplyr::ungroup(sscData)
  
  ##Filter to just the sampler type result
  sscData <- sscData[sscData$PARM_CD == "84164",]
  
  ##summarize counts grouped by site
  sumBySampler_SSC <- dplyr::summarise(dplyr::group_by(sscData,SITE_NO, WY, RESULT_VA),
                                       n = length(RESULT_VA))
  
  ##dast results for all methods by site and WY, rename columns
  if(nrow(sumBySampler_SSC)>0){
    sumBySampler_SSC <- reshape2::dcast(sumBySampler_SSC, SITE_NO + WY ~ RESULT_VA, value.var = 'n')
    names(sumBySampler_SSC) <- paste("SSC_sampler", names(sumBySampler_SSC), sep = "_")
  }else{
    warning("No SSC and Sampler Type pairs")
    sumBySampler_SSC <- NULL}
  
  ##############################
  #Summarize methods used for bedload
  
  ##Just get the results for the bedload parm code and bedload sampling method
  bedloadData <- x[x$PARM_CD %in% c("80225","82398"),]
  
  ##Group-wise filter of records that contain both SSC data (80225) AND a sampling method (82398)
  bedloadData <- dplyr::filter(dplyr::group_by(bedloadData,UID),
                    all(c("80225","82398") %in% PARM_CD))
  
  ##Ungroup the data for full filter
  bedloadData <- dplyr::ungroup(bedloadData)
  
  ##Filter to just the sampling method result
  bedloadData <- bedloadData[bedloadData$PARM_CD == "82398",]
  
  ##summarize counts grouped by site
  sumByMethod_bedload <- dplyr::summarise(dplyr::group_by(bedloadData,SITE_NO, WY, RESULT_VA),
                                          n = length(RESULT_VA))
  
  ##dast results for all methods by site and WY, rename columns
  if(nrow(sumByMethod_bedload)>0){
    sumByMethod_bedload <- reshape2::dcast(sumByMethod_bedload, SITE_NO + WY ~ RESULT_VA, value.var = 'n')
    names(sumByMethod_bedload) <- paste("bedload_method", names(sumByMethod_bedload), sep = "_")
  }else{
    warning("No bedload and Sample Method pairs")
    sumByMethod_bedload <- NULL}
  
  ###########################
  #Join all summaries together by site
  sumOut <- sumBySite
  if(!is.null(sumByMethod_SSC)){
    sumOut <- dplyr::left_join(sumOut,sumByMethod_SSC,by=c("SITE_NO"="SSC_method_SITE_NO", "WY"="SSC_method_WY"))}
  if(!is.null(sumBySampler_SSC)){
    sumOut <- dplyr::left_join(sumOut,sumBySampler_SSC,by=c("SITE_NO"="SSC_sampler_SITE_NO", "WY"="SSC_sampler_WY"))}
  if(!is.null(sumByMethod_bedload)){
    sumOut <- dplyr::left_join(sumOut,sumByMethod_bedload,by=c("SITE_NO"="bedload_method_SITE_NO", "WY"="bedload_method_WY"))}
  
  #Replace NAs with 0
  sumOut[is.na(sumOut)] <- 0
  # output only sites with a SED method count
  sumOut$methCount <- rowSums(sumOut[4:ncol(sumOut)])
  sumOut <- sumOut[sumOut$methCount > 0, 1:(ncol(sumOut)-1)]


  
  return(sumOut)
                             
}