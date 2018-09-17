#' find_boxcoef. Find box coefficient pairs between SSC non-cross section/point samples and 
#' SSC cross section samples at a site.
#' @description Function to find box coefficient pairs between SSC point samples (pumped or grab) and SSC cross-section samples (EWI,EDI,EWT) at a site.
#' @param x A \code{dataframe} output from \code{get_localNWIS}
#' @param site_no Character of a site number in the x \code{dataframe} from \code{get_localNWIS} if x contains more than one site. Default is NULL.
#' @param timediff Number of hours to look before and after a point sample for a paired cross-section sample.
#' Default is 1 (ie. look for a paired samples 1 hour before and 1 hour after a non-cross section/point sample timestamp)
#' @param methods_NX Vector of Sampling Method (P82398) values that define non-cross section/point samples of the box coefficient.
#' Default is:\cr\cr methods_NX = c(30,40,50,55,70,100,900,920,930,940,4033,4080)\cr\cr 
#' 30 (single vertical), 40 (multiple verticals), 50 (point sample), 55 (composite - multiple point samples), 
#' 60 (weighted bottle), 70 (grab sample - dip), 100 (Van Dorn), 900 (SS pumping), 
#' 920 (SS BSV DI att), 930 (SS partial depth), 940 (SS partial width), 4033 (suction lift peristaltic), 4080 (peristaltic pump).
#' \cr\cr The option 'missing' can be added to include samples that have no associated sampling method for the record number,
#' or a blank sampling method value
#' @param methods_X Vector of Sampling Method (P82398) values that define cross section samples of the box coefficient.
#' Default is:\cr\cr methods_X = c(10,15,20)\cr\cr
#' 10 (EWI), 15 (multiple verticals non-isokinetic EWT), or 20 (EDI).
#' \cr\cr The option 'missing' can be added to include samples that have no associated sampling method for the record number,
#' or a blank sampling method value
#' @param includeUV Logical. If \code{x} was returned from \code{get_UVflow}, 
#' run optional addition of available Approved Unit Value discharge to output table. Default is \code{FALSE}.
#' @details Returns a dataframe of paired samples, flow values, and calculated box coefficient at given site_no for SSC (P80154). 
#' A summary count of all box coefficient pairs for all sites in
#' \code{x} can be found using the related function \code{summary_boxcoef}. 
#' @details The function finds non-cross section/point samples defined by \code{methods_NX} and all possible cross section pairs
#' within the time window (ex. a grab sample and 2 EWI passes (A and B passes) would yield 2 pairs. It is up to the user to
#' determine which box coefficients to utilize based on timing, flow conditions, and other considerations.
#' @details Rejected samples are not included.   
#' @examples 
#' data("exampleData",package="sedReview")
#' x <- exampleData
#' 
#' #find box coefficient sample pairs at site 05586300
#' boxcoef_05586300 <- find_boxcoef(x, site_no = "05586300")
#' 
#' #find box coefficient sample pairs at site 06934500 
#' #and expand paired sample window to +/- 2 hours from point sample
#' boxcoef_06934500 <- find_boxcoef(x, site_no = "06934500", timediff = 2)
#' @importFrom dplyr left_join
#' @export
#' @return A dataframe containing sample pairs with SSC result values of point and cross-section sample, with date/time stamps.
#' @seealso \code{\link[sedReview]{get_localNWIS}}, \code{\link[sedReview]{get_UVflow}}, \code{\link[sedReview]{summary_boxcoef}}

find_boxcoef <- function(x, site_no = NULL, timediff = 1,
                         methods_NX = c(30,40,50,55,70,100,900,920,930,940,4033,4080),
                         methods_X = c(10,15,20),
                         includeUV = FALSE){

  # remove rejected samples
  x <- x[!(x$DQI %in% c("Q","X")),]
  
  #Convert timediff to seconds
  timediff <- timediff * 60 * 60
  
  if(!(is.null(site_no))){
    site_no <- as.character(site_no)
    if((site_no %in% x$SITE_NO)==FALSE){
      stop("Site number not in input data.")
    }
    x <- x[x$SITE_NO == site_no,]
  }
  if(length(unique(x$SITE_NO))>1){
    stop("More than one site, please specify 'site_no'")}  
  
  methods <- setNames(c('single vertical', 'multiple verticals', 'point sample', 'composite - multiple point samples',
                        'weighted bottle', 'grab sample - dip', 'Van Dorn', 'SS pumping', 'SS BSV DI att', 'SS partial depth',
                        'SS partial width', 'suction lift peristaltic', 'peristaltic pump',
                        'EWI', 'multiple verticals non-isokinetic EWT', 'EDI'),
                      c(30, 40, 50, 55, 60, 70, 100, 900, 920, 930, 940, 4033, 4080,
                        10, 15, 20))
  x$method[x$PARM_CD == '82398'] <- methods[as.character(x$RESULT_VA[x$PARM_CD == '82398'])]

  
  #SSC samples
  SSC <- x[x$PARM_CD == "80154",c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT","RESULT_VA")]
  SSC <- unique(SSC)
  
  #All methods
  methodAll <- x[x$PARM_CD == "82398" & x$UID %in% SSC$UID, 
                 c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT","method")]
  methodAll$method[is.na(methodAll$method)] <- 'method missing'
  
  #SSC with no associated method
  noMethod <- SSC[!(SSC$UID %in% methodAll$UID),c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT")]
  if(nrow(noMethod) > 0){noMethod$method <- 'method missing'}
  else {noMethod$method <- NULL}
  
  #Samples with point or non-cross-section method and corresponds to an SSC sample
  methodNX <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% methods_NX 
                & x$UID %in% SSC$UID, 
                c("UID","SITE_NO","STATION_NM","SAMPLE_START_DT","method")]
  methodNX <- unique(methodNX)
  ##if 'missing' option is in methods_NX vector, add those rows
  if('missing' %in% methods_NX){
    methodNX <- rbind(methodNX, noMethod)
    methodNX <- rbind(methodNX, methodAll[methodAll$method == 'method missing',])
  }
  
  #Samples with EWI,EWT,EDI method and corresponds to an SSC sample. Call it x-section sample
  methodX <- x[x$PARM_CD == "82398" & x$RESULT_VA %in% methods_X & x$UID %in% SSC$UID, c("UID","SAMPLE_START_DT","method")]
  methodX <- unique(methodX)
  ##if 'missing' option is in methods_X vector, add those rows
  if('missing' %in% methods_X){
    methodX <- rbind(methodX, noMethod[c("UID","SAMPLE_START_DT","method")])
    methodX <- rbind(methodX, methodAll[methodAll$method == 'method missing',c("UID","SAMPLE_START_DT","method")])
  }
  
  
  # if no pairs possible, return empty dataframe
  if(nrow(methodNX) == 0 | nrow(methodX) == 0){
    sitePairs <- as.data.frame(matrix(nrow = 0,ncol = 13))
    names(sitePairs) <- c("SITE_NO","STATION_NM","UID_nonXS","SAMPLE_START_DT_nonXS","method_nonXS","RESULT_VA_nonXS",
                          "RESULT_VA_xsection","SAMPLE_START_DT_xsection","method_xsection","UID_xsection",
                          "QW_flow_cfs_nonXS","QW_flow_cfs_xsection","calc_box_coef")
    return(sitePairs)
  }
  
  #Internal function to find index of paired samples by input time difference. Time input in seconds via as.numeric(POSIXct)
  #Basis for function borrowed from smwrBase::mergeNearest
  ### NEEDS TO BE REFACTORED TO INCLUDE ALL PAIRS WITHIN TIME FRAME, TRY REMOVE MINDIFF AND WHICH(DIFF<=MAXDIFF)
  timeIndex <- function(time, pairTimes, maxdiff){
    diff <- abs(time - pairTimes)                  #get absolute value of difference between time and all times in compared dataset
    if(min(diff) > maxdiff){return(0)}               #if minimum difference > than max difference, no paired samples
    else{return(which(diff<= maxdiff))}        #if pairs, return index of first pair (not sure there'd be more than 1??)
  }
  
  #Indexes of methodNX/methodX pairs
  pairs <- sapply(as.numeric(methodNX$SAMPLE_START_DT), timeIndex,
                  pairTimes = as.numeric(methodX$SAMPLE_START_DT), maxdiff = timediff)
  if(length(pairs)==0){pairs <- 0}
  
  # expand methodNX to number of pairs per sample
  ## first get number of potential pairs
  for(i in 1:length(pairs)){
    methodNX$numpairs[i] <- length(pairs[[i]])
  };rm(i)
  ### next expand each row by number of potential pairs 
  ###(https://stackoverflow.com/questions/2894775/replicate-each-row-of-data-frame-and-specify-the-number-of-replications-for-each)
  methodNX <- methodNX[rep(seq_len(nrow(methodNX)), methodNX$numpairs), c('UID','SITE_NO','STATION_NM','SAMPLE_START_DT','method')]
  #### join the index number of which samples are paired
  methodNX$pairIndex <- unlist(pairs)
  ##### remove unpaired samples (pairIndex is 0)
  methodNX <- methodNX[methodNX$pairIndex != 0,]
  
  # join by pairs
  ## first add index column
  methodX$index <- seq.int(1, nrow(methodX), by = 1)
  ### join
  sitePairs <- dplyr::left_join(methodNX, methodX, by = c('pairIndex'='index'), suffix = c('_nonXS','_xsection'))
  #### remove the exact matches if 40 multi verts is selected from both method lists
  sitePairs <- sitePairs[sitePairs$UID_nonXS != sitePairs$UID_xsection,]
  
  #SSC samples for non-X-section and X-section
  sitePairs <- dplyr::left_join(sitePairs, SSC[,c("UID","RESULT_VA")],by = c("UID_nonXS" = "UID"))
  sitePairs <- dplyr::left_join(sitePairs, SSC[,c("UID","RESULT_VA")],by = c("UID_xsection" = "UID"), suffix = c('_nonXS','_xsection'))
  #re-order columns and drop index
  sitePairs <- sitePairs[,c("SITE_NO","STATION_NM","UID_nonXS","SAMPLE_START_DT_nonXS","method_nonXS","RESULT_VA_nonXS",
                            "RESULT_VA_xsection","SAMPLE_START_DT_xsection","method_xsection","UID_xsection")]

  #Get flow data
  qRecords <- x[x$PARM_CD %in%  c("00060", "00061", "30208"), c("UID","PARM_CD","RESULT_VA")]
  qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] <- qRecords$RESULT_VA[qRecords$PARM_CD == "30208"] * 35.3147 #cms to cfs
  qRecords <- qRecords[,c("UID","RESULT_VA")]
  names(qRecords) <- c("UID", "QW_flow_cfs")
  
  #Join flow metadata to pairs
  sitePairs <- dplyr::left_join(sitePairs,qRecords, by = c("UID_nonXS" = "UID"))
  sitePairs <- dplyr::left_join(sitePairs,qRecords, by = c("UID_xsection" = "UID"), suffix = c('_nonXS','_xsection'))
  
  # include Approved UV flow if included
  if(includeUV == TRUE){
    UVrecs <- unique(x[,c("UID","UV_flow_cfs")])
    sitePairs <- dplyr::left_join(sitePairs,UVrecs, by = c("UID_nonXS" = "UID"))
    sitePairs <- dplyr::left_join(sitePairs,UVrecs, by = c("UID_xsection" = "UID"), suffix = c('_nonXS','_xsection'))
  }
  
  #Calculate box coefficient
  sitePairs$calc_box_coef <- round((sitePairs$RESULT_VA_xsection / sitePairs$RESULT_VA_nonXS), digits = 2)
  
  return(sitePairs)
}

