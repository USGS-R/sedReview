# Sand/Silt break differs by more than x%
# possibly could be combined with sedOutliers, does similar task

sandSiltBreak <- function(x){
  ssbreak <- x[x$PARM_CD == 70331, ]
  quantiles <- data.frame(quantile = seq(0, 1, 0.1))
  quantiles$ssbreak <- quantile(ssbreak$RESULT_VA, probs = seq(0, 1, 0.1))
  
  if(nrow(ssbreak) > 0){
    title <- paste(unique(ssbreak$PARM_CD), unique(ssbreak$PARM_NM), sep = " ")
    boxplot(ssbreak$RESULT_VA)
    title(title)
  } else{}
  
  return(quantiles)
}