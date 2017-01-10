#' Title  TwoColumnDataFrameToLinearModel
#'
#' @param aDataFrame 
#' @param columnX 
#' @param columnY 
#'
#' @return lm
#' @export TBD
#'
#' @examples TBD
TwoColumnDataFrameToLinearModel <- function(aDataFrame = NULL
                                     , columnX = -1
                                     , columnY = -1){
  
  if (is.null(aDataFrame)) {
    
    return(NULL);
  } else {
    linearModel <- NULL;
    # lm
    linearModel <- stats::lm(log(aDataFrame[[columnY]]) ~ log(aDataFrame[[columnX]]) , data = aDataFrame);
    
    return(linearModel);
  }
}

