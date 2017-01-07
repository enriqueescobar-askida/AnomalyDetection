# to center our data by subtracting its mean
require(caret);
#
#
#

#' Title  CaretPreprocessList
#'
#' @param anyList 
#'
#' @return list
#' @export TBD
#'
#' @examples TBD
CaretPreprocessList <- function(anyList = list()){
  
  caretList <- anyList;
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.data.frame(listElement)){
      caretList[[nameElement]] <- caret::preProcess(listElement, method = "center");
    }else{
      caretList[[nameElement]] <- NULL;
    }
  }
  
  return(caretList);
}
