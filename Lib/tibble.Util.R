require(tibble);
#
#
#

#' Title  ScreenMatrix()
#'
#' @param anyMatrix 
#' @param anyName 
#' @param multiNameList 
#' @param singleNameList 
#'
#' @return tibble
#' @export TBD
#'
#' @examples TBD
ScreenMatrix <- function(anyMatrix = matrix(NULL),
                         anyName = "",
                         multiNameList = NULL,
                         singleNameList = NULL){
  print(paste0("ScreenMatrix .....\t", anyName));
  aTibble <- NULL;
  nbCol <- ncol(anyMatrix);
  nbName <- length(multiNameList);
  
  if(nbCol >= 2){
    aTibble <- tibble::as_data_frame(anyMatrix);
  }else{
    aTibble <- anyMatrix;
    colnames(aTibble) <- singleNameList;
  }
  
  if(nbCol == nbName){
    colnames(aTibble) <- multiNameList;
  }
  
  return(aTibble);
}

