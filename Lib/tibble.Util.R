# for data frame manipulation
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

#' Title ProbabilityListBind
#'
#' @param anyList 
#' @param probabilityList 
#'
#' @return tibble list
#' @export
#'
#' @examples
ProbabilityListBind <- function(anyList = list(), probabilityList = list()){
  
  for (nameElement in names(probabilityList)) {
    listElement <- probabilityList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    anyElement <- anyList[[nameElement]];
    
    if(is.data.frame(listElement)){
      anyElement <- cbind(anyElement, listElement);
    }else{
      anyElement <- cbind(anyElement, data.frame(NULL));
    }
    
    anyList[[nameElement]] <- tibble::as_data_frame(anyElement);
  }
  
  return(anyList);
}
