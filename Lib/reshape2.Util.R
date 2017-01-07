require(reshape2);
#
#
#

#' Title  MeltReshape()
#'
#' @param anyList 
#'
#' @return list of data.frame
#' @export TBD
#'
#' @examples TBD
MeltReshape <- function(anyList = list()){
  
  meltList <- anyList;
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.data.frame(listElement)){
      meltList[[nameElement]] <- as.data.frame(reshape2::melt(listElement));
    }else{
      meltList[[nameElement]] <- NULL;
    }
  }
  
  return(meltList);
}

