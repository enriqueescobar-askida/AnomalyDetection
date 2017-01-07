require(ggplot2);
#
#
#

#' Title  DescribeList()
#'
#' @param anyList 
#'
#' @return list of ggplot2
#' @export TBD
#'
#' @examples TBD
DescribeList <- function(anyList = list()){
  
  plotList <- anyList;
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    aTitle <- paste0("GGPlot for ", nameElement);
    nbCol <- ncol(listElement);
    
    if(nbCol <= 2){
      if(is.data.frame(listElement)){
        plotList[[nameElement]] <- ggplot(data = listElement, aes(`Latency (ms)`, `Throughput (mb/s)`)) +
          ggtitle(aTitle) +
          geom_point(color = "skyblue");
      }else{ #barplot(listElement);
        plotList[[nameElement]] <- ggplot(as.data.frame(listElement), aes(x=NoName)) +
          ggtitle(aTitle) +
          geom_histogram(binwidth = 1);
      }
    }
  }
  
  return(plotList);
}

#' Title  DescribeMeltList()
#'
#' @param anyList 
#'
#' @return list of ggplot2
#' @export TBD
#'
#' @examples TBD
DescribeMeltList <- function(anyList = list()){
  
  plotList <- anyList;
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.data.frame(listElement)){
      plotList[[nameElement]] <- ggplot(data = listElement, aes(x = value, fill = variable, color = variable)) +
        geom_density(alpha = 0.3) +
        ggtitle(paste0("Distibution of ", nameElement));
    }else{
      plotList[[nameElement]] <- NULL;
    }
  }
  
  return(plotList);
}

