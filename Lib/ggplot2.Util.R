# for ploting
require(ggplot2);
#
#
#

#' Title  DescribePlotList()
#'
#' @param anyList 
#'
#' @return list of ggplot2
#' @export TBD
#'
#' @examples TBD
DescribePlotList <- function(anyList = list()){
  
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

#' Title  DescribeMeltPlotList()
#'
#' @param anyList 
#'
#' @return list of ggplot2
#' @export TBD
#'
#' @examples TBD
DescribeMeltPlotList <- function(anyList = list()){
  
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

#' Title  DescribeAndProbabilityPlotList
#'
#' @param anyList 
#'
#' @return a plot list
#' @export TBD
#'
#' @examples TBD
DescribeAndProbabilityPlotList <- function(anyList = list()){
  
  plotList <- anyList;
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    aTitle <- paste0("GGPlot Probability distribution for ", nameElement);
    
    if(is.data.frame(listElement)){
      plotList[[nameElement]] <- ggplot(data = listElement, aes(x = `Latency (ms)`, y = `Throughput (mb/s)`, z = `Probability`)) +
        ggtitle(aTitle) +
        geom_point(size=2, colour="skyblue")+
        stat_density2d(color = "red");
    }#else{
    #  plotList[[nameElement]] <- ggplot(as.data.frame(listElement), aes(x=NoName)) +
    #    ggtitle(aTitle) +
    #    geom_histogram(binwidth = 1);
    #}
  }
  
  return(plotList);
}

DescribeOutlierPlotList <- function(anyList = list()){
  
  plotList <- anyList;
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.data.frame(listElement)){
      plotList[[nameElement]] <- ggplot(data = listElement, aes(x = `Latency (ms)`, y = `Throughput (mb/s)`)) +
        geom_point(aes(color = Outliers)) +
        ggtitle(paste0("Anomaly Detection of ", nameElement));
    }else{
      plotList[[nameElement]] <- NULL;
    }
  }
  
  return(plotList);
}

#' Title  ProbabilityListToDensityPlot
#'
#' @param probabilityList 
#'
#' @return density plot list
#' @export TBD
#'
#' @examples TBD
ProbabilityListToDensityPlot <- function(probabilityList = list()){
  
  plotList <- probabilityList;
  
  for (nameElement in names(probabilityList)) {
    listElement <- probabilityList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.data.frame(listElement)){
      plotList[[nameElement]] <- ggplot(data = listElement, aes(x = Probability)) +
        geom_density(fill = "skyblue") +
        ggtitle(paste0("Distibution of calculated probabilities of ", nameElement));
    }else{
      plotList[[nameElement]] <- NULL;
    }
  }
  
  return(plotList);
}

#' Title  ProbabilityListToBoxPlot
#'
#' @param probabilityList 
#'
#' @return boxplot list
#' @export TBD
#'
#' @examples TBD
ProbabilityListToBoxPlot <- function(probabilityList = list()){
  
  plotList <- probabilityList;
  
  for (nameElement in names(probabilityList)) {
    listElement <- probabilityList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.data.frame(listElement)){
      plotList[[nameElement]] <- ggplot(data = listElement, aes(y = Probability, x = 1)) +
        geom_boxplot(fill = "skyblue", colour = "steelblue4") +
        geom_jitter(colour = "grey50") +
        xlab('') +
        ggtitle(paste0("Box plot of calculated probabilities of ", nameElement));
    }else{
      plotList[[nameElement]] <- NULL;
    }
  }
  
  return(plotList);
}
