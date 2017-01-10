# to use piping %>%
library(magrittr);
# for ploting
library(ggplot2);
# to calculate the pseudo-inverse of a matrix
library(MASS)
# to center our data by subtracting its mean
library(caret);
# for data manipulation
library(reshape2);
#
#
#
# import stats.Util.R
myLibrary <- "Lib/stats.Util.R";
write(paste0(c("Load Util........\t", myLibrary), sep = "", collapse = ""), stdout());
source(myLibrary);
rm(myLibrary);
# import tibble.Util.R
myLibrary <- "Lib/tibble.Util.R";
write(paste0(c("Load Util........\t", myLibrary), sep = "", collapse = ""), stdout());
source(myLibrary);
rm(myLibrary);
# import ggplot2.Util.R
myLibrary <- "Lib/ggplot2.Util.R";
write(paste0(c("Load Util........\t", myLibrary), sep = "", collapse = ""), stdout());
source(myLibrary);
rm(myLibrary);
# import reshape2.Util.R
myLibrary <- "Lib/reshape2.Util.R";
write(paste0(c("Load Util........\t", myLibrary), sep = "", collapse = ""), stdout());
source(myLibrary);
rm(myLibrary);
# import caret.Util.R
myLibrary <- "Lib/caret.Util.R";
write(paste0(c("Load Util........\t", myLibrary), sep = "", collapse = ""), stdout());
source(myLibrary);
rm(myLibrary);
#
#
#
ScreenListToTibble <- function(anyList = list(),
                       multiNameList = c("", ""),
                       singleNameList = NULL){
  
  for (nameElement in names(anyList)) {
    listElement <- anyList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    if(is.matrix(listElement)){
      listElement <- ScreenMatrix(listElement,
                                   nameElement,
                                   multiNameList,
                                   singleNameList);
    }
    anyList[[nameElement]] <- listElement;
  }
  
  return(anyList);
}
#
#
#
PredictPreprocessList <- function(preProcessList = list(), aList = list()){
  
  centerList <- list();
  
  for (nameElement in names(preProcessList)) {
    listElement <- preProcessList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    aElement <- aList[[nameElement]];
    
    centerList[[nameElement]] <- as.matrix(stats::predict(listElement, aElement));
  }
  
  return(centerList);
}
#
###
#
CenteredToProbabilityList <- function(centeredList = list()){
  
  probaList <- list();
  
  for (nameElement in names(centeredList)) {
    listElement <- centeredList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    
    probaList[[nameElement]] <- diag(var(listElement));
    probaList[[nameElement]] <- diag(probaList[[nameElement]]);
    
    A <- (2*pi)^(-ncol(listElement) / 2) * det(probaList[[nameElement]])^(-0.5);
    B <- exp(-0.5 * rowSums((listElement %*% ginv(probaList[[nameElement]])) * listElement));
    p <- as.data.frame(A*B);
    names(p) <- c("Probability");
    p <- tibble::as_data_frame(p);
    probaList[[nameElement]] <- p;
  }
  
  return(probaList);
}
#
#
#
ProbabilityListToOutliers <- function(aList = list(), frequencyMatrix = matrix(ncol = 1)){
  
  for (nameElement in names(aList)) {
    listElement <- aList[[nameElement]];
    cat("@", nameElement, "\t", class(listElement), "\t", typeof(listElement), "\n");
    print(head(listElement, 1));
    nbCol <- ncol(listElement);
    
    if(nbCol <= 2){
      
      if(is.data.frame(listElement)){
        bestEpsilon <- 0;
        bestF1 <- 0;
        F1 <- 0;
        probabilities <- aList[[nameElement]]$Probability;
        maxProb <- max(probabilities);
        minProb <- min(probabilities);
        stepSize <- (maxProb - minProb) / 1000;
        
        for (epsilon in seq(from = minProb, by = stepSize, to = maxProb)){
          predictions <- (probabilities < epsilon) * 1;
          truePositive <- sum((predictions == 1) & (frequencyMatrix == 1));
          falsePositive <- sum((predictions == 1) & (frequencyMatrix == 0));
          falseNegative <- sum((predictions == 0) & (frequencyMatrix == 1));
          trueNegative <- sum((predictions == 0) & (frequencyMatrix == 0));
          #sensitivity or true positive rate (TPR)
          rec <- truePositive / (truePositive + falseNegative);
          #specificity (SPC) or true negative rate
          TNR <- trueNegative / (trueNegative + falsePositive);
          #precision or positive predictive value (PPV)
          prec <- truePositive / (truePositive + falsePositive);
          #negative predictive value (NPV)
          NPV <- trueNegative / (trueNegative + falsePositive);
          #fall-out or false positive rate (FPR)
          FPR <- falsePositive / (falsePositive + trueNegative);
          #false negative rate (FNR)
          FNR <- falseNegative / (truePositive + falseNegative);
          #false discovery rate (FDR)
          FDR <- falsePositive / (truePositive + falsePositive);
          #accuracy (ACC)
          ACC <- (truePositive + trueNegative) / (truePositive + falsePositive + falseNegative + trueNegative);
          #F1 score = 2*TP / (2TP + FP + FN)
          F1score <- 2 * truePositive / (2 * truePositive + falsePositive + falseNegative);
          F1 <- (2 * prec * rec) / (prec + rec);
          #Matthews correlation coefficient (MCC)
          MCC <- (truePositive * trueNegative - falsePositive * falseNegative) /
            sqrt((truePositive + falsePositive)*(truePositive + falseNegative)*(trueNegative + falsePositive)*(trueNegative + falseNegative));
          Informedness <- rec + TNR - 1;
          Markedness <- prec + NPV - 1;
          
          if (!is.na(F1) & (F1 > bestF1)==TRUE){
            bestF1 <- F1;
            bestEpsilon <- epsilon;
          }
        }
        
        cat("\nbestF1 = ", round(bestF1,4), "\n");
        cat("bestEpsilon = ", round(bestEpsilon,4), "\n\n");
        
        probabilitiesWithBestEpsilon <- aList[[nameElement]]$Probability < bestEpsilon;
        aList[[nameElement]]$Outliers <- as.factor(probabilitiesWithBestEpsilon);
      }
    }
  }
  
  return(aList);
}

