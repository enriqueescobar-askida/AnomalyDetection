# load Util
source(paste0("Lib/", projectName, ".Util.R"));
#
#
#
library(foreign);
# ggplot2 contains a dataset called diamonds. Make this dataset available using the data() function.
data(diamonds, package = "ggplot2");
# Find out what kind of object it is.
class(diamonds);
typeof(diamonds);
# Now investigate the structure of diamonds, a data frame with 53,940 observations
str(diamonds);
# Look at the dimension of the data frame.
dim(diamonds);
# names
names(diamonds);
# Create a random sample of the diamonds data.
diamondSample <- diamonds[sample(nrow(diamonds), 5000),];
dim(diamondSample);
# In this sample you use ggplot2.
TwoColumnDataFrameToPlot(diamondSample, 1, 7, "Diamond Sample");
# Add a log scale.
TwoColumnDataFrameToXLogPlot(diamondSample, 1, 7, "Diamond Sample");
# Add a log scale for both scales.
TwoColumnDataFrameToXYLogPlot(diamondSample, 1, 7, "Diamond Sample");
### Linear Regression in R
# Build the model. log of price explained by log of carat. This illustrates how linear regression works. Later we fit a model that includes the remaining variables
model <- TwoColumnDataFrameToLinearModel(diamondSample, 1, 7);
# Look at the results.
summary(model);
# R-squared = 0.9334, i.e. model explains 93.3% of variance
# Extract model coefficients.
stats::coefficients(model);
stats::coefficients(model)[1];
# exponentiate the log of price, to convert to original units
exp(stats::coefficients(model)[1]);
# Show the model in a plot.
TwoColumnDataFrameToXYLogLmPlot(diamondSample, 1, 7, "Diamond Sample");
### Regression Diagnostics 
# It is easy to get regression diagnostic plots.
# The same plot function that plots points either with a formula or
# with the coordinates also has a "method" for dealing with a model object.
# Look at some model diagnostics.
# check to see Q-Q plot to see linearity which means residuals are normally distributed
# Set up for multiple plots on the same figure.
par(mfrow = c(2, 2));
plot(model, col = "blue");
# Rest plot layout to single plot on a 1x1 grid
par(mfrow = c(1, 1));
### The Model Object 
# Finally, let's look at the model object. R packs everything that goes with the model, e.g. the formula and results into the object. You can pick out what you need by indexing into the model object.
str(model);
# note this is the same as coef(model)
model$coefficients;
# Now fit a new model including more columns
# Model log of price against all columns
model <- stats::lm(log(price) ~ log(carat) + ., data = diamonds);
summary(model);
# R-squared = 0.9824, i.e. model explains 98.2% of variance, i.e. a better model than previously
# Create data frame of actual and predicted price
# anti-log of predictions
diamondsActualVsPredicted <- data.frame(actual = diamonds$price, predicted = exp(stats::predict(model, diamonds)));
# Inspect predictions
head(diamondsActualVsPredicted);
# Create plot of actuals vs predictions
ColumnVersusColumnDataFrameToPlot(diamondsActualVsPredicted, "Diamonds Actual vs. Pedicted");
## Introduction to the ggplot2 plotting package
# The ggplot2 package is tremendously popular because it allows you to create
# beautiful plots by describing the plot structure.
# Install and load the packages.
#options(warn = -1)
#options(warn = 0)
library(ggplot2);
library(mapproj);
# In this example you use the dataset called quakes.
# This data contains locations of earthquakes off Fiji.
str(quakes);
head(quakes);
names(quakes);
# Set the font size so that it will be clearly legible.
ggplot2::theme_set(theme_gray(base_size = 18));
# Plot longitude and latitude of quakes.
# In this example, you map the column long to the x-axis and lat to the y-axis.
# Then you add a layer with points (geom_point) and a layer to plot maps.
PlotQuakes(quakeDF = quakes);
PlotQuakes(quakeDF = quakes, useHeat = TRUE);
PlotQuakes(quakeDF = quakes, useHeat = TRUE, useMagnitude = TRUE);
#
#
#
###name
twoNameList <- c("Latency (ms)", "Throughput (mb/s)");
oneNameList <- c("NoName");
# DATA1
# load("Data/data1.RData");
aList <- get(load("Data/data1.RData"));
rm(data1);
# screen a list
aList <- ScreenListToTibble(anyList = aList, twoNameList, oneNameList);
aPlotList <- DescribePlotList(aList);
meltList <- MeltReshape(aList);
meltPlotList <- DescribeMeltPlotList(meltList);
caretPreprocessList <- CaretPreprocessList(aList);
centeredList <- PredictPreprocessList(caretPreprocessList, aList);
probabilityList <- CenteredToProbabilityList(centeredList);
probabilityDensityPlotList <- ProbabilityListToDensityPlot(probabilityList);
probabilityBoxplotList <- ProbabilityListToBoxPlot(probabilityList);
aList <- ProbabilityListBind(aList, probabilityList);
aProbaPlotList <- DescribeAndProbabilityPlotList(aList);
aList <- ProbabilityListToOutliers(aList, frequencyMatrix = aList$yval);
# error in AES() DescribeOutlierPlotList(aList);
break;
# All the variables below are matrices
# X <- data1$X;
# X <- tibble::as_data_frame(as.data.frame(X));
# names(X) <- matrixNames;
# X %>% ggplot(aes(x = `Latency (ms)`, y = `Throughput (mb/s)`)) +
#  geom_point(color='blue');
# XX <- reshape2::melt(X);
# XX %>% ggplot(aes(x = value, fill = variable, color = variable)) +
#  geom_density(alpha = 0.3) +
#  ggtitle('Distibution of X');
# This is cross-validation data
# Xval <- data1$Xval;
# Xval <- tibble::as_data_frame(as.data.frame(Xval));
# names(Xval) <- matrixNames;
# Xval %>% ggplot(aes(x = `Latency (ms)`, y = `Throughput (mb/s)`)) +
#  geom_point(color = 'blue');
# XXval <- reshape2::melt(Xval);
# XXval %>% ggplot(aes(x = value, fill = variable, color = variable)) +
#  geom_density(alpha = 0.3) +
#  ggtitle('Distibution of Xval');
# This shows which rows in Xval are anomalous
# yval <- data1$yval;
# table(yval);

# Create preProcess object
# preObj <- caret::preProcess(X, method = "center");
# Center the data-subtract the column means from the data points
# X_centered <- stats::predict(preObj, X);
# X_centered <- as.matrix(X_centered);
# sigma2 <- diag(var(X_centered));
# sigma2 <- diag(sigma2);
# sigma2;

# A <- (2*pi)^(-ncol(X_centered) / 2) * det(sigma2)^(-0.5);
# B <- exp(-0.5 * rowSums((X_centered %*% ginv(sigma2)) * X_centered));
# p <- A*B;

# p <- p %>% as.data.frame();
# names(p) <- c('probability');
# DensityPlot
# p %>% ggplot(aes(probability)) +
#      geom_density(fill = 'skyblue') +
#      ggtitle('Distibution of calculated probabilities');
# BoxPlot
#p %>% ggplot(aes(y = probability, x = 1)) +
#  geom_boxplot() +
#  geom_jitter() +
#  xlab('') +
#  ggtitle("Box plot of calculated probabilities");
# X <- cbind(X, p);
# ggplot(X, aes(x = `Latency (ms)`, y = `Throughput (mb/s)`, z = `probability`)) +
#      geom_point(size=2, colour="skyblue")+
#      stat_density2d(color = 'red');
# Create preProcess object
# preObj <- caret::preProcess(Xval,method="center");
# Center the data- subtract the column means from the data points
# Xval_centered <- stats::predict(preObj, Xval);
# Xval_centered <- as.matrix(Xval_centered);
# sigma2 <- diag(var(Xval_centered));
# sigma2 <- diag(sigma2);
# sigma2;
# 
# A <- (2*pi)^(-ncol(Xval_centered) / 2) * det(sigma2)^(-0.5);
# B <- exp(-0.5 * rowSums((Xval_centered %*% ginv(sigma2)) * Xval_centered));
# pval <- A*B;
# 
# bestEpsilon <- 0;
# bestF1 <- 0;
# F1 <- 0;
# stepsize <- (max(pval) - min(pval)) / 1000;
# 
# for (epsilon in seq(from = min(pval), by = stepsize, to = max(pval))){
#   predictions <- (pval < epsilon) * 1;
#   tp <- sum((predictions == 1) & (yval == 1));
#   fp <- sum((predictions == 1) & (yval == 0));
#   fn <- sum((predictions == 0) & (yval == 1));
#   prec <- tp / (tp + fp);
#   rec <- tp / (tp + fn);
#   F1 <- (2 * prec * rec) / (prec + rec);
#   
#   if (!is.na(F1) & (F1 > bestF1)==TRUE){
#     bestF1 <- F1;
#     bestEpsilon <- epsilon;
#   }
# }
# 
# cat("\n bestF1 =",round(bestF1,4));
# cat("\n bestEpsilon =",round(bestEpsilon,4));
# 
# X$outliers <- X$probability < bestEpsilon;
# X$outliers <- as.factor(X$outliers);
# head(X, 2);
# 
# table(X$outliers);
# #
# X %>% ggplot(aes(x = `Latency (ms)`, y = `Throughput (mb/s)`)) +
#       geom_point(aes(color = outliers)) +
#       ggtitle('Anomaly Detection');

# DATA2
aList <- get(load("Data/data2.RData"));
rm(data2);
# screen a list
aList <- ScreenListToTibble(anyList = aList, twoNameList, oneNameList);
DescribeList(aList);
meltList <- MeltReshape(aList);
DescribeMeltList(meltList);
preProcessList <- CaretPreprocessList(aList);
centeredList <- PredictPreprocessList(preProcessList, aList);
probabilityList <- CenteredToProbabilityList(centeredList);
ProbabilityListToDensityPlot(probabilityList);
ProbabilityListToBoxPlot(probabilityList);
aList <- ProbabilityListBind(aList, probabilityList);
DescribeListAndProbability(aList);
aList <- ProbabilityListToOutliers(aList, frequencyMatrix = aList$yval);
#DescribeOutlierList(aList);






load("Data/data2.RData")

X<-data2$X         # has 11 columns
Xval<-data2$Xval   # This is cross-validation data
yval <- data2$yval  # This shows which rows in Xval are anomalous

X%>%as.data.frame()%>%melt()%>%
  ggplot(aes(x=value,color=variable))+geom_density()

cat("Number of variables and observations of X")
dim(X)

cat("Number of variables and observations of cross-validation data")
dim(Xval)

cat("Labels of cross-validation data")
cat("\ny=0 corresponds to normal servers and y=1 corresponds to anomalous servers")
dim(yval)

Xval%>%as.data.frame()%>%melt()%>%
  ggplot(aes(x=value,color=variable))+geom_density()

# Create preProcess object
X = as.data.frame(X)
preObj <- preProcess(X,method="center")
# Center the data- subtract the column means from the data points
X_centered <- predict(preObj,X)

sigma2<-cov(X_centered)
sigma2<-diag(sigma2)
sigma2<-diag(sigma2)

X_centered= as.matrix(X_centered)
A=(2*pi)^(-ncol(X_centered)/2)*det(sigma2)^(-0.5)
B = exp(-0.5 *rowSums((X_centered%*%ginv(sigma2))*X_centered))
p = A*B

# Create preProcess object
Xval = as.data.frame(Xval)
preObj <- preProcess(Xval,method="center")
# Center the data- subtract the column means from the data points
Xval_centered <- predict(preObj,Xval)

Xval_centered = as.matrix(Xval_centered)
sigma2<-diag(var(Xval_centered))
sigma2= diag(sigma2)

A=(2*pi)^(-ncol(Xval_centered)/2)*det(sigma2)^(-0.5)
B = exp(-0.5 *rowSums((Xval_centered%*%ginv(sigma2))*Xval_centered))
pval = A*B

bestEpsilon = 0
bestF1 = 0
F1 = 0
stepsize = (max(pval) - min(pval)) / 1000

for (epsilon in seq(from =min(pval), by= stepsize,to =max(pval))){
  predictions = (pval < epsilon)*1
  tp = sum((predictions == 1) & (yval == 1))
  fp = sum((predictions == 1) & (yval == 0))
  fn = sum((predictions == 0) & (yval == 1))
  prec = tp / (tp + fp)
  rec = tp / (tp + fn)
  F1 = (2 * prec * rec) / (prec + rec)
  
  if (!is.na(F1)& (F1 > bestF1)==TRUE){
    bestF1 = F1
    bestEpsilon = epsilon
  }
}

cat("\n bestF1 =",bestF1)
cat("\n bestEpsilon =",bestEpsilon)

probability<-p
X<-cbind(X,probability)
X$outliers= X$probability < bestEpsilon
X$outliers<-as.factor(X$outliers)

table(X$outliers)


