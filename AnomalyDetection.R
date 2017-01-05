# env var
gDriveHome <- gsub("\\\\", "/", Sys.getenv("GDrive"));
write(paste0(c("Environment.....\t", gDriveHome), sep = "", collapse = ""), stdout());
# shared folder
gDriveFolder <- "MiApp";
write(paste0(c("Folder..........\t", gDriveFolder), sep = "", collapse = ""), stdout());
# solution
gDriveSolution <- "Anomaly_Detection_with_R";
write(paste0(c("Solution........\t", gDriveSolution), sep = "", collapse = ""), stdout());
# project
gDriveProject <- "";
write(paste0(c("Project.........\t", gDriveProject), sep = "", collapse = ""), stdout());
# namespace
gDriveNamespace <- paste0(c(gDriveSolution, gDriveProject), sep = "", collapse = ".");
write(paste0(c("Namespace.......\t", gDriveNamespace), sep = "", collapse = ""), stdout());
# common
gDriveCommon <- "RCommon";
write(paste0(c("Common..........\t", gDriveCommon), sep = "", collapse = ""), stdout());
# path
gDriveList <- c(gDriveHome, gDriveFolder, gDriveSolution, gDriveProject); #, "Data");
#, gDriveSpace);
gDrivePath <- paste0(gDriveList, sep = "/", collapse = "");
write(paste0(c("Path old........\t", getwd()), sep = "", collapse = ""), stderr());
write(paste0(c("Path new........\t", gDrivePath), sep = "", collapse = ""), stdout());
setwd(gDrivePath);
###name
twoNameList <- c("Latency (ms)", "Throughput (mb/s)");
oneNameList <- c("NoName");
# DATA1
# load("Data/data1.RData");
aList <- get(load("Data/data1.RData"));
rm(data1);
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
DescribeOutlierList(aList);
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


