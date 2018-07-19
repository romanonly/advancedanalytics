#load indepenndent packages
'
source("https://bioconductor.org/biocLite.R")
biocLite("pcaGoPromoter")

UBUNTU: 

sudo apt-get  install libcurl4-gnutls-dev
install.packages("DMwR")

sudo apt-get update
sudo apt-get install libxml2-dev
sudo apt-get install r-cran-xml
install.packages("XML")
install.packages("plotROC")
'
#load from R repository
packages1 <- c("corrr","vcd", "readxl","tidyverse", "bnlearn", "rpart",  "gbm", "caret", "mice", "VIM", "missForest", "ggplot2","randomForest", "e1071", "ROSE",  "DMwR")#"SMOTE",
packages2 <- c("ellipse", "doParallel", "lattice","iterators","foreach","parallel")
packages3 <- c("mlbench", "pROC", "ROCR","jpeg","tidyr","plyr","dplyr")
packages4 <- c("gridExtra", "XML", "gridSVG","plotROC", "pcaGoPromoter")
packages5 <- c("scatterplot3d", "plotly", "ggfortify","stats","psych","pca3d","logisticPCA","ggplot2", "discretization")
packages6 <- c("lubridate", "corrplot", "h2o")#,"h2oEnsemble", "imputeTS","rjson", "DT")
packages_backup = c("bigpca")

packages = c(packages1, packages2, packages3, packages4,packages5,packages6)
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

lapply(packages, library, character.only = TRUE)
