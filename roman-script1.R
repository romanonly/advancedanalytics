"
---
  title: unbalanced-testcase
output: html_document
---
"
  
##==##r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
##==##``` 

##==##r include, include=FALSE}
file.rename("~/.RData", "~/.RData.backup")

#setwd("C:\Users\rkazinnik\Desktop\RStudio\romaprojects\Webinar_ISDS")
setwd("~/R/romaprojects/Webinar_ISDS")
source("roman-functions.R")

## install.packages('e1071', dependencies=TRUE)
library(mice)
library(ggplot2)
## try http:// if https:// URLs are not supported
## source("https://bioconductor.org/biocLite.R")
## biocLite("pcaGoPromoter")

library(pcaGoPromoter)
library(ellipse)
library(tidyr)

library(doParallel)
#library(caret)

#library(dplyr)
##==##``` 

## R Markdown
"
This is an R Markdown document. 
https://shiring.github.io/machine_learning/2017/03/31/webinar_code

https://shiring.github.io/machine_learning/2017/04/02/unbalanced
"

##==##r cars}

#------------------------------------------- READ DATA
#
#
#
#
#------------------------------------------- READ DATA
bc_data <- read_data()
#
# configure multicore
#
#library(doParallel)
if (FALSE) {
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
}
#------------------------------------------------
"

        PARTITION 

"
#------------------------------------------------
library(lattice)
library(caret)
library(dplyr)

set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.8, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]


rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>%
  gather(x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)


#----------------- UNBALANCED TOMEK
#
# 
# Tomek: remove majority class in toder to boost minority
#
#
library(unbalanced)
library(assertthat)
#data(ubIonosphere)

train_data_Tomek <- make_tomek(train_data) 

str(train_data_Tomek)
summary(train_data_Tomek)
set.seed(42)


#------------------------------------- MODELING
#
#
#
#
#-------------------------------------------
"
number - Either the number of folds or number of resampling iterations
repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
"
tr_n = 2 # 10
tr_r = 2 #10
tr_numtree = 5 #100???



library(ggplot2)
library(mlbench)
library(plotROC)
library(pROC)
#plot_ROC <- function(model_rf, test_data, train_data, npar=TRUE,print=TRUE) {
  
#
#----------------------------------- MODELING
#
#
#
#
#-------------------------------------------
#make_model_rf <- function(test_data, train_data, par_sampling="none", isprint=TRUE) {

m <- make_model_rf(test_data, train_data_Tomek, tr_n, tr_r, tr_numtree)
model_rf2<- m$model
cm_original2 <- m$cm
final2 <- m$final

m <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree)
model_rf<- m$model
cm_original <- m$cm
final <- m$final

m <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, par_sampling="down")
model_rf_under <- m$model
cm_under <- m$cm
final_under <- m$final

m <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, par_sampling="up")
model_rf_over <- m$model
cm_over <- m$cm


m <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, par_sampling="rose")
model_rf_rose<- m$model
cm_rose <- m$cm

m <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, par_sampling="smote")
model_rf_smote<- m$model
cm_smote <- m$cm


## Generate the test set results
lift_testing <- test_data
lift_results <- data.frame(Class = lift_testing$classes)
class1 = "malignant"
#class1 = "benign"
lift_results$GEN <- predict(model_rf, newdata = lift_testing, type = "prob")[,class1]
lift_results$ROSE <- predict(model_rf_rose, newdata = lift_testing, type = "prob")[,class1]
lift_results$SMOTE <- predict(model_rf_smote, newdata = lift_testing, type = "prob")[,class1]
lift_results$UNDER <- predict(model_rf_under, newdata = lift_testing, type = "prob")[,class1]
#head(lift_results,20)
trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ GEN + ROSE + SMOTE + UNDER, data = lift_results)
ggplot(lift_obj, values = 60)
cal_obj <- calibration(Class ~ GEN + ROSE + SMOTE + UNDER,
                       data = lift_results,
                       cuts = 25) #13)
ggplot(cal_obj)
'
plot(lift_obj, values = 60, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))

plot(cal_obj, type = "l", auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))

'
#
# Gain Lift Chairs
#install.packages('ROCR')
#require('ROCR')
#
#
library('ROCR')
#plot_gain <- function(model_rf, test_data, train_data, npar=TRUE,print=TRUE) {



##---------------------- PLOT RESULTS
#
#
# 
##-------------------------


##==##r}
models <- list(original2 = model_rf2,
               original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)

resampling <- resamples(models)
bwplot(resampling)
#summary(resampling, metric = "ROC")

##==##``` 

##==##r}
library(dplyr)
library(tidyr)

comparison <- plot_results(models)

comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.3, alpha = 0.7, size = 4)


  
test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$classes, 
                 predict(model, data, type = "prob")[, "malignant"],
                 levels = c("benign", "malignant"))
  ci(roc_obj)
}

outside_test <- lapply(models, test_roc, data = test_data)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
outside_test <- as.data.frame(outside_test)

summary(outside_test)

#---------------------------------
#
#
#
#
#

gains <- plot_gain(model_rf, test_data, train_data)
gains <- plot_gain(model_rf2, test_data, train_data2)
gains <- plot_gain(model_rf_under, test_data, train_data)
gains <- plot_gain(model_rf_over, test_data, train_data)
gains <- plot_gain(model_rf_rose, test_data, train_data)
gains <- plot_gain(model_rf_smote, test_data, train_data)
# gains$gaintest
plot_ROC(model_rf, test_data, train_data) 
plot_ROC(model_rf2, test_data, train_data) #_Tomek) 
plot_ROC(model_rf_under, test_data, train_data) 
plot_ROC(model_rf_over, test_data, train_data) 
plot_ROC(model_rf_rose, test_data, train_data) 
plot_ROC(model_rf_smote, test_data, train_data) 

#----------------------------------
#
#
#
#use Racing to select the best technique for an unbalanced dataset
library(unbalanced)
#ubIonosphere = train_data
#ubIonosphere = bc_data
#data(ubIonosphere)

#configure sampling parameters
ubConf <- list(type="ubUnder", percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)

#load the classification algorithm that you intend to use inside the Race
#see 'mlr' package for supported algorithms
library(randomForest)
library(data.table)
#use only 5 trees
#results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=50)
#
#
#bc_data$Target = FALSE
#bc_data$Target = as.factor(bc_data$classes == "malignant")
#bc_data <- subset(bc_data, select = -c(Target ))

results <- ubRacing(classes ~., bc_data, "randomForest", positive="malignant", ubConf=ubConf, ntree=50)

# try with 500 trees
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=500)
# let's try with a different algorithm
# library(e1071)
# results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)


