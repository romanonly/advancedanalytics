# install.packages("h2o")
# install.packages('randomForest')
library(randomForest)
setwd("~/R/romaprojects/Webinar_ISDS")

library(mice)
library(ggplot2)
## try http:// if https:// URLs are not supported
## source("https://bioconductor.org/biocLite.R")
## biocLite("pcaGoPromoter")

library(pcaGoPromoter)
library(ellipse)
library(tidyr)

library(doParallel)
source("roman-functions.R")

# point to the prostate data set in the h2o folder - no need to load h2o in memory yet
if (TRUE) {
  
  bc_data <- read_data(proportion = 5)
  outcome_name <- 'classes'
  prostate_df <- bc_data
} else {
  
  prosPath = system.file("extdata", "prostate.csv", package = "h2o")
  prostate_df <- read.csv(prosPath)
  outcome_name <- 'CAPSULE'
  
  # We don't need the ID field
  prostate_df <- prostate_df[,-1]
}

summary(prostate_df)

set.seed(1234)
random_splits <- runif(nrow(prostate_df))
train_df <- prostate_df[random_splits < .5,]
dim(train_df)

validate_df <- prostate_df[random_splits >=.5,]
dim(validate_df)
# Get benchmark score


feature_names <- setdiff(names(prostate_df), outcome_name)


set.seed(1234)
rf_model <- randomForest(x=train_df[,feature_names],
                         y=as.factor(train_df[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)

validate_predictions <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")


# install.packages('pROC')
library(pROC)
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions[,2])

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')
"
auc_rf = roc(response=as.numeric(as.factor(train_df[,outcome_name]))-1,
             predictor=predict(rf_model, newdata=train_df[,feature_names], type='prob')[,2])

plot(auc_rf, col='red', print.thres = 'best', main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')
"

# build autoencoder model

library(h2o)
localH2O = h2o.init()
prostate.hex<-as.h2o(train_df, destination_frame="train.hex")
prostate.dl = h2o.deeplearning(x = feature_names, training_frame = prostate.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 1234,
                               hidden = c(6,5,6), epochs = 50)

# interesting per feature error scores
# prostate.anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=TRUE)
# head(prostate.anon)

prostate.anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=FALSE)
head(prostate.anon)
err <- as.data.frame(prostate.anon)

# interesting reduced features (defaults to last hidden layer)
# http://www.rdocumentation.org/packages/h2o/functions/h2o.deepfeatures
# reduced_new  <- h2o.deepfeatures(prostate.dl, prostate.hex)

plot(sort(err$Reconstruction.MSE))

# use the easy portion and model with random forest using same settings
summary(train_df[err$Reconstruction.MSE < 1110.05, ] $classes)
K = 0.06
summary(train_df[err$Reconstruction.MSE < K, ] $classes)
train_df_auto <- train_df[err$Reconstruction.MSE < K, ] #0.1,]

set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)
rf_model_0 = rf_model
validate_predictions_known <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")

auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_known[,2])

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# use the hard portion and model with random forest using same settings
train_df_auto <- train_df[err$Reconstruction.MSE >= K,] #0.1,]

set.seed(1234)
rf_model <- randomForest(x=train_df_auto[,feature_names],
                         y=as.factor(train_df_auto[,outcome_name]),
                         importance=TRUE, ntree=20, mtry = 3)
rf_model_1 = rf_model
validate_predictions_unknown <- predict(rf_model, newdata=validate_df[,feature_names], type="prob")
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=validate_predictions_unknown[,2])

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

# bag both results set and measure final AUC score
valid_all <- (validate_predictions_known[,2] + validate_predictions_unknown[,2]) / 2
valid_all_train <- (predict(rf_model_0, newdata=train_df[,feature_names], type="prob")+
                      predict(rf_model_1, newdata=train_df[,feature_names], type="prob")) / 2.

  
auc_rf = roc(response=as.numeric(as.factor(validate_df[,outcome_name]))-1,
             predictor=valid_all)

plot(auc_rf, print.thres = "best", main=paste('AUC:',round(auc_rf$auc[[1]],3)))
abline(h=1,col='blue')
abline(h=0,col='green')

gains <- plot_gain(rf_model_0, test_data, train_data)
gains <- plot_gain(rf_model_1, test_data, train_data)
gains <- plot_gain(rf_model_0, test_data, train_data, valid_all, valid_all_train)
