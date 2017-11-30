library(caret)

library(mice)
library(ggplot2)

library(pcaGoPromoter)
library(ellipse)
library(tidyr)

library(doParallel)

library(lattice)

library(dplyr)

library(mlbench)
library(pROC)

file.rename("~/.RData", "~/.RData.backup")
PATHNAME = "."
setwd(PATHNAME)

source("script2-functions.R")
output_dir_name = "/datasets/bc_data_10_percent_ensemble"

#
# Get data
#
file_path=paste(PATHNAME, output_dir_name, sep="")
dir.create(file_path)
data_file_name="bcdata_model"
# output re-directed in cwd. output is appended
# to existing file. output also send to terminal. 
mypath <- file.path(file_path, paste("report_script3_", data_file_name, ".txt", sep = ""))
sink(mypath, append=FALSE, split=TRUE)

foldout_proportion = 10
d <- read_data(proportion = foldout_proportion)
bc_data = d$bc_data
bc_data_missing = d$bc_data_missing
summary(bc_data)
ggplot(bc_data, aes(x = classes, fill = classes)) + geom_bar()

print(file_path)
print(summary(bc_data$classes))
colnames(bc_data)

# Train and test partition
set.seed(42)
index <- caret::createDataPartition(bc_data$classes, p = 0.5, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]


plot_print_model_list <- function(model_list, testing, training, prefix_str)
{
  pred_frame <- as.data.frame(predict(model_list, newdata=testing)) #head(testing,100)))#, type = "prob"))
  #head(pred_frame, 2)
  
  m1 = names(pred_frame)[1]
  m2 = names(pred_frame)[2]
  
  # CORRELATION: glm, rf and gbm
  graphics.off()
  mypath <- file.path(file_path, paste(prefix_str, "compare_glm_", "rf", ".jpg", sep = "")); print(mypath)
  my_plot = qplot(pred_frame[[m1]], pred_frame[[m2]], colour = testing$classes, 
                  geom = c("point", "smooth"), span = 1
                  #geom = "jitter", alpha = I(1 / 2)
                  )
  my_plot = my_plot + labs(x = m1, y = m2)
  my_plot = my_plot + labs(title = paste("Correlation of two models: ", m1, " and ", m2, sep = " "))
  print(my_plot); dev.copy(jpeg,filename=mypath); dev.off()
  
  
  m1 = names(pred_frame)[2]
  m2 = names(pred_frame)[3]

  graphics.off()
  mypath <- file.path(file_path, paste(prefix_str, "compare_gbm_", "glm", ".jpg", sep = "")); print(mypath)
  #myplot = qplot(pred_frame$gbm, pred_frame$glm, colour = testing$classes)
  my_plot = qplot(pred_frame[[m1]], pred_frame[[m2]], colour = testing$classes, 
                  geom = c("point", "smooth"), span = 1
                  #geom = "jitter", alpha = I(1 / 2)
  )
  my_plot = my_plot + labs(x = m1, y = m2)
  my_plot = my_plot + labs(title = paste("Correlation of two models: ", m1, " and ", m2, sep = " "))
  print(my_plot); dev.copy(jpeg,filename=mypath); dev.off()
  
  #
  # Plot Uncertainties
  #
  graphics.off()
  mypath <- file.path(file_path, paste(prefix_str, "resamples_glm_", "rf", ".jpg", sep = "")); print(mypath)
  resampling <- resamples(model_list)
  my_plot = bwplot(resampling)
  print(my_plot); dev.copy(jpeg,filename=mypath); dev.off()
  
  # Correlation: BAGGING is working best for least CORRELATED models
  print( modelCor(resampling) )
  
  graphics.off()
  mypath <- file.path(file_path, paste(prefix_str, "_correlations.jpg", sep = "")); print(mypath)
  boxplot( modelCor(resampling) )
  #print(my_plot); 
  dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off()
  mypath <- file.path(file_path, paste(prefix_str, "resamples_xyplot_glm", "rf", ".jpg", sep = "")); print(mypath)
  my_plot = xyplot(resampling)
  print(my_plot); dev.copy(jpeg,filename=mypath); dev.off()
  
  #print( summary(resampling, metric = "ROC") )  
}

# Print statistics for ENSEBLE of models
analyze_ensemble <- function(greedy_ensemble, model_preds, testing, training, prefix_str)
{
  ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
  df_ens_preds = data.frame(malignant = ens_preds)
  model_preds$ensemble <- ens_preds
  #summary(model_preds)
  
  print(" == AUC test data: notice ensenble improvementon test data" ); 
  print( caTools::colAUC(model_preds, testing$classes) );
  print(" == AUC test data end" );
  
  graphics.off()
  mypath <- file.path(file_path, paste(prefix_str, "_auc_testdata_all_ensemble.jpg", sep = "")); print(mypath)
  boxplot( caTools::colAUC(model_preds, testing$classes)  ) 
  dev.copy(jpeg,filename=mypath); dev.off()
  
  varImp(greedy_ensemble)
  
  ens_preds_train <- predict(greedy_ensemble, newdata=training, type="prob")
  df_ens_preds_train = data.frame(malignant = ens_preds_train)
  
  jpgname = paste(prefix_str, "ens_gain_model_rf", sep = ""); print(jpgname)
  gain = plot_gain (greedy_ensemble, 
                    testing, training, 
                    df_ens_preds, df_ens_preds_train,
                    file_path=file_path, 
                    jpgname=jpgname)
}

# Make two ENSEBLES of models
analyze_model_list <- function(model_list, testing, training, prefix_str)
{
  #
  # model_list: each model independent prefromance  
  # 
  model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
  #str(model_preds)
  model_preds <- lapply(model_preds, function(x) x[,"malignant"])
  #str(model_preds)
  model_preds1 <- data.frame(model_preds)
  model_preds2 <- data.frame(model_preds)
  #head(model_preds, 2)
  #summary(model_preds)
  
  
  #
  # model_list: ENSEMBLE prefromance. Train regression forthree preiction results (glm, rf, gbm)
  #
  greedy_ensemble <- caretEnsemble(
    model_list, 
    metric="ROC",
    trControl=trainControl(
      number=tr_n,
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))
  
  print( " === print greedy_ensemble summary "); print( summary(greedy_ensemble) ); print( " === print summary end");
  
  analyze_ensemble(greedy_ensemble, model_preds1, testing, training, prefix_str=paste(prefix_str, "_greedy_ens_", sep=""))
  
  #
  #caretStack allows us to move beyond simple blends of models to using "meta-models" to ensemble 
  # collections of predictive models. DO NOT use the trainControl object you used to fit the 
  # training models to fit the ensemble. The re-sampling indexes will be wrong. 
  # Fortunately, you don"t need to be fastidious with re-sampling indexes for caretStack, 
  # as it only fits one model, and the defaults train uses will usually work fine:
  glm_ensemble <- caretStack(
    model_list,
    method="glm",
    metric="ROC",
    trControl=trainControl(
      method="boot",
      number=10,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=twoClassSummary
    )
  )
  print( " === print glm_ensemble summary "); print( summary(glm_ensemble) ); print( " === print summary end");
  analyze_ensemble(greedy_ensemble, model_preds2, testing, training, prefix_str=paste(prefix_str, "_glm_ens_", sep=""))
}



#
# BAGGING-1
#
training <- train_data
testing <- test_data

tr_numtree = 100 # 2 #100
tr_n = 9 #25
#tr_r = 4 #10

my_control <- trainControl(
  method="boot",
  number=tr_n, #5, #25,
  #repeats = tr_r, 
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$classes, tr_n), #25),
  summaryFunction=twoClassSummary,
  #from ctrlDown
  verboseIter = FALSE
  #sampling = "smote" #down"
)

#
# BAGGING: rf, glm over my_control parameters
#
library("caretEnsemble")
#
# Ensemble more models: RF, GBM
#
results <- capture.output(
  model_list <- caretList(
    classes~., 
    data=training,
    metric="ROC",
    trControl=my_control,
    #verbose = FALSE,
    continue_on_fail = TRUE,
    methodList=c("rf", "gbm", "glm") #c("glm", "rpart")
  )
)
#
# Ensemble more models: RF, NN
#
library(mlbench)
library(randomForest)
library(nnet)

results <- capture.output(
  model_list_big <- caretList(
    classes~., 
    data=training,
    trControl=my_control,
    metric="ROC",
    verbose = FALSE,
    methodList=c("gbm"), 
    tuneList=list(
      rf=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=3), ntree = tr_numtree, preProcess = c("scale", "center")),
      #rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=8), ntree = tr_numtree, preProcess="pca"),
      nnet=caretModelSpec(method="nnet", tuneLength=4, trace=FALSE)
    )
  )
)
#
# Analysis, print, plot 
#
prefix_str = 'models_1_'
plot_print_model_list(model_list, testing, training, prefix_str)
analyze_model_list(model_list, testing, training, prefix_str)


prefix_str = 'models_2_'
plot_print_model_list(model_list_big, testing, training, prefix_str)
analyze_model_list(model_list_big, testing, training, prefix_str)



mypath <- file.path(file_path, paste("saved_workspace_ensemble", data_file_name, ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)




