# Train parameters
model_params0 = list()
model_params0$tr_is_repeatedcv_vs_oob = "repeatedcv" #"oob"
model_params0$tr_n = 2
model_params0$tr_r = 2

# RMSE       Rsquared   MAE = 0.3,0.3,0.237
model_params0$gbm.interaction.depth = c(5) # c(3,5)#c(1,3)
model_params0$gbm.n.trees = c(10) # c(50,100) #c(25,50)
model_params0$gbm.n.minobsinnode = c(100, 500) # c(100,300) #c(10,100)
model_params0$gbm.shrinkage = c(0.2) # c(0.1, 0.2, 0.3)

model_params0$glm.parameter = c(0.1, 1e-3, 1e-5, 1e-8) # c(0.1, 0.2, 0.3)

#model_params0$rf.mtry = c(2) # seq(2,8,4)
model_params0$rf.ntree = c(100)
#===========================

modeling <- function(dtrain, method_name = "gbm", model_params=model_params0)
{
  #Train GBM
  print(modelLookup(model=method_name))
  ctrlGbm <- trainControl(method = model_params$tr_is_repeatedcv_vs_oob, #Error : Out of bag estimates are not implemented for this model
                          number = model_params$tr_n, 
                          repeats = model_params$tr_r, 
                          verboseIter = FALSE
                          ,allowParallel = TRUE
                          ,savePredictions = TRUE
  )
  
  gbmGrid <-  expand.grid( .interaction.depth = model_params$gbm.interaction.depth #c(1,2,3) # 5, 9 - overfitting , #c(1, 5, 9), 
                           , .n.trees = model_params$gbm.n.trees #(1:3)*25 #  50
                           , .shrinkage = model_params$gbm.shrinkage #0.2 #0.1
                           , .n.minobsinnode = model_params$gbm.n.minobsinnode #10
                           )
  glmGrid <-  expand.grid(parameter = model_params$glm.parameter)
  
  rfGrid <-  expand.grid(mtry = floor(1+sqrt(1+ncol(dtrain))))#model_params$rf.mtry)#, .ntree=model_params0$rf.ntree)#mtry (#Randomly Selected Predictors)
  
  
  print(method_name)
  gGrid = gbmGrid
  if (method_name=="glm") { gGrid = glmGrid}
  if (method_name=="rf") { gGrid = rfGrid}
  print(gGrid)
  
  model_gbm = NULL
  tryCatch(
    if (method_name=="rf") 
      model_gbm <- caret::train(Target ~ ., data = dtrain ,method = method_name #"gbm"
                                #,metric = model_params$gbm_metric # "Kappa"
                                ,preProcess = c("scale", "center")
                                ,ntree=model_params$rf.ntree
                                ,trControl = ctrlGbm
                                #, verbose = TRUE #FALSE
                                , tuneGrid = gGrid,
                                importance=TRUE#n specify the option importance=T. This will get passed to the underlying random forest call.
      )
    else 
      model_gbm <- caret::train(Target ~ .,
                                data = dtrain
                                ,method = method_name #"gbm"
                                #ntree = tr_numtree,
                                #,metric = model_params$gbm_metric # "Kappa"
                                ,preProcess = c("scale", "center")
                                ,trControl = ctrlGbm
                                #, verbose = TRUE #FALSE
                                ## Now specify the exact models to evaluate:
                                , tuneGrid = gGrid
      ),
    warning = function(w) {print(paste(" MYEXCEPTIONwarning ", w)); },
    error = function(e) {print(paste(" MYEXCEPTION: error", e));  NaN})
  
  
  return (model_gbm)
}

#modeling_result_plot(model_gbm, dtest, dtrain)
modeling_result_plot<-function(model_gbm, dtest, dtrain){
  graphics.off(); par(mfrow=c(1,1));  
  
  tryCatch(
    plot(model_gbm),
    warning = function(w) {print(paste(" MYEXCEPTIONwarning ", w)); },
    error = function(e) {print(paste(" MYEXCEPTION: error", e));  NaN})
  
  mypath <- file.path(file_path, paste("modeling_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=model_gbm),main="GBM - Variable Importance")
  mypath <- file.path(file_path, paste("modeling_response", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
}

pred_errors<-function(predictions, dtest_Target)
{
  t = dtest_Target
  et = exp(t)
  
  err = sum(abs(predictions-t))/length(predictions)
  err_median = median(abs(predictions-t))
  
  err_exp = sum(abs(exp(predictions)-et))/length(predictions)
  err_exp_rel = sum( abs(exp(predictions)-et)/et) /length(predictions)
  err_exp_median = median(abs(exp(predictions)-et))
  print(paste("error: mean, median, relative=", err_exp, err_exp_median, err_exp_rel, "log mean, median=", err, err_median, sep=" "))
  return (list(err=err, err_median=err_median, err_exp=err_exp, err_exp_rel=err_exp_rel, err_exp_median=err_exp_median))
}
pred_print<-function(model_gbm0, dtest)
{
  predictions<-predict.train(object=model_gbm0,dtest,type="raw")
  #table(predictions)
  ret=pred_errors(predictions, dtest$Target)
  return(predictions)
}
modeling_result_print<-function(model_gbm0, dtest, dtrain){
  pred_test=NULL
  pred_train=NULL
  if (!is.null(model_gbm0)) {
    #Predictions
    print("=== TEST")
    pred_test=pred_print(model_gbm0, dtest)
    print("=== TRAIN")
    pred_train=pred_print(model_gbm0, dtrain)
    #VarImp
    print(model_gbm0)  
    print(varImp(object=model_gbm0))
  }
  return (list(pred_train=pred_train, pred_test=pred_test))
}



#========== h2o
train_deep_learning_h2o <- function(dtrain, dtest, hidden_layers=c(6,6)) 
{
  # Load H2O
  library(h2o)
  h2o.init(nthreads=-1, max_mem_size="2G")
  h2o.removeAll() ## clean slate - just in case the cluster was already running
  #kd_h2o<-h2o.init(nthreads = -1, max_mem_size = "16g")
  
  # Installation of H2O-Ensemble, does not work on Kaggle cloud
  #install.packages("https://h2o-release.s3.amazonaws.com/h2o-ensemble/R/h2oEnsemble_0.1.8.tar.gz", repos = NULL)
  #library(h2oEnsemble)
  set.seed(12345)
  " load H2o data frame // validate that H2O flow looses all continous data
  train_frame.hex<-as.h2o(dtrain)
  valid_frame.hex<-as.h2o(dtest)
  valid_predict.hex<-as.h2o(dtest$Target)
  test.hex<-as.h2o(test)"
  #---------------------------------
  'data_h20 <- as.h2o(data)
  splits <- h2o.splitFrame(data_h20, c(0.6,0.2), seed=1234)
  train  <- h2o.assign(splits[[1]], "train.hex") # 60%
  valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
  test   <- h2o.assign(splits[[3]], "test.hex")  # 20%
  '
  
  test_h2o <- as.h2o(dtest)
  
  splits <- h2o.splitFrame(test_h2o, c(0.5), seed=1234)
  valid  <- h2o.assign(splits[[1]], "valid.hex") # 20%
  test   <- h2o.assign(splits[[2]], "test.hex")  # 20%
  
  #data_h2o <- as.h2o(dtrain)
  train  <- as.h2o(dtrain) #h2o.assign(splits[[1]], "train.hex") # 80%
  
  predictors <- setdiff(names(train), "Target")
  #print(predictors)
  response <- "Target"
  
  m2 <- h2o.deeplearning(
    model_id="dl_model_faster", 
    training_frame=train, 
    validation_frame=valid, 
    x=predictors,
    y=response,
    overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
    #activation="Rectifier",  ## default
    #activation="RectifierWithDropout",
    #input_dropout_ratio=0.05,
    hidden=hidden_layers, #c(6,6,6), #32,32,32),                  ## small network, runs faster
    epochs=10,#1000000,                      ## hopefully converges earlier...
    score_validation_samples=2000, #10000,      ## sample the validation dataset (faster)
    stopping_rounds=2,
    stopping_metric="MSE", #misclassification", ## could be "MSE","logloss","r2"
    stopping_tolerance=0.01
    ,l1=1e-5,#1e-5,                        ## add some L1/L2 regularization
    l2=1e-5,#1e-5,
    max_w2=10                       ## helps stability for Rectifier
    ,   nfolds=4 #N+1 models will be built: 1 full training data, and N models with each 1/N-th of the data held out 
    , fold_assignment="Modulo" # can be "AUTO", "Modulo", "Random" or "Stratified"
    ,standardize = TRUE
  )
  
  path <- h2o.saveModel(m2, path=paste(file_path,"/h2o_best_deeplearning_model", sep=""), force=TRUE); print(path)
  #m_loaded <- h2o.loadModel(path)
  #summary(m_loaded)
  
  print(head(as.data.frame(h2o.varimp(m2))))
  
  print(summary(m2))
  
  graphics.off()
  par(mfrow=c(1,1))
  plot(m2)
  mypath <- file.path(file_path, paste("fig_h2o_model", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off()
  par(mfrow=c(1,1))
  vars = as.data.frame(h2o.varimp(m2))
  #barplot(vars$relative_importance, names.arg=vars$variable)
  q<- vars$relative_importance
  names(q)<-vars$variable
  barchart(q)
  mypath <- file.path(file_path, paste("fig_h2o_varimp", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  #print(h2o.performance(m2, train=T))          ## sampled training data (from model building)
  #print(h2o.performance(m2, valid=T))          ## sampled validation data (from model building)
  
  'pred_test_h20 <- h2o.predict(m2, test)
  pred_train_h20 <- h2o.predict(m2, train)
  pred_valid_h20 <- h2o.predict(m2, valid)'
  
  #test$Accuracy <- pred$predict == test$Cover_Type
  #summary( as.data.frame(test$Target))
  
  pred_train_h20_df = as.data.frame(h2o.predict(m2, train))
  
  pred_test_h20_df = as.data.frame(h2o.predict(m2, test_h2o))
  
  
  print("== h2o Test")
  ret = pred_errors(pred_test_h20_df$predict, dtest$Target)
  print("== h2o Train")
  ret = pred_errors(pred_train_h20_df$predict, dtrain$Target)
  
  return(list(train_pred=pred_train_h20_df$predict, test_pred=pred_test_h20_df$predict))  
}
modeling_ensemble <- function(dtrain, dtest, split_ratio=0.8, is_timesorted_vs_sample=TRUE)
{
  #Multicore
  library(doParallel)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  model_gbm = modeling(dtrain, method_name = "gbm", model_params=model_params0)
  pred_gbm=modeling_result_print(model_gbm, dtest, dtrain)
  modeling_result_plot(model_gbm, dtest, dtrain)
  
  model_glm = modeling(dtrain, method_name = "glm", model_params=model_params0)
  pred_glm=modeling_result_print(model_glm, dtest, dtrain)
  modeling_result_plot(model_glm, dtest, dtrain)
  
  #model_rf = modeling(dtrain, method_name = "rf", model_params=model_params0)
  #pred_rf=modeling_result_print(model_rf, dtest, dtrain)
  stopCluster(cl)
  
  h2o_model <- train_deep_learning_h2o(dtrain, dtest, hidden_layers=c(6,6,6)) 
  
  #ensemble
  dtrain_ensemble = data.frame("h2o"=h2o_model$train_pred, "glm"=pred_glm$pred_train, "gbm"=pred_gbm$pred_train, "Target"=dtrain$Target)#, "rf"=pred_rf$pred_train,
  dtest_ensemble = data.frame("h2o"=h2o_model$test_pred, "glm"=pred_glm$pred_test, "gbm"=pred_gbm$pred_test, "Target"=dtest$Target)#,"rf"=pred_rf$pred_test, 
  jpgname = "_dtrain_ensemble"
  plot_corr(dtrain_ensemble)  
  jpgname = "_dtest_ensemble"
  plot_corr(dtest_ensemble)  
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  model_ensemble_glm=modeling(dtrain_ensemble, method_name = "glm", model_params=model_params0)
  stopCluster(cl)
  pred_model_ensemble_glm=modeling_result_print(model_ensemble_glm, dtest_ensemble, dtrain_ensemble)
  
  print("========= modeling ============")
  print("========= gbm ============")
  #pred_gbm=modeling_result_print(model_gbm, dtest, dtrain)
  print("== h2o Test")
  r=pred_print(model_gbm, dtest)
  print("== h2o Train")
  r=pred_print(model_gbm, dtrain)
  
  print("========= glm ============")
  #pred_glm=modeling_result_print(model_glm, dtest, dtrain)
  print("== h2o Test")
  r=pred_print(model_gbm, dtest)
  print("== h2o Train")
  r=pred_print(model_gbm, dtrain)
  
  print("========= h2o ============")
  print("== h2o Test")
  ret = pred_errors(h2o_model$test_pred, dtest$Target)
  print("== h2o Train")
  ret = pred_errors(h2o_model$train_pred, dtrain$Target)
  
  print("========= ensemble  ============")
  #pred_model_ensemble_glm=modeling_result_print(model_ensemble_glm, dtest_ensemble, dtrain_ensemble)  
  print("== h2o Test")
  pred_print(model_ensemble_glm, dtest_ensemble)
  print("== h2o Train")
  pred_print(model_ensemble_glm, dtrain_ensemble)
  
  return (list(model_gbm=model_gbm, model_glm=model_glm, model_ens=model_ensemble_glm))
}
