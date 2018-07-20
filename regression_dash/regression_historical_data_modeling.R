# Train parameters
model_params0 = list()
model_params0$tr_is_repeatedcv_vs_oob = "repeatedcv" #"oob"
model_params0$tr_n = 5
model_params0$tr_r = 2

#
# Main parameters:
#
# model_params0$gbm.n.trees = c(10) #1000) # c(50,100) #c(25,50)
## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
#search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 3600, max_models = 1000, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
#  epochs=500,
  

# RMSE       Rsquared   MAE = 0.3,0.3,0.237
model_params0$gbm.interaction.depth = c(5)#(5) # c(3,5)#c(1,3)
model_params0$gbm.n.trees = c(500) #1000) # c(50,100) #c(25,50)
model_params0$gbm.n.minobsinnode = c(100,200) # c(100,300) #c(10,100)
model_params0$gbm.shrinkage = c(0.2, 0.3) # c(0.1, 0.2, 0.3)

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
                                ,preProcess = c("scale", "center")
                                ,trControl = ctrlGbm
                                , tuneGrid = gGrid
      ),
    #warning = function(w) {print(paste(" MYEXCEPTIONwarning ", w)); },
    error = function(e) {print(paste(" MYEXCEPTION: error", e));  NaN})
  
  
  return (model_gbm)
}

#modeling_result_plot(model_gbm, dtest, dtrain)
modeling_result_plot<-function(model_gbm, dtest, dtrain,file_path, jpgname){
  graphics.off(); par(mfrow=c(1,1));  
  
  if (!is.null(model_gbm)) {
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
}

pred_errors<-function(predictions, dtest_Target)
{
  t = dtest_Target
  dt = abs(predictions-t)
  
  n = length(dt)
  err2 = sqrt(sum(dt*dt)/n)
  err = sum(dt)/n
  err_median = median(dt)
  
  et = exp(t)
  expt = exp(predictions)
  dexpt = abs(expt-et)
  err_exp2 = sqrt(sum(dexpt*dexpt)/n)
  err_exp = sum(dexpt)/n
  err_exp_rel = sum( dexpt/et) / n
  err_exp_median = median(dexpt)
  
  #err = formatC(x, digits = 8, format = "f") # format(round(err, 2), nsmall = 2)
  err2 = sprintf(err2, fmt = '%#.2f')
  err = sprintf(err, fmt = '%#.2f')
  err_median = sprintf(err_median, fmt = '%#.2f')
  
  err_exp2 = sprintf(err_exp2, fmt = '%#.0f')
  err_exp = sprintf(err_exp, fmt = '%#.0f')
  err_exp_rel = sprintf(err_exp_rel, fmt = '%#.2f')
  err_exp_median = sprintf(err_exp_median, fmt = '%#.0f')
  
  print(paste("error: mse, mean, median, relat=", err_exp2, err_exp, err_exp_median, err_exp_rel, "log mse, mean, median=", err2, err, err_median, sep=" "))
  return (list(err2=err2, err=err, err_median=err_median, err_exp=err_exp, err_exp_rel=err_exp_rel, err_exp_median=err_exp_median))
}
pred_print<-function(model_gbm0, dtest)
{
  predictions = NULL
  if (!is.null(model_gbm0)) {
    predictions<-predict.train(object=model_gbm0,dtest,type="raw")
    #table(predictions)
    ret=pred_errors(predictions, dtest$Target)
  }
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
    #print(varImp(object=model_gbm0))
  }
  return (list(pred_train=pred_train, pred_test=pred_test))
}



#========== h2o
train_deep_learning_h2o <- function(dtrain, dtest, hidden_layers=c(6,6),jpgname,file_path) 
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

  test_h2o <- as.h2o(dtest)
  
  splits <- h2o.splitFrame(test_h2o, c(0.5), seed=1234)
  valid  <- h2o.assign(splits[[1]], "valid.hex") # 20%
  test   <- h2o.assign(splits[[2]], "test.hex")  # 20%
  
  train  <- as.h2o(dtrain) #h2o.assign(splits[[1]], "train.hex") # 80%
  
  predictors <- setdiff(names(train), "Target")

  response <- "Target"
  
  grid = NULL
  m2 = NULL
  
  'hyper_params <- list(
    hidden=list(c(32,32,32),c(64,64),c(10,10,10),c(10,20)),
    input_dropout_ratio=c(0,0.01,0.05),
    rate=c(0.01,0.02,0.03),
    rate_annealing=c(1e-8,1e-7,1e-6,1e-4)
  )
  hyper_params'
  hyper_params <- list(
    activation=c("Maxout","RectifierWithDropout"),#,"Rectifier","Tanh","TanhWithDropout","MaxoutWithDropout"),
    hidden=list(c(20), c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
    input_dropout_ratio=c(0)#,0.05, 0.2),
    ,rate=c(0.01,0.02,0.03)
    ,l1=seq(0, 1e-4, 1e-6)
    #l2=seq(0,1e-4,1e-6)
  )
  #hyper_params
  
  ## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
  search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 7200, max_models = 1000, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
  #search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 60, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
  
    
  grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id="dl_grid", 
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    epochs=500
    
  #m2 <- h2o.deeplearning(
    #model_id="dl_model_faster", 
    #hidden=hidden_layers, #c(6,6,6), #32,32,32),                  ## small network, runs faster
    #epochs=200,#1000000,                      ## hopefully converges earlier...
  
    ,training_frame=train, 
    validation_frame=valid, 
    x=predictors,
    y=response,
    overwrite_with_best_model=TRUE,    ## Return the final model after 10 epochs, even if not the best
    #activation="Rectifier",  ## default
    #activation="RectifierWithDropout",
    #input_dropout_ratio=0.05,
  
    
    score_validation_samples=5000, #10000,      ## sample the validation dataset (faster)
    stopping_rounds=5,
    stopping_metric="MSE", #misclassification", ## could be "MSE","logloss","r2"
    stopping_tolerance=0.01
    #,l1=1e-5
    ,l2=1e-5,
    max_w2=10                       ## helps stability for Rectifier
  
    , score_duty_cycle=0.025         ## don't score more than 2.5% of the wall time
    #,nfolds=5 #N+1 models will be built: 1 full training data, and N models with each 1/N-th of the data held out 
    #, fold_assignment="Modulo" # can be "AUTO", "Modulo", "Random" or "Stratified"
    ,standardize = TRUE
  )
  
  if (!is.null(grid)) {
    grid <- h2o.getGrid("dl_grid",sort_by="MSE",decreasing=FALSE)
    #grid
  
    ## To see what other "sort_by" criteria are allowed
    #grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)
    
    ## Sort by logloss
    #h2o.getGrid("dl_grid",sort_by="logloss",decreasing=FALSE)
  
    ## Find the best model and its full set of parameters
    print("********************* grid@summary_table[1,] ************** ")
    print("********************* grid@summary_table[1,] ************** ")
    print("********************* grid@summary_table[1,] ************** ")
    print( grid@summary_table[1,] )
    best_model <- h2o.getModel(grid@model_ids[[1]])
    print("******************* best_model")
    #print(best_model)
  
    #print(best_model@allparameters)
    #print(h2o.performance(best_model, valid=T))
    #print(h2o.logloss(best_model, valid=T))
    
    m2 = best_model
  }

  path <- h2o.saveModel(m2, path=paste(file_path,"/h2o_best_deeplearning_model", sep=""), force=TRUE); print(path)
  #m_loaded <- h2o.loadModel(path)
  #summary(m_loaded)
  
  #print(head(as.data.frame(h2o.varimp(m2))))
  
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
  
  pred_train_h20_df = as.data.frame(h2o.predict(m2, train))
  
  pred_test_h20_df = as.data.frame(h2o.predict(m2, test_h2o))
  
  
  print("== h2o Test")
  ret = pred_errors(pred_test_h20_df$predict, dtest$Target)
  print("== h2o Train")
  ret = pred_errors(pred_train_h20_df$predict, dtrain$Target)
  
  return(list(model_h2o=m2, train_pred=pred_train_h20_df$predict, test_pred=pred_test_h20_df$predict))  
}

modeling_ensemble <- function(dtrain, dtest, split_ratio=0.8, 
                              is_timesorted_vs_sample=TRUE,
                              hidden_layers=c(6,6,6)
                              ,file_path, jpgname)
{
  h2o_model <- train_deep_learning_h2o(dtrain, dtest, hidden_layers,jpgname,file_path)
  
  if (!is.null(h2o_model)) { 
    dtrain_ensemble = data.frame("h2o"=h2o_model$train_pred)
    dtest_ensemble = data.frame("h2o"=h2o_model$test_pred)
  }
  #Multicore
  library(doParallel)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, outfile=paste0('./info_parallel.log'))
  registerDoParallel(cl)
  
  model_gbm = modeling(dtrain, method_name = "gbm", model_params=model_params0)
  pred_gbm=modeling_result_print(model_gbm, dtest, dtrain)
  modeling_result_plot(model_gbm, dtest, dtrain,file_path, jpgname)
  
  model_glm = modeling(dtrain, method_name = "glm", model_params=model_params0)
  pred_glm=modeling_result_print(model_glm, dtest, dtrain)
  modeling_result_plot(model_glm, dtest, dtrain,file_path, jpgname)
  
  stopCluster(cl)
  registerDoSEQ()
  
  dtrain_ensemble$Target=dtrain$Target
  dtest_ensemble$Target =dtest$Target
  
  if (!is.null(model_glm)) { 
    dtrain_ensemble$glm=pred_glm$pred_train 
    dtest_ensemble$glm = pred_glm$pred_test
  }
  if (!is.null(model_gbm)) { 
    dtrain_ensemble$gbm=pred_gbm$pred_train
    dtest_ensemble$gbm = pred_gbm$pred_test
  }
  
  jpgname = "_dtrain_ensemble"
  plot_corr(dtrain_ensemble,file_path, jpgname)  
  jpgname = "_dtest_ensemble"
  plot_corr(dtest_ensemble,file_path, jpgname)  
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  model_ensemble_glm=modeling(dtrain_ensemble, method_name = "glm", model_params=model_params0)
  stopCluster(cl)
  pred_model_ensemble_glm=modeling_result_print(model_ensemble_glm, dtest_ensemble, dtrain_ensemble)
  
  print("========= gbm ============")
  print("== gbm Test")
  r=pred_print(model_gbm, dtest)
  print("== gbm Train")
  r=pred_print(model_gbm, dtrain)
  
  print("== glm Test")
  r=pred_print(model_glm, dtest)
  print("== glm Train")
  r=pred_print(model_glm, dtrain)
  
  print("== h2o Test")
  ret = pred_errors(h2o_model$test_pred, dtest$Target)
  print("== h2o Train")
  ret = pred_errors(h2o_model$train_pred, dtrain$Target)
  
  print("== ensemble Test")
  pred_print(model_ensemble_glm, dtest_ensemble)
  print("== ensemble Train")
  pred_print(model_ensemble_glm, dtrain_ensemble)
  
  return (list(model_gbm=model_gbm, model_glm=model_glm, h2o_model=h2o_model, model_ens=model_ensemble_glm))
}
