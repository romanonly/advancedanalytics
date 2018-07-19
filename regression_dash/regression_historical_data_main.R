#===== Setup Env
setwd("~/RstudioProjects/2018/doordash")
source("regression_historical_data_functions.R")
source("regression_historical_data_modeling.R")
source("regression_historical_data_transformation.R")
source("regression_script1-data-quality-metrics.R")
#===========read data
# make numeric predictors, except Target
convert_to_pca<-function(train, num_pca=10,pca=NULL) {
  # PCA
  library(h2o)
  h2o.init()
  num_cols = get_num_variables_list(train)
  num_cols = num_cols[num_cols != "Target"]
  
  d_numeric = train[names(train) %in% num_cols]
  d_others = train[!names(train) %in% names(d_numeric)]
  if (is.null(pca)) {
    pca = h2o.prcomp(training_frame = as.h2o(d_numeric), k = num_pca, transform = "STANDARDIZE",impute_missing=TRUE)
  }
  dpca = cbind(as.data.frame( predict(pca, newdata=as.h2o(d_numeric)) ), d_others)
  stopifnot(length(names(dpca)) == ncol(d_others) + num_pca)
  return (list(dpca=dpca, pca=pca))
}

is_apply_pca = TRUE
main <- function()
{
  #=== create folder for test data: save test data in folder:
  file_path_datatotest="./data_to_test_0023"
  file_log = make_dir_sink_log_txt(file_path_datatotest)
  #============
  readdata = read_train_submit_data()
  #===== prepare train-test data and submit data: apply same transormation
  ret = split_data(readdata$dtrain, split_ratio = 0.7, is_timesorted_vs_sample = FALSE)
  rtrain=data_transformation(ret$dtrain)
  rtest=data_transformation(ret$dtest)
  rsubmit=data_transformation(readdata$dsubmit)
  
  #===============================
  # Expirement 1: 
  # factor predictors removed
  train = rtrain$d_Target
  valid = rtest$d_Target
  submit = rsubmit$d_Target
  # make sure to have same labels in test data, as in train data 
  submit <- extend_levels_in_factors(train, submit)

  hist(train$Target)
  hist(test$Target)
  
  if (is_apply_pca) {
    num_pca = 10
    rpca_train = convert_to_pca(train, num_pca=num_pca,pca=NULL)
    train = rpca_train$dpca
    valid = convert_to_pca(valid, num_pca=num_pca,pca=rpca_train$pca)$dpca
    submit = convert_to_pca(submit, num_pca=num_pca,pca=rpca_train$pca)$dpca
  }
  
  stopifnot(0 == sum(names(train) != names(valid)))
  stopifnot(1 ==sum(!names(train) %in% names(submit)))
  #plot histograms of predictors
  plot_hist(submit[names(submit) %in% get_num_variables_list(submit)],file_path_datatotest, jpgname="_outliers_testset")
  
  
  r1 = run_modeling(train
                    , valid
                    , file_path="./datasets1_023_log"
                    , jpgname="_nofactors"
                    , hidden_layers=c(10,10))
  
  temp = data_to_test_compare(train, submit, r1)
  df1 = data_to_test(r1, submit)
  write.csv(df1, file = paste(file_path_datatotest, "/df1.csv",sep=""))
  # ==========================================
  # Expirement 3: factor predictors - to replace with frequencies
  train = rtrain$d_Target_responses_freq
  valid = rtest$d_Target_responses_freq
  submit = rsubmit$d_Target_responses_freq
  # make sure to have same labels in test data, as in train data 
  submit <- extend_levels_in_factors(train, submit)
  
  stopifnot(ncol(train) == ncol(valid))
  stopifnot(ncol(train) == ncol(submit)) # submit: has no Target but row-id

  if (is_apply_pca) {
    num_pca = 17
    rpca_train = convert_to_pca(train, num_pca=num_pca,pca=NULL)
    train = rpca_train$dpca
    valid = convert_to_pca(valid, num_pca=num_pca,pca=rpca_train$pca)$dpca
    submit = convert_to_pca(submit, num_pca=num_pca,pca=rpca_train$pca)$dpca
  }
  stopifnot(0 == sum(names(train) != names(valid)))
  stopifnot(1 ==sum(!names(train) %in% names(submit)))
  
  plot_hist(submit[names(submit) %in% get_num_variables_list(submit)],file_path_datatotest, jpgname="_outliers_testset_freq")
  
  r2 = run_modeling(train, valid, 
                    file_path="./datasets3_023_log", 
                    jpgname="_freq")
  temp = data_to_test_compare(train, submit, r2)
  df2 = data_to_test(r2, submit)
  write.csv(df2, file = paste(file_path_datatotest, "/df2.csv",sep=""))
  #===============
  # Expirement 4: group by factors into new Target-derived features
  train <- rtrain$d_Target_responses_freq
  
  # factors - to be computed using TRAN only, and added to VALID and SUBMIT
  factors <- get_factors_responses_target(train) 
  
  train <- add_factors_response_target(train, factors)
  
  valid <- rtest$d_Target_responses_freq
  valid <- extend_levels_in_factors(train, valid)
  valid <-  add_factors_response_target(valid, factors)
  
  submit = rsubmit$d_Target_responses_freq
  submit <- extend_levels_in_factors(train, submit)
  submit<-add_factors_response_target(submit, factors)
  
  stopifnot(ncol(train) == ncol(valid))
  stopifnot(ncol(train) == ncol(submit)) # submit: has no Target but row-id

  if (is_apply_pca) {
    num_pca = 22
    rpca_train = convert_to_pca(train, num_pca=num_pca,pca=NULL)
    train = rpca_train$dpca
    valid = convert_to_pca(valid, num_pca=num_pca,pca=rpca_train$pca)$dpca
    submit = convert_to_pca(submit, num_pca=num_pca,pca=rpca_train$pca)$dpca
  }
  stopifnot(0 == sum(names(train) != names(valid)))
  stopifnot(1 ==sum(!names(train) %in% names(submit)))
  
  plot_hist(submit[names(submit) %in% get_num_variables_list(submit)],file_path_datatotest, jpgname="_outliers_testset_freq")
  
  r3 = run_modeling(train, valid, 
                    file_path="./datasets4_023_log", 
                    jpgname="_response_target")
  temp = data_to_test_compare(train, submit, r3)
  df3 = data_to_test(r3, submit)
  write.csv(df3, file = paste(file_path_datatotest, "/df3.csv",sep=""))
  
  #==========================
  # Expirement 10: 
  # convert factors to one-hot encoding features
  # three data sets: train, validation (with Target) and resulting test (no Target)
  train = rtrain$d_Target
  valid = rtest$d_Target
  submit=rsubmit$d_Target
  
  stopifnot(ncol(train) == ncol(valid))
  stopifnot(ncol(train) == ncol(submit)) # submit: has no Target but row-id
  
  print (paste(" names in TRAIN and not in SUBMIT = ",names(train)[!names(train) %in% names(submit)],sep=""))
  print (paste(" names in SUBMIT and not in TRAIN = ",names(submit)[!names(submit) %in% names(train)],sep=""))
  
  # make one hot encoding
  train=cat_to_one_hot(train)
  # make sure to have same labels in test and train and submit data 
  valid <- extend_levels_in_factors(train, valid)
  valid=cat_to_one_hot(rtest$d_Target)
  # make sure to have same labels in test and train and submit data 
  submit <- extend_levels_in_factors(train, submit)
  submit=cat_to_one_hot(submit)
  
  stopifnot(ncol(train) == ncol(valid))
  stopifnot(ncol(train) == ncol(submit)) # submit: has no Target but row-id
  
  if (is_apply_pca) {
    num_pca = 50
    rpca_train = convert_to_pca(train, num_pca=num_pca,pca=NULL)
    train = rpca_train$dpca
    valid = convert_to_pca(valid, num_pca=num_pca,pca=rpca_train$pca)$dpca
    submit = convert_to_pca(submit, num_pca=num_pca,pca=rpca_train$pca)$dpca
  }
  stopifnot(0 == sum(names(train) != names(valid)))
  stopifnot(1 ==sum(!names(train) %in% names(submit)))
  
  plot_hist(submit[names(submit) %in% get_num_variables_list(submit)],file_path_datatotest, jpgname="_outliers_testset_freq")
  
  r5 = run_modeling(train, valid
                    ,file_path="./datasets6_023_log"
                    ,jpgname="_response_target_05"
                    , hidden_layers=c(50,25,15))
  temp = data_to_test_compare(train, submit, r5)
  df5 = data_to_test(r5, submit)
  write.csv(df5, file = paste(file_path_datatotest, "/df5.csv",sep=""))
}

# MAIN function: remove global variables from global enviroment
main()
