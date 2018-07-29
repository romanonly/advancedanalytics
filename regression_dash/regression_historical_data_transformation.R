make_target_sort_date <- function(d)
{
  #The target value to predict here is the total seconds value between 
  # `created_at` and `actual_delivery_time`. '
  x1 <- strptime(d$created_at, "%Y-%m-%d %H:%M:%OS")
  
  stopifnot("actual_delivery_time" %in% names(d))
    
  if ("actual_delivery_time" %in% names(d)) {
    x2 <- strptime(d$actual_delivery_time, "%Y-%m-%d %H:%M:%OS")
    dx=x2-x1 #difftime(x2-x1, unites="seconds")
    d$Target = floor(60*as.integer( as.numeric(dx)))#, units="secs") )
    #sort in time, most recent come first
  }
  d = d[rev(order(x1)),]
  #return (list(d=d, date_create_at=x1))
  return (d)
}
data_historical_prepare<-function(d)
{
  stopifnot (class(d$market_id) == "factor")# = as.factor(d$market_id)
  stopifnot (class(d$order_protocol) == "factor") #as.factor(d$order_protocol)
  #remove negative valriables
  #d$min_item_price = d$min_item_price - min(d$min_item_price)
  d = d[d$min_item_price>=0,]
  #replace long name
  names(d)[names(d)=="estimated_store_to_consumer_driving_duration"]="store_to_consumer_dt"
  names(d)[names(d)=="estimated_order_place_duration"] = "estim_order_place_dt"


  #remove non-consuisten data: negative dashers
  print(paste(" amount of negarive dashers data points=",nrow( d[d$total_onshift_dashers<0 | d$total_busy_dashers<0,]), sep=""))
  d = d[d$total_onshift_dashers>=0 & d$total_busy_dashers>=0,]
  
  #Log normal outliers
  d = data_historical_replace_log_transformation(d)
  
  if (FALSE) { # try to remove collinear predictors
  
  # Take care collinear features
  d$dashers_ratio = log(1.0+d$total_onshift_dashers / (1.0+d$total_busy_dashers))
  #hist(d$dashers_ratio)
  d$num_distinct_items = as.numeric(d$num_distinct_items / (1+d$total_items))
  d$total_items = as.numeric(d$subtotal / (1.0+d$total_items))
  d$max_item_price = as.numeric(d$max_item_price) - d$total_items
  d$min_item_price = d$total_items - as.numeric(d$min_item_price)
  
  d$total_onshift_dashers = d$total_onshift_dashers - d$total_busy_dashers
  }  
  #add new variables
  #d_Target_responses_freq$date_create_at = strptime(d_Target_responses_freq$created_at, "%Y-%m-%d %H:%M:%OS")
  
  d_Target = d
  # make Target - for train set, nothing for test set
  if ("actual_delivery_time" %in% names(d)) {
    d_Target <- make_target_sort_date(d) #date_create_at=ret$x1
    # remove 'actual_delivery_time'
    d_Target = d_Target[!names(d_Target) %in% c("actual_delivery_time")]
  }
  # responses for factors
  #d_Target_responses_freq <- add_factors_responses_frequencies(d_Target) # MUST BE APPLIED TO json !!!

  temp_date_create_at = strptime(d_Target$created_at, "%Y-%m-%d %H:%M:%OS")
  
  
  date_create_at = strptime(temp_date_create_at, "%Y-%m-%d %H:%M:%OS")
  #d_Target_responses_freq$created_weekday = as.integer(wday(date_create_at))#weekdays(x1)
  temp_weekday = as.integer(wday(date_create_at))#weekdays(x1)
  temp_hour = as.integer(format(date_create_at, "%H"))
  temp_min = as.integer(format(date_create_at, "%M"))
  temp_min2 = as.integer(minute(date_create_at))
  stopifnot(0 == sum(temp_min2-temp_min, na.rm=TRUE))
  
  
  d_Target$created_weekday = as.integer(temp_weekday)
  i60 = 1.0/60.0
  d_Target$created_hourmin = as.integer((12+as.integer(floor(temp_hour+0.5+i60*temp_min))) %% 24)
  d_Target$created_day = as.integer(format(date_create_at, "%d"))
  d_Target$created_month = as.integer(format(date_create_at, "%m"))
  
  return (d_Target)  
}

data_historical_replace_log_transformation <- function(dd)
{
  col_num_outliers=c("total_items", "subtotal", "num_distinct_items",
                     "min_item_price", "max_item_price", 
                     "estim_order_place_dt" ## estimates_order_place_duration, 
  )
  for (nm in names(dd)) {
    if ("integer" == class(dd[,nm]) | "numeric" == class(dd[,nm])) {
      #print(nm)
      #hist( (dd[,nm]),main=nm) #log makes more gaussian
      if (nm %in% col_num_outliers) {
        dd[, nm] = log(1.0 + (dd[, nm] - min(dd[, nm])))
      }
    }
  }
  return (dd)
}
#data_transformation<-function(data, remove_upper_quantile_anomaly=0.98, remove_upper_quantile_anomaly_sd_fraction=0.1)
data_remove_missing <-function(data)
{
  #remove missing variables
  print(paste(" Num of missing = ", sum(is.na(data)), sep=""))
  print(paste("============ missing variables[columns]=",names(data)[colSums(is.na(data)) > 0],sep=""))#dC=d[ , colSums(is.na(data)) == 0]
  #d = na.omit(data)
  for(i in 1:ncol(data)){
    if (sum(is.na(data[,i]))>0 & "factor" != class(data[,i])) {
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
    }
  }
  print(paste(" ---- fixed: Num of missing as mean = ", sum(is.na(data)), sep=""))
  d = na.omit(data)
  print(paste(" ---- removed after fixed: Num missing as mean = ", sum(is.na(d)), sep=""))
  print(paste("============ fixed as mean: missing variables in [columns]=",names(data)[colSums(is.na(data)) > 0],sep=""))#dC=d[ , colSums(is.na(data)) == 0]
  print(paste("============ removed: missing variables in [columns]=",names(d)[colSums(is.na(d)) > 0],sep=""))#dC=d[ , colSums(is.na(data)) == 0]
  return (d)
}
  
data_remove_outliers<-function(data, name_var="Target", remove_upper_quantile_anomaly=0.98, remove_upper_quantile_anomaly_sd_fraction=0.1, is_loss_log=TRUE)  
{
  d_Target = data
  stopifnot(name_var %in% names(d_Target))
  
  if (name_var %in% names(d_Target)) {
    tt = d_Target[, name_var]#$Target
    #summary( tt[tt<quantile(tt, 0.98)] )
    tmax = quantile(tt, remove_upper_quantile_anomaly)
    tmax = tmax + (tmax - mean(tt)) * remove_upper_quantile_anomaly_sd_fraction
    tmin = quantile(tt, 1.0-remove_upper_quantile_anomaly)
    tmin = tmin - (mean(tt) - tmin) * remove_upper_quantile_anomaly_sd_fraction
    
    print(paste(" data_transofrmation remove_quantile # = ", length(tt)-length(tt[tt<tmax & tt>tmin]),sep=""))
    d_Target = d_Target[tt<tmax & tt>tmin,]
    
    if (TRUE==is_loss_log) {
      #d_Target$Target = log(1.0 + d_Target$Target)
      d_Target[,name_var] = log(1.0 + d_Target[,name_var])
    }
    '    
    stopifnot("Target" %in% names(d_Target_responses_freq))
    stopifnot(1e-10 > abs(tt - d_Target_responses_freq$Target))
    d_Target_responses_freq = d_Target_responses_freq[tt<tmax & tt>tmin,]
    d_Target_responses_freq$Target = log(1.0 + d_Target_responses_freq$Target)'
  }
  
  return ((d_Target=d_Target))#, d_Target_responses_freq=d_Target_responses_freq))
}

#==============
plot_target_pred_lines <- function(data, file_path, jpgname=NULL, num_points = 1000, plot_nrow=2, plot_ncol=5, numer_names=NULL)
{
  if (!is.null(jpgname))
    graphics.off()
  
  op <- par(mfrow = c(plot_nrow, plot_ncol))
  
  num_plots = plot_nrow * plot_ncol
  d_numerical_only = data[names(data) %in% get_num_variables_list(data)]
  i = 0
  
  if (is.null(numer_names)) {
    nms = names(d_numerical_only)
  } else {
    nms = names(d_numerical_only[,names(d_numerical_only) %in% numer_names])
  }
  
  nsample = min(num_plots, length(nms))
  ind_nms = sample(length(nms),nsample)
  
  num_points = min(num_points, nrow(data))
  
  for (nm in nms[ind_nms]) {
    if (nm != "Target") {
      if (i <= num_plots) {
        train_index = sample(nrow(data),num_points)
        plot(data$Target[train_index] ~ data[train_index, nm], xlab=nm)
        i = i + 1
      }
    }
  }
  if (!is.null(jpgname)) {
    mypath <- file.path(file_path, paste("fig_target_predictors", jpgname, ".jpg", sep = ""))
    print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
}
#==============
plot_target_factor <- function(data, file_path, jpgname=NULL, num_points=1000, plot_nrow=2, plot_ncol=5, numer_names=NULL, target_name="Target",max_nlevels=30)
{
  if (!is.null(jpgname))
    graphics.off()
  
  op <- par(mfrow = c(plot_nrow, plot_ncol))
  
  num_plots = plot_nrow * plot_ncol
  
  if (is.null(numer_names)) {
    nms = names(data)
  } else {
    nms = names(data[names(data) %in% numer_names])
  }
  
  #nsample = min(num_plots, length(nms))
  #ind_nms = sample(length(nms),nsample)
  
  num_points = min(num_points, nrow(data))
  train_index = sample(nrow(data),num_points)
  
  data_target = data[train_index, target_name]
  i = 0
  for (nm in nms) { #[ind_nms]) {
    if (nm != "Target" & class(data[, nm]) == "factor" & i <= num_plots) {
          y = data[train_index, nm]
          if (nlevels(y) < max_nlevels) {
            #print(summary(y))
            plot(data_target ~ y, xlab=nm)
            i = i + 1
          }
    }
  }
  if (!is.null(jpgname)) {
    mypath <- file.path(file_path, paste("fig_target_predictors_factors", jpgname, ".jpg", sep = ""))
    print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
}


#=============== MODEL ============
run_model<-function(dtrain=NULL,dtest=NULL,jpgname=NULL,file_path=NULL,hidden_layers=c(25,20,15))
{
  #EDA: 
  data = dtrain
  
  plot_target_pred_lines(data, file_path, jpgname,n=100)
  
  d_numerical_only = data[names(data) %in% get_num_variables_list(data)]
  # Outliers to log
  jpgname = "_outliers"; plot_hist(d_numerical_only,file_path, jpgname)
  
  #d_numerical_only_log = replace_log_transformation(d_numerical_only)
  #jpgname = "_outliers_logged"; plot_hist(d_numerical_only_log,file_path, jpgname)
  #jpgname = "_correl_log"; plot_corr(d_numerical_only_log,file_path, jpgname)  
  
  # Correlation to target
  jpgname = "_correl"; plot_corr(d_numerical_only,file_path, jpgname)  
  
  
  # PCA
  # Factors only: 
  col_num_factors = c("market_id","store_primary_category","order_protocol")#,"store_id")
  d_factors = data[names(data) %in% col_num_factors]
  
  labels = as.numeric(d_numerical_only$Target - min(d_numerical_only$Target))
  labels = 1 + as.integer(20 * labels / max(labels))
  #if ("order_protocol" %in% names(d_factors)) { labels = d_factors$order_protocol }
  jpgname = "_numerical"
  ret = pca_numerical(pcadata=d_numerical_only, labels=labels, num_pca_comps = 10);  
  if (!is.null(ret$ret_err))
    pca_plot(ret,labels,file_path, jpgname)
  
  # Modeling
  #library(caret)
  #library(gbm)
  hist(d_numerical_only$Target)
  
  dtrain_num = dtrain[names(dtrain) %in% get_num_variables_list(dtrain)]
  dtest_num =  dtest[names(dtest) %in% get_num_variables_list(dtest)]
  
  r = modeling_ensemble(dtrain_num, dtest_num, 
                        split_ratio=0.8, 
                        is_timesorted_vs_sample=FALSE
                        ,hidden_layers=hidden_layers
                        ,file_path=file_path, 
                        jpgname=jpgname)
  
  # plot varimp
  if (!is.null(r$model_gbm)) {
    graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r$model_gbm),main="GBM - Variable Importance")
    mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
    print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  return(r)#list(model_gbm=r$model_gbm, model_glm=r$model_glm, model_h2o=r$model_))
  #======================
  #plot(na.omit(model_gbm$finalModel$fitted.values  - model_glm$finalModel$fitted.values))
  #install.packages('e1071', dependencies=TRUE).
}

add_factors_response_target<-function(dtrain, factors)
{
  nrow_train = nrow(dtrain)
  
  for (i in 1:length(factors)) {
    f = factors[[i]]
    #print(names(f))
    uf = f
    if (0<sum(duplicated(f[,-1]))) {
      #stopifnot(nrow(f) == nrow(unique(f[,-1])))
      #uf = unique(f[,-1])
      print(paste(" ***** duplicate values! #=", sum(duplicated(f[,-1])), sep=""))
      print(names(f))
      uf = f[!duplicated(f[,-1]),]
    }
    
    dtrain = dtrain %>% left_join( as.data.frame(uf) ) 
    stopifnot(nrow_train== nrow(dtrain))
    
    #dtest2=as.data.frame(f) %>% left_join(dtest, all.x=TRUE, sort= TRUE) )
    #dtest = dtest %>% left_join(df, all.x=TRUE)#, sort= TRUE) 
  }
  
  print(paste(" left_join missing #=",sum(is.na(dtrain)), sep=""))
  #library(imputeTS)
  #dtest = na.mean(dtest)
  for(i in 1:ncol(dtrain)){
    dtrain[is.na(dtrain[,i]), i] <- mean(dtrain[,i], na.rm = TRUE)
  }
  print(paste(" ... after mean-impute left_join missing #=",sum(is.na(dtrain)), sep=""))
  return(dtrain)
}
#========================
#==== add factors respionse of Target: take care of LEAKS
run_modeling<-function(dtrain2, 
                       dtest2, 
                       file_path="./datasetsAAA_BBB_nolog", 
                       jpgname="_jpgnameAAA",
                       hidden_layers=c(25,20,15))
{
  file_path_created = make_dir_sink_log_txt(file_path)#("/data_to_test_0013")
  #file_path=paste(".",dir_name,sep="")
  
  
  r3=run_model(dtrain2,dtest2,jpgname,file_path,hidden_layers=hidden_layers)
  
  if (!is.null(r3$model_gbm)) {
  graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r3$model_gbm),main="GBM - Variable Importance")
  mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  return (r3)
}
#===================== data_to_test
data_to_test<-function(r1, d_test_0)
{
  predictions_gbm<-predict.train(object=r1$model_gbm, d_test_0, type="raw")
  predictions_glm<-predict.train(object=r1$model_glm, d_test_0, type="raw")
  predictions_h2o = as.data.frame(h2o.predict(r1$h2o_model$model_h2o, as.h2o(d_test_0)))$predict
  
  graphics.off()
  op <- par(mfrow = c(1,3))
  hist(predictions_glm)
  hist(predictions_gbm)
  hist(predictions_h2o)
  
  print("gbm vs glm")
  ret = pred_errors(predictions_glm, predictions_gbm)
  print("gbm vs h2o")
  ret = pred_errors(predictions_gbm, predictions_h2o)
  dd = data.frame("gbm"=predictions_gbm, "glm"=predictions_glm,"h2o"=predictions_h2o)
  M = cor(as.matrix(dd))
  print(M)
  #plot(predictions_glm ~ predictions_gbm)
  #plot(pred_test_h2o$predict ~ predictions_gbm)
  return (list(predictions_glm=predictions_glm,predictions_gbm=predictions_gbm,predictions_h2o=predictions_h2o))
}

data_to_test_compare<-function(d_Target_train, d_test_0, r)
{
  print( names(d_Target_train)[!names(d_Target_train) %in% names(d_test_0)] )
  print( names(d_test_0)[!names(d_test_0) %in% names(d_Target_train)] )
  ret_test1 = data_to_test(r, d_test_0)
  return(ret_test1)
}
data_to_test<-function(r, d_test_0)
{
  data_to_test_1 = data.frame("delivery_id"=d_test_0$delivery_id)
  
  if (!is.null(r$model_gbm)) {
    data_to_test_1$prediction_gbm=exp(predict.train(object=r$model_gbm, d_test_0, type="raw"))
  }
  if (!is.null(r$model_glm)) {
    data_to_test_1$prediction_glm=exp(predict.train(object=r$model_glm, d_test_0, type="raw"))
  }
  if (!is.null(r$h2o_model$model_h2o)) {
    data_to_test_1$prediction_h2o=exp(as.data.frame(h2o.predict(r$h2o_model$model_h2o, as.h2o(d_test_0)))$predict)
  }
  M = cor(as.matrix(data_to_test_1[,names(data_to_test_1) %in% c("prediction_gbm","prediction_glm","prediction_h2o")]))
  print(M)
  return(data_to_test_1)
}
#=============================

read_train_submit_data<-function()
{
  #========= read  data =========================
  fn = paste("./datasets/historical_data.csv",sep="")
  print(fn)
  d <- read.csv(fn)
  d$market_id = as.factor(d$market_id)
  d$order_protocol = as.factor(d$order_protocol)
  
  #====== read data to test ========
  fn0 = paste("./datasets/", "data_to_predict.csv",sep="")
  print(fn0)
  d0 <- read.csv(fn0)
  d0$market_id = as.factor(d0$market_id)
  d0$order_protocol = as.factor(d0$order_protocol)
  
  d0 = d0[!names(d0) %in% c("platform")]
  
  print (paste(" names in test and not in train = ",names(d0)[!names(d0) %in% names(d)],sep=""))
  print (paste(" names in train and not in test = ",names(d)[!names(d) %in% names(d0)],sep=""))
  return (list(dtrain=d, dsubmit=d0))
}
