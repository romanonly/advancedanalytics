data_transformation<-function(d)
{
  d$market_id = as.factor(d$market_id)
  d$order_protocol = as.factor(d$order_protocol)
  #remove negative valriables
  #d$min_item_price = d$min_item_price - min(d$min_item_price)
  d = d[d$min_item_price>=0,]
  #replace long name
  names(d)[names(d)=="estimated_store_to_consumer_driving_duration"]="store_to_consumer_dt"
  names(d)[names(d)=="estimated_order_place_duration"] = "estim_order_place_dt"
  #remove missing variables
  
  data = d
  print(sum(is.na(data)))
  print(paste("============ missing variables[columns]=",names(data)[colSums(is.na(data)) > 0],sep=""))#dC=d[ , colSums(is.na(data)) == 0]
  #d = na.omit(data)
  for(i in 1:ncol(d)){
    d[is.na(d[,i]), i] <- mean(d[,i], na.rm = TRUE)
  }
  print(sum(is.na(d)))
  d = na.omit(d)
  print(paste("============ missing variables[columns]=",names(data)[colSums(is.na(data)) > 0],sep=""))#dC=d[ , colSums(is.na(data)) == 0]
  
  #remove non-consuisten data: negative dashers
  print(paste(" amount of negarive dashers data points=",nrow( d[d$total_onshift_dashers<0 | d$total_busy_dashers<0,]), sep=""))
  d = d[d$total_onshift_dashers>=0 & d$total_busy_dashers>=0,]
  
  #Log normal outliers
  d = replace_log_transformation(d)
  
  # Take care collinear features
  d$dashers_ratio = log(1.0+d$total_onshift_dashers / (1.0+d$total_busy_dashers))
  #hist(d$dashers_ratio)
  d$num_distinct_items = as.numeric(d$num_distinct_items / (1+d$total_items))
  d$total_items = as.numeric(d$subtotal / (1.0+d$total_items))
  d$max_item_price = as.numeric(d$max_item_price) - d$total_items
  d$min_item_price = d$total_items - as.numeric(d$min_item_price)
  
  d$total_onshift_dashers = d$total_onshift_dashers - d$total_busy_dashers
  
  
  # Target
  d_Target <- make_target_sort_date(d) #date_create_at=ret$x1
  # remove 'actual_delivery_time'
  d_Target = d_Target[!names(d_Target) %in% c("actual_delivery_time")]
  
  # responses for factors
  d_Target_responses_freq <- add_factors_responses_frequencies(d_Target) # MUST BE APPLIED TO json !!!
  
  #add new variables
  #d_Target_responses_freq$date_create_at = strptime(d_Target_responses_freq$created_at, "%Y-%m-%d %H:%M:%OS")
  date_create_at = strptime(d_Target_responses_freq$created_at, "%Y-%m-%d %H:%M:%OS")
  d_Target_responses_freq$created_weekday = as.integer(wday(date_create_at))#weekdays(x1)
  temp_hour = as.integer(format(date_create_at, "%H"))
  temp_min = as.integer(format(date_create_at, "%M"))
  temp_min2 = as.integer(minute(date_create_at))
  stopifnot(0 == sum(temp_min2-temp_min))
  i60 = 1.0/60.0
  d_Target_responses_freq$created_hourmin = as.integer((12+as.integer(floor(temp_hour+0.5+i60*temp_min))) %% 24)
  d_Target_responses_freq$created_day = as.integer(format(date_create_at, "%d"))
  d_Target_responses_freq$created_month = as.integer(format(date_create_at, "%m"))
  
  if ("Target" %in% names(d_Target)) {
    d_Target$Target = log(1.0 + d_Target$Target)
    stopifnot("Target" %in% names(d_Target_responses_freq))
    d_Target_responses_freq$Target = log(1.0 + d_Target_responses_freq$Target)
  }
  
  return (list(d_Target=d_Target, d_Target_responses_freq=d_Target_responses_freq))
}

#==============
plot_target_pred_lines <- function(data, file_path, jpgname, n = 1000)
{
  graphics.off()
  op <- par(mfrow = c(3,5))
  
  d_numerical_only = data[names(data) %in% get_num_variables_list(data)]
  i = 0
  nms = names(d_numerical_only)
  nsample = min(15, length(nms))
  ind_nms = sample(length(nms),nsample)
  for (nm in nms[ind_nms]) {
    if (nm != "Target") {
      if (i < 15) {
        train_index = sample(nrow(data),n)
        plot(data$Target[train_index] ~ data[train_index, nm], xlab=nm)
      }
      i = i + 1
    }
  }
  mypath <- file.path(file_path, paste("fig_target_predictors", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
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
  labels = 1 + as.integer(5 * labels / max(labels))
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
  
  graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r3$model_gbm),main="GBM - Variable Importance")
  mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
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
  data_to_test_1 = data.frame("delivery_id"=d_test_0$delivery_id, 
                              "prediction_gbm"=exp(predict.train(object=r$model_gbm, d_test_0, type="raw"))
                              ,"prediction_glm"=exp(predict.train(object=r$model_glm, d_test_0, type="raw"))
                              ,"prediction_h2o"=exp(as.data.frame(h2o.predict(r$h2o_model$model_h2o, as.h2o(d_test_0)))$predict)
  )
  M = cor(as.matrix(data_to_test_1[,c(2,3,4)]))
  print(M)
  return(data_to_test_1)
}
extend_levels_in_factors<-function(d_Target_train, d_to_be_extended)
{
  d_test_0 = d_to_be_extended
  for (nm in c("order_protocol", "market_id","store_primary_category")) {
    if (length(levels(d_Target_train[,nm]))>length(levels(d_test_0[,nm]))) {
      print(paste(" missing lavels in ",nm,sep=""))
      d_test_0[,nm] <- factor(d_test_0[,nm], levels=levels(d_Target_train[,nm]))
      #levels(ff) <- c(levels(ff), "fgh")
    }
    print (sum(!levels(d_Target_train[,nm]) %in% levels(d_test_0[,nm])))
  }
  return (d_test_0)
}
#=============================
cat_to_one_hot<-function(dd)
{
  dret = dd
  for (nm in c("order_protocol", "market_id","store_primary_category")) {
    dd0 <- subset(dd,select=nm)
    
    data = model.matrix(~.-1,dd0)#,CLASS=dd$CLASS)
    # must add noise to deal with correlations
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -0.01, 0.01), dim(data)[1])
    noisified <- data + noise
    
    dd1 = data.frame(noisified)
    dret = cbind(dret, dd1)
  }
  'dd2 = with(dd1,
  data.frame(model.matrix(~RACE-1,dd),
  store_primary_category,market_id,order_protocol))'
  return (dret)
}

read_train_submit_data<-function()
{
  #========= read  data =========================
  fn = paste("./datasets/historical_data.csv",sep="")
  print(fn)
  d <- read.csv(fn)
  #====== read data to test ========
  fn0 = paste("./datasets/", "data_to_predict.csv",sep="")
  print(fn0)
  d0 <- read.csv(fn0)
  
  d0 = d0[!names(d0) %in% c("platform")]
  
  print (paste(" names in test and not in train = ",names(d0)[!names(d0) %in% names(d)],sep=""))
  print (paste(" names in train and not in test = ",names(d)[!names(d) %in% names(d0)],sep=""))
  return (list(dtrain=d, dsubmit=d0))
}
