# find label of the minority class
find_minority_label<-function(x) { #train_data, classes) {
  stopifnot(2 == nlevels(x)) # train_data[,classes]))
  malignant=levels(x)[1] #train_data[,classes])[1]
  #if (sum(train_data[,classes]==malignant) > sum(train_data[,classes]!=malignant))
  #  malignant=levels(train_data[,classes])[2]
  if (sum(x==malignant) > sum(x!=malignant))
    malignant=levels(x)[2]
  stopifnot(malignant %in% levels(x))#train_data[,classes]))
  return (malignant)
}

#
print_models <- function(file_path, models, cm_list, test_data)
{
  #
  # Plot Confidence intervals as resampling
  #
  mypath <- file.path(file_path, paste("resample_", "all", ".jpg", sep = ""))
  print(mypath)
  graphics.off()
  resampling <- resamples(models)
  my_plot = bwplot(resampling)
  print(my_plot);  dev.copy(jpeg,filename=mypath);  dev.off()
  #print( summary(resampling, metric = "ROC") )
  
  #
  # Plot confusion matrix metrics for 0.5-threshold
  #
  comparison <- plot_results(models, cm_list)
  pic1=
    comparison %>%
    gather(x, y, Sensitivity:F1) %>%
    ggplot(aes(x = x, y = y, color = model)) +
    geom_jitter(width = 0.3, alpha = 0.7, size = 3)
  
  filename=file.path(file_path, paste("comparison_confusion_matrix_", "all", ".jpg", sep = ""))
  ggsave(filename, pic1)
  assign(filename, pic1, envir=.GlobalEnv)
  
  #
  # Plot COnficence Intervals (CI) of ROC as lower-ROC-upper bounds
  #
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
  #cc = c("original", "under", "over", "smote", "rose", "glm")
  model_names = row.names(outside_test)
  print(model_names)
  outside_test = cbind(outside_test, model = model_names) #row.names(outside_test))
  outside_test <- as.data.frame(outside_test)
  print(outside_test)
  
  p1 =
    outside_test %>%
    gather(x, y, lower:upper) %>%
    ggplot(aes(x = x, y = y, color = model)) +
    geom_jitter(width = 0.3, alpha = 0.7, size = 3)
  
  filename=file.path(file_path, paste("ROC_test_CI_", "all", ".jpg", sep = ""))
  print(filename)
  ggsave(filename, p1)
  assign(filename, p1, envir=.GlobalEnv)
  
  #
  # Plot Lift and calibration
  #
  
  lift_results0 <- data.frame(Class = test_data$classes)
  class1 = "benign"
  print(model_names)
  
  for (mname0 in model_names) {
    print(mname0)
    if (mname0 %in% names(models)) {
      stopifnot(exists(mname0, where=models))
      stopifnot(!(is.null(models[[mname0]])))
      model0 = models[[mname0]]
      lift_results0[[mname0]] <- predict(model0, newdata = test_data, type = "prob")[,class1]
      
      if (!exists("lift_formula"))
        lift_formula = paste('Class ~ ', mname0, sep="")
      else 
        lift_formula = paste(lift_formula, mname0, sep=" + ")
      print(lift_formula)
    }
  }
  lift_formula = as.formula(lift_formula)
  print(lift_formula)
  
  lift_results = lift_results0
  print( head(lift_results, 7) )
  
  #
  # Lift
  #
  trellis.par.set(caretTheme())
  lift_obj <- lift(lift_formula, data = lift_results) #test_data) 
  pic000 <- ggplot(lift_obj, values = 60)
  # plot(lift_obj, values = 60, auto.key = list(columns = 3,lines = TRUE,points = FALSE))
  filename=file.path(file_path, paste("lift_results_", "all", ".jpg", sep = ""))
  print(filename)
  ggsave(filename, pic000)
  
  
  cal_obj <- calibration(lift_formula,data = lift_results,cuts = 5) 
  pic001 <- ggplot(cal_obj)
  filename=file.path(file_path, paste("calibration_results_", "all", ".jpg", sep = ""))
  ggsave(filename, pic001)
  
  filename=file.path(file_path, paste("calibration_results_", "all_curves", ".jpg", sep = ""))
  print(filename)
  #jpeg(file = filename)
  graphics.off()
  my_plot = plot(cal_obj, type = "l", auto.key = list(columns = 3,lines = TRUE, points = FALSE))
  print(my_plot)
  dev.copy(jpeg,filename=filename)
  dev.off()
}

# Kolmogorov-Smirnov
plot_KS<-function(model_rf, test_data, train_data,  
                  file_path="~//", jpgname=NULL,
                  train_name="Train",test_name="Validation"
                  , classes = "Target")
{
  malignant = find_minority_label(train_data[, classes]) #train_data[,classes]))
  
  x <- predict(model_rf,newdata = test_data,type="prob")[,malignant]
  y <- 1 - x
 
  print ( ks.test(x, y, alternative = "less") ) #"two.sided" "greater")
  
  # simulate two distributions - your data goes here!
  sample1 <- x #rnorm(10000, 10, 5)
  sample2 <- y # rnorm(10000, 1, 5)
  group <- c(rep("minor", length(sample1)), rep("major", length(sample2)))
  dat <- data.frame(KSD = c(sample1,sample2), group = group)
  # create ECDF of data
  cdf1 <- ecdf(sample1) 
  cdf2 <- ecdf(sample2) 
  # find min and max statistics to draw line between points of greatest distance
  minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
  x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
  y0 <- cdf1(x0) 
  y1 <- cdf2(x0)   

  if (!is.null(jpgname)) {
    graphics.off()
    mypath <- file.path(file_path, paste("plotROC_", jpgname, ".jpg", sep = ""))
    mypath_png <- file.path(file_path, paste("plotROC_", jpgname, ".png", sep = ""))
    png(file = mypath_png, width = 1024, height = 768, type="cairo-png")
  }
  
  ggplot(dat, aes(x = KSD, group = group, color = group))+
    stat_ecdf(size=1) +
    theme_bw(base_size = 28) +
    theme(legend.position ="top") +
    xlab("Sample") +
    ylab("ECDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
    ggtitle("K-S Test: Class Minor / Class Major 2") +
    theme(legend.title=element_blank())
  #gbm2.ROC <- roc(predictor=gbm2.probs, response=test_data[,classes], levels=levels(test_data[,classes]))
  #gbm3.probs <- predict(model_rf,newdata = train_data,type="prob")[,malignant]
  if (!is.null(jpgname)) {
    print(mypath);dev.copy(jpeg,filename=mypath);dev.off()
    #paste("plotROC_", jpgname, ".jpg", sep = ""));
  }
  
  
  #set.seed(15)
  dd <- data.frame(x=x)#x=rnorm(30))
  ddy<- data.frame(y=y)
  ggplot(dd, aes(x)) +
    stat_ecdf()#  
    #+stat_function(fun = pnorm, colour = "red")
  
  #You can find the maximal distance if you like with
  
  ed <- ecdf(dd$x)
  maxdiffidx <- which.max(abs(ed(dd$x)-pnorm(dd$x)))
  maxdiffat <- dd$x[maxdiffidx]
  
  #and add that to the plot with
  
  ggplot(dd, aes(x)) + 
    geom_line(aes(y = x, colour = "var0")) + 
    geom_line(aes(y = y, colour = "var1"))+
    stat_ecdf() 
    #stat_function(fun = pnorm, colour = "red") + 
    #+geom_vline(x=maxdiffat, xintercept=maxdiffidx,lty=2)
  
  
  if (FALSE){
      # Do x and y come from the same distribution?
      ks.test(x, y)
      # Does x come from a shifted gamma distribution with shape 3 and rate 2?
      ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
      ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
      ks.test(x+2, "pgamma", 3, 2, alternative = "gr")
      
      # test if x is stochastically larger than x2
      x2 <- rnorm(50, -1)
      plot(ecdf(x), xlim = range(c(x, x2)))
      plot(ecdf(x2), add = TRUE, lty = "dashed")
      t.test(x, x2, alternative = "g")
      wilcox.test(x, x2, alternative = "g")
      ks.test(x, x2, alternative = "l")
  }
}
plot_ROC <- function(model_rf, test_data, train_data,  
                     file_path="~//", jpgname=NULL,
                     train_name="Train",test_name="Validation",classes="Target")
{
  #model_rf=model_glm; test_data=cvalid; train_data=ctrain;classes = "Target";test_predictions=NULL; train_predictions=NULL;train_name = 'train';test_name = 'validation';jpgname=NULL
  result <- list()
  rfFit <- model_rf
  selectedIndices <- rfFit$pred$mtry == 2
  
  malignant = find_minority_label(train_data[, classes])
    #Draw the ROC curve 
    
    gbm2.probs <- predict(model_rf,newdata = test_data,type="prob")[,malignant]
    
    gbm2.ROC <- roc(predictor=gbm2.probs, response=test_data[,classes], levels=levels(test_data[,classes]))
    test_auc = gbm2.ROC$auc
    
    gbm3.probs <- predict(model_rf,newdata = train_data,type="prob")[,malignant]
    
    gbm3.ROC <- roc(predictor=gbm3.probs, response=train_data[,classes], levels=levels(train_data[,classes]))
    train_auc = gbm3.ROC$auc
    
    
    roc_title = paste("AUC_test =", round(test_auc, 3), "AUC_train=", round(train_auc, 3))
    print(roc_title)
    
    "
    x11()
    dev.off() only works in an interactive session. If you're interested in implementing such behavior in a script, you should use
    "
    if (!is.null(jpgname))
      graphics.off()
    
    #par(fig=c(0,0.8,0,0.8))
    op <- par(mfrow = c(1,1))
    #op <- par(mfrow = c(1, 1))
    plot(gbm3.ROC,main=roc_title,col='blue')
    #par(fig=c(0,0.8,0,0.8), new=TRUE)
    par(new=TRUE)
    #op <- par(mfrow = c(1, 1), new=TRUE)
    plot(gbm2.ROC,col='red')#,main=as.character(gbm2.ROC$auc))

    legend('topright', 
           legend=c(train_name,test_name) , 
           lty=c(1,2,3), 
           bty='n', cex=2.75, 
           col=c('blue', 'red'))
    
    if (!is.null(jpgname)) {
      mypath <- file.path(file_path, paste("plotROC_", jpgname, ".jpg", sep = ""))
      print(mypath);dev.copy(jpeg,filename=mypath);dev.off()
      #paste("plotROC_", jpgname, ".jpg", sep = ""));
    }
    #histogram(~gbm2.probs$malignant|test_data$classes,xlab="Histogram misbalance")  
    result <- list(auc_test = gbm2.ROC$auc, auc_train = gbm3.ROC$auc)

  return(result)
}

make_gain_curves <- function(score, cls_num) 
{
  #library(ROCR)
  #data(ROCR.simple)
  # str(ROCR.simple$labels) numerical !!!
  #pred <- ROCR::prediction( ROCR.simple$predictions, ROCR.simple$labels)
  #perf <- ROCR::performance(pred,"tpr","fpr")
  #perf = ROCR::performance(pred, "tpr", "rpp") #Plotting gain chart
  #plot(perf)  
  
  #cls_num = -1 + as.numeric(cls0)
  stopifnot("numeric" == class(cls_num))
  stopifnot(abs(min(cls_num)) < 1e-10 & (abs(1-max(cls_num)) < 1e-10))
  stopifnot(sum(score<0) == 0)
  
  pred = ROCR::prediction(score, cls_num)
  
  perf = ROCR::performance(pred, "tpr", "rpp") # Plotting gain chart
  #plot(perf)  
  perf.x = unlist(slot(perf, 'x.values'))
  perf.y = unlist(slot(perf, 'y.values'))
  gain2 = data.frame(x = perf.x, y = perf.y)
  
  
  ## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
  perf <- ROCR::performance(pred,"tpr","fpr")
  #plot(perf)  
  perf.x = unlist(slot(perf, 'x.values'))
  perf.y = unlist(slot(perf, 'y.values'))
  roc2 = data.frame(x = perf.x, y = perf.y)
  
  ## precision/recall curve (x-axis: recall, y-axis: precision)
  perf <- performance(pred, "prec", "rec")
  perf.x = unlist(slot(perf, 'x.values'))
  perf.y = unlist(slot(perf, 'y.values'))
  recall_precision = data.frame(x = perf.x, y = perf.y)
  #plot(perf)
  ## sensitivity/specificity curve (x-axis: specificity,
  ## y-axis: sensitivity)
  perf <- performance(pred, "sens", "spec")
  #plot(perf)
  perf.x = unlist(slot(perf, 'x.values'))
  perf.y = unlist(slot(perf, 'y.values'))
  sens_spec = data.frame(x = perf.x, y = perf.y)

  return (list(gain=gain2, roc=roc2, recall_precision=recall_precision, sens_spec=sens_spec))
}
plot_gain <- function(model_rf=NULL, test_data=NULL, train_data=NULL, 
                      test_predictions=NULL, train_predictions=NULL,
                      train_name = 'train',
                      test_name = 'validation',
                      file_path="~//", 
                      jpgname=NULL, #'gain_model_rf',
                      classes = "Target") 
{ 
  #model_rf=model_glm; test_data=cvalid; train_data=ctrain;classes = "Target";test_predictions=NULL; train_predictions=NULL;train_name = 'train';test_name = 'validation'
  malignant = find_minority_label(train_data[, classes])
  malignant_test = find_minority_label(test_data[, classes])
  stopifnot(malignant == malignant_test)
  
  if (is.null(test_predictions)) {
    stopifnot(!is.null(model_rf))
    test_predictions = predict(model_rf, newdata = test_data, type = "prob")
    test_predictions = test_predictions[,malignant]
  }
  stopifnot(ncol(test_predictions) == 1)
  final_test <- data.frame(actual = test_data[, classes], malignant = test_predictions)
  
  if (is.null(train_predictions)) {
    train_predictions = predict(model_rf, newdata = train_data, type = "prob")
    train_predictions = train_predictions[,malignant]
  } 
  stopifnot(ncol(train_predictions) == 1)
  final_train <- data.frame(actual = train_data[,classes],malignant = train_predictions)


  gain_test <- make_gain_curves(score =  final_test[,"malignant"], cls = as.numeric((test_data[,classes] == malignant))) #cls = as.array(test_data[,classes])) 
  gain_train <- make_gain_curves(score =  final_train[,"malignant"], cls = as.numeric((train_data[,classes] == malignant)))# cls = as.array(train_data[,classes])) 
  
  if (!is.null(jpgname))
    graphics.off()
  
  plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
       ylab="True Positive Rate", 
       xlab="Rate of Positive Predictions")
  
  lines(x=gain_test$gain$x, y=gain_test$gain$y, col="green", lwd=2)
  lines(x=gain_train$gain$x, y=gain_train$gain$y, col="blue", lwd=2)
  
  legend('topright', 
         legend=c('random', test_name, train_name) , 
         lty=c(1,2,3), 
         bty='n', cex=2.75, 
         col=c('red', 'green', 'blue'))
  if (!is.null(jpgname)) {
    mypath <- file.path(file_path, paste("gain_", jpgname, ".jpg", sep = ""))
    print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  
  result <- list(gaintestx=gain_test$gain$x, gaintesty=gain_test$gain$y, gaintrain=gain_train$gain
                 , roc_test=gain_test$roc
                 , recall_precision_test=gain_test$recall_precision
                 , sens_spec_test=gain_test$sens_spec)
  
  return(result)
}

experement<-function(train, valid, submit
                     ,num_pca
                     ,file_path_datatotest
                     ,df_name="df7.csv"
                     ,file_path="./datasets7_024_log"
                     ,jpgname="_freq_hotkey")
{
  stopifnot(0 == sum(names(train) != names(valid)))
  stopifnot(1 ==sum(!names(train) %in% names(submit)))
  
  stopifnot(ncol(train) == ncol(valid))
  stopifnot(ncol(train) == ncol(submit)) # submit: has no Target but row-id
  
  if (num_pca > 0) {
    rpca_train = convert_to_pca(train, num_pca=num_pca,pca=NULL)
    train = rpca_train$dpca
    valid = convert_to_pca(valid, num_pca=num_pca,pca=rpca_train$pca)$dpca
    submit = convert_to_pca(submit, num_pca=num_pca,pca=rpca_train$pca)$dpca
  }
  stopifnot(0 == sum(names(train) != names(valid)))
  stopifnot(1 ==sum(!names(train) %in% names(submit)))
  
  r7 = run_modeling(train, valid
                    #,train[sample(nrow(train),10000),], valid[sample(nrow(valid),5000),]
                    ,file_path=file_path#"./datasets7_025_log"
                    ,jpgname=jpgname
                    , hidden_layers=c(40))
  temp = data_to_test_compare(train, submit, r7)
  df7 = data_to_test(r7, submit)
  write.csv(df7, file = paste(file_path_datatotest, df_name,sep="/"))  
  
  plot_hist(submit[names(submit) %in% get_num_variables_list(submit)],file_path_datatotest, jpgname="_outliers_submit")
  plot_hist(train[names(train) %in% get_num_variables_list(train)],file_path_datatotest, jpgname="_outliers_train")
  plot_hist(valid[names(valid) %in% get_num_variables_list(valid)],file_path_datatotest, jpgname="_outliers_valid")
  
  return(list(df=df7, r=r7, train=train, valid=valid, valid=valid))    
}

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

make_dir_sink_log_txt <- function(output_dir_name)
{
  
  # output re-directed in cwd. output is appended
  # to existing file. output also send to terminal. 
  #file_path=paste(PATHNAME, output_dir_name, sep="")
  #dir.create(file_path) # file.path(mainDir, subDir))
  dir.create(output_dir_name) # file.path(mainDir, subDir))
  
  mypath <- file.path(output_dir_name, paste("report_logs", ".txt", sep = ""))
  sink(mypath, append=FALSE, split=TRUE)
  return(mypath)
}
split_data<-function(data, split_ratio = 0.8, is_timesorted_vs_sample = FALSE)
{
  n = floor(split_ratio*nrow(data))
  if (is_timesorted_vs_sample) {
    train_index = 1:n # take latest weeks 
  } else {
    train_index = sample(nrow(data),n)
  }
  dtrain = data[train_index,]
  dtest = data[-train_index,]
  return (list(dtrain=dtrain, dtest=dtest))
}

plot_hist<-function(dd,file_path, jpgname=NULL, plot_nrow=3, plot_ncol=6)
{
  if (!is.null(jpgname))
      graphics.off()
  op <- par(mfrow = c(plot_nrow, plot_ncol))
  i = 1
  for (nm in names(dd)) {
    if ("integer" == class(dd[,nm]) | "numeric" == class(dd[,nm])) {
      #print(nm)
      if (i <= plot_nrow*plot_ncol) 
        hist( (dd[,nm]),main=nm) #log makes more gaussian
        i = 1 + i
    }
  }
  if (!is.null(jpgname)) {
    mypath <- file.path(file_path, paste("eda_hist_all", jpgname, ".jpg", sep = ""))
    print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
}

#===========================
get_num_variables_list<-function(dd) 
{
  col_num = c()
  for (nm in names(dd)) {
    if ("integer" == class(dd[,nm]) || "numeric" == class(dd[,nm])) {
      col_num = c(col_num, nm)
    }
  }
  return(col_num)
}

# factor variables: add reponses as mean Target
# Use:
# r = get_factors_responses_target(d0)
# d0 = add_factors_responses_target(d0,r$dfactor1,r$dfactor2,r$dfactor3,r$dfactor4) 
get_factors_responses_target <- function(d0) 
{
  stopifnot("Target" %in% names(d0))

  ret = list()  
  #store_primary_category
  dfactor1 <- d0 %>% 
    dplyr::group_by(store_primary_category) %>% 
    dplyr::summarize(store_primary_category_target=mean(Target))
  ret[["dfactor1"]]=dfactor1
  dfactor11 <- d0 %>% 
    dplyr::group_by(store_primary_category, created_weekday) %>% 
    dplyr::summarize(store_primary_category_target_W=mean(Target))
  ret[["dfactor11"]]=dfactor11
  dfactor12 <- d0 %>% 
    #dplyr::group_by(store_primary_category, created_weekday, created_hourmin) %>%
    dplyr::group_by(store_primary_category, created_hourmin) %>% 
    dplyr::summarize(store_primary_category_target_H=mean(Target))
  ret[["dfactor12"]]=dfactor12
  
  #market_id
  dfactor2 <- d0 %>% 
    dplyr::group_by(market_id) %>% 
    dplyr::summarize(market_id_target= mean(Target))
  ret[["dfactor2"]]=dfactor2
  dfactor21 <- d0 %>% 
    dplyr::group_by(market_id, created_weekday) %>% 
    dplyr::summarize(market_id_target_W= mean(Target))
  ret[["dfactor21"]]=dfactor21
  dfactor22 <- d0 %>% 
    #dplyr::group_by(market_id, created_weekday, created_hourmin) %>%
    dplyr::group_by(market_id, created_hourmin) %>% 
    dplyr::summarize(market_id_target_H= mean(Target))
  ret[["dfactor22"]]=dfactor22
  dfactor23 <- d0 %>% 
    dplyr::group_by(market_id, created_month, created_day) %>%
    dplyr::summarize(market_id_target_MD= mean(Target))
  #ret[["dfactor23"]]=dfactor23
  dfactor24 <- d0 %>% 
    dplyr::group_by(market_id, created_month, created_day, created_hourmin) %>%
    dplyr::summarize(market_id_target_MDH= mean(Target))
  #ret[["dfactor24"]]=dfactor24
  dfactor25 <- d0 %>% 
    dplyr::group_by(market_id, created_weekday, created_hourmin) %>%
    dplyr::summarize(market_id_target_WH= mean(Target))
  #ret[["dfactor25"]]=dfactor25
  dfactor26 <- d0 %>% 
    dplyr::group_by(market_id, created_hourmin) %>%
    dplyr::summarize(market_id_target_H= mean(Target))
  ret[["dfactor26"]]=dfactor26
  
  # store_id
  dfactor3 <- d0 %>% 
    dplyr::group_by(store_id) %>% 
    dplyr::summarize(store_id_target=mean(Target))
  #ret[["dfactor3"]]=dfactor3
  dfactor31 <- d0 %>% 
    dplyr::group_by(store_id, created_weekday) %>% 
    dplyr::summarize(store_id_target_W=mean(Target))
  #ret[["dfactor31"]]=dfactor31
  dfactor32 <- d0 %>% 
    dplyr::group_by(store_id, created_weekday,created_hourmin) %>% 
    dplyr::summarize(store_id_target_WH=mean(Target))
  #ret[["dfactor32"]]=dfactor32
  dfactor33 <- d0 %>% 
    dplyr::group_by(store_id, created_month, created_day,created_hourmin) %>% 
    dplyr::summarize(store_id_target_MDH=mean(Target))
  #ret[["dfactor33"]]=dfactor33
  dfactor34 <- d0 %>% 
    dplyr::group_by(store_id, created_hourmin) %>% 
    dplyr::summarize(store_id_target_H=mean(Target))
  #ret[["dfactor34"]]=dfactor34

    
  # order_protocol
  dfactor4 <- d0 %>% 
    dplyr::group_by(order_protocol) %>% 
    dplyr::summarize(order_protocol_target= mean(Target)) 
  ret[["dfactor4"]]=dfactor4
  return (ret) 
  'list(dfactor1=dfactor1,dfactor11=dfactor11, dfactor12=dfactor12,  
              dfactor2=dfactor2,dfactor21=dfactor21, dfactor22=dfactor22,  
              dfactor3=dfactor3, #dfactor31=dfactor31, dfactor32=dfactor32,  
              dfactor4=dfactor4))'
}
#
# factor variables: 
#
extend_levels_in_factors_new<-function(d_Target_train, d_to_be_extended, 
                                       factor_names=c()) #("order_protocol", "market_id","store_primary_category"))
{
  d_test_0 = d_to_be_extended
  for (nm in factor_names) {
    if (length(levels(d_Target_train[,nm]))>length(levels(d_test_0[,nm]))) {
      print(paste(" missing lavels in ",nm,sep=""))
      d_test_0[,nm] <- factor(d_test_0[,nm], levels=levels(d_Target_train[,nm]))
      #levels(ff) <- c(levels(ff), "fgh")
    }
    print (paste(nm, " variable missing levels = ", sum(!levels(d_Target_train[,nm]) %in% levels(d_test_0[,nm])),sep=""))
  }
  return (d_test_0)
}

cat_to_one_hot<-function(dd, factors_names = c())
{
  dret = dd
  for (nm in factors_names) {
    print(paste("one hot in factor=", nm, sep=""))
    #dd0 <- subset(dd,select=nm)
    dd0 <- dd %>% select_(.dots = list(nm))
    
    data = model.matrix(~.-1, dd0)#,CLASS=dd$CLASS)
    
    # must add noise to deal with correlations
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -0.001, 0.001), dim(data)[1])
    noisified <- data + noise
    
    dd1 = data.frame(noisified)
    dret = cbind(dret, dd1)
  }
  'dd2 = with(dd1,
  data.frame(model.matrix(~RACE-1,dd),
  store_primary_category,market_id,order_protocol))'
  return (dret)
}

# factor variables: add reponses as frequencies
add_factors_responses_frequencies <- function(d0, factor_names = c("store_primary_category", "market_id", "store_id", "order_protocol")) 
{
  nrow_d0 = nrow(d0)
  nrow1 = 1e6 / nrow(d0) # scaling factor
  
  d_factor = d0
  
  for (nm in factor_names) {
    #store_primary_category
    new_nm = paste(nm, "_num", sep="")
    print(paste(" added new numerical variable: ", new_nm, sep=""))
    
    dfactor <- d0 %>% 
      dplyr::group_by_(.dots = nm) %>% 
      dplyr::summarize_(.dots = setNames('n()', new_nm)) #new_nm=n())
    
    d0 = d0 %>% left_join(as.data.frame(dfactor))
    
    stopifnot(nrow_d0 == nrow(d0))
    d0[, new_nm] = as.numeric(nrow1 * d0[, new_nm])
  }

  return(d0)
}

add_factors_responses_frequencies_bak <- function(d0) 
{
  #store_primary_category
  dfactor1 <- d0 %>% 
    dplyr::group_by(store_primary_category) %>% 
    dplyr::summarize(store_primary_category_num=n())
              #store_primary_category_estim=mean(estim_order_place_dt,na.rm=TRUE))
  #market_id
  dfactor2 <- d0 %>% 
    dplyr::group_by(market_id) %>% 
    dplyr::summarize(market_id_num=n())
  # store_id
  dfactor3 <- d0 %>% 
    dplyr::group_by(store_id) %>% 
    dplyr::summarize(store_id_num=n())
  # order_protocol
  dfactor4 <- d0 %>% 
    dplyr::group_by(order_protocol) %>% 
    dplyr::summarize(order_protocol_num=n()) 
  
  nrow_d0 = nrow(d0)
  d0 = d0 %>% left_join(as.data.frame(dfactor1))
  d0 = d0 %>% left_join(as.data.frame(dfactor2))
  d0 = d0 %>% left_join(as.data.frame(dfactor3))
  d0 = d0 %>% left_join(as.data.frame(dfactor4))
  stopifnot(nrow_d0 == nrow(d0))

  nrow1 = 1e6 / nrow(d0) # scaling factor
  
  d0$store_primary_category_num = as.integer(nrow1 * d0$store_primary_category_num)
  d0$market_id_num = as.integer(nrow1 * d0$market_id_num)
  d0$store_id_num = as.integer(nrow1 * d0$store_id_num)
  d0$order_protocol_num = as.integer(nrow1 * d0$order_protocol_num)
  
  return(d0)
}
# plot 
plot_corr <- function(dd,file_path, jpgname=NULL)  
{
  col_num = get_num_variables_list(dd)
  M = cor(as.matrix(dd[names(dd) %in% col_num]))#c("Target", "created_hour")]))

  if (!is.null(jpgname))
    graphics.off()
  
  op <- par(mfrow = c(1,1))
  #library(corrplot)
  corrplot(M, method="number")
  
  if (!is.null(jpgname)) {
    mypath <- file.path(file_path, paste("fig_corrplot_number", jpgname, ".jpg", sep = ""))
    print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  
  if (!is.null(jpgname)) 
    graphics.off()
  
  corrplot(M, method="circle")#"")#corrplot(M, method="number")
  
  if (!is.null(jpgname)) {
  mypath <- file.path(file_path, paste("fig_corrplot", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  
  
  d_numerical_only0 = dd[names(dd) %in% col_num]
  if ("Target" %in% names(d_numerical_only0)) {
    if (!is.null(jpgname))
      graphics.off()
    
    target_cors = apply(d_numerical_only0,2, function(col) { cor(col, d_numerical_only0$Target)} ) 
    
    #barplot( target_cors, names.arg = names(target_cors))
    q<- target_cors
    names(q)<-names(target_cors)
    plot(barchart(q))
    if (!is.null(jpgname)) {
      mypath <- file.path(file_path, paste("fig_corr_target", jpgname, ".jpg", sep = ""))
      print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
    }
  }
}
#pca
pca_numerical<-function(pcadata=pcadata, labels=labels,
                        num_pca_comps = 15
) 
{
  sc_pcadata <- scale(pcadata, center=TRUE, scale=TRUE)
  print(paste(" sc_pcadata data NA = ",sum(is.na(sc_pcadata)),sep=""))
  
  print(" ************ PCA NUMERICAL *******************")
  #head(crimtab) #show sample data
  print( dim(sc_pcadata)) #check dimensions
  
  sum(sc_pcadata) 
  #colnames(crimtab)
  #apply(sc_pcadata,2,var) #check the variance accross the variables
  mean_var = sum( apply(sc_pcadata,2,var) ) / ncol(sc_pcadata)
  print(paste(" mean_var after scaling = ", as.character(mean_var)))
  
  ret_pca_columns=NULL
  pca=NULL
  scaled_pcadata=NULL
  err=NULL
  if (!is.na(mean_var)) { 
    if(1e-6 < abs(1.0 - mean_var)) { 
    print(paste("*** error mean_var=", mean_var, sep=""))
    }
  pca =prcomp(pcadata, scale = T, center = T) 
  
  # variables in PCA coords
  print(" summary(pca)")
  print(summary(pca))
  print (" principal directions: components ")
  print (pca$rotation[,1:min(5,num_pca_comps)]) #head(pca$rotation[,1:7]))
  
  ret_pca_columns = data.frame(pca$x)
  
  
  # in old coords, using inly few PCs
  pc.use <-  num_pca_comps # 3 # explains 93% of variance
  #Finally, you can create a truncated version of your data by 
  #using only the leading (important) PCs
  trunc <- pca$x[,1:pc.use] %*% t(pca$rotation[,1:pc.use])
  #and add the center (and re-scale) back to data
  #if(pca$scale != FALSE)
  trunc <- scale(trunc, center = FALSE , scale=1/pca$scale)
  #if(res$center != FALSE){
  trunc <- scale(trunc, center = -1 * pca$center, scale=FALSE)
  err = data.matrix(pcadata)  - data.matrix(trunc)
  #error_fitting = sum(abs(err)) / sum(abs(pcadata))
  #hist(pcadata$Years_Tenure_of_MetLife) 
  # hist(err[,"Years_Tenure_of_MetLife"])
  }
  return (list(pca_columns = ret_pca_columns, pca=pca , scaled_pcadata=sc_pcadata, ret_err=err))
}
pca_plot <-function(ret,labels=NULL,file_path=NULL, jpgname=NULL)
{
  sc_pcadata = ret$scaled_pcadata
  pca = ret$pca
  
  'graphics.off()
  plot(colMeans(data.matrix(sc_pcadata)), xlab="Columns scaled data")
  mypath <- file.path(file_path, paste("pca_means_scales_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()'
  
  if (!is.null(jpgname)) 
    graphics.off()
  
  par(mfrow=c(1,2))
  plot(cumsum(pca$sdev^2/sum(pca$sdev^2)), xlab=" data numerical variables ")
  plot(pca) 
  
  if (!is.null(jpgname)) {
  mypath <- file.path(file_path, paste("pca_plot_", jpgname, ".jpg", sep = "")); 
  print(mypath); dev.copy(jpeg,filename=mypath) ; dev.off()
  }
  
  if (!is.null(jpgname)) 
    graphics.off()
  par(mfrow=c(1,1))#biplot (pca , scale =0) #plot pca components using biplot in r
  #hide the wines to make it easier to view these vectors.
  #biplot(pca, xlabs = rep("", nrow(sc_pcadata)))
  biplot(pca,scale=0, cex=.8, xlabs=rep("",nrow(sc_pcadata)))
  
  if (!is.null(jpgname)) {
  mypath <- file.path(file_path, paste("pca_biplot_", jpgname, ".jpg", sep = ""))
  print(mypath);   dev.copy(jpeg,filename=mypath) ;   dev.off()
  }
  
  #plot(pca$rotation) #=names(pca$rotation))
  if (!is.null(jpgname)) 
    graphics.off()
  
  par(mfrow=c(1,2))
  #x_small_pcadata
  if (!is.na(labels)) {
    plot(pca$x[,1:2], col=as.integer(as.factor(labels)))
    plot(pca$x[,2:3], col=as.integer(as.factor(labels)))
  } else {
    plot(pca$x[,1:2])
    plot(pca$x[,2:3])
  }
  #plot(x_small_pcadata[,1:2], col=as.integer(as.factor(x_small_labels)))
  #plot(x_small_pcadata[,2:3], col=as.integer(as.factor(x_small_labels)))
  if (!is.null(jpgname)) {
  mypath <- file.path(file_path, paste("pca_all_x_1_2_3_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath) ; dev.off()
  }
  
  if (!is.null(jpgname)) 
    graphics.off()
  par(mfrow=c(1,2))
  prop_varex = pca$sdev^2
  plot(prop_varex / (sum(prop_varex)), xlab="Princ Comp", ylab=" Var Explained")
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  if (!is.null(jpgname)) {
  mypath <- file.path(file_path, paste("pca_variance_explained", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  #http://blog.haunschmid.name/dimensionality-reduction-1-understanding-pca-and-ica-using-r/
  #labels = as.numeric(bin_data$classes[1:num_minority_plot])
}

