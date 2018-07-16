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


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, outfile=paste0('./info_parallel.log'))
#registerDoParallel(cl)
stopCluster(cl)
registerDoSEQ()


PATHNAME = "."
setwd(PATHNAME)


source("script2-functions.R")

make_dir_sink_log <- function(output_dir_name)
{
  
  # output re-directed in cwd. output is appended
  # to existing file. output also send to terminal. 
  file_path=paste(PATHNAME, output_dir_name, sep="")
  dir.create(file_path) # file.path(mainDir, subDir))
  mypath <- file.path(file_path, paste("report_logs", ".txt", sep = ""))
  sink(mypath, append=FALSE, split=TRUE)
  return (file_path)
}

replace_numerical_binomial_with_PCA <- function(d0, data_num, data_binary, 
                                                corr_fac_vars,
                                                NUM_PCA_COLS = 10,
                                                NUM_BIN_PCA_COLS = 3,
                                                plotMajorIsTimesMinor=3, 
                                                num_minority_plot=200)
{
  #========= PCA numerical
  P <- pca_numerical(pcadata=data_num, labels=data_binary$classes, num_pca_comps = NUM_PCA_COLS)
  
  pca_numerical_plot(file_path, pca=P$pca, 
                     pcadata=data_num, 
                     data_binary = data_binary,
                     MajorIsTimesMinor=plotMajorIsTimesMinor, 
                     num_minor_plot=num_minority_plot)
  
  # USe only NumK new variables
  
  pca_cols = P$pca_columns[,1:NUM_PCA_COLS]  
  num_names = names(data_num)
  
  print(paste(" PCA  variables: remove numerical vars and add 15 PCA numerical", str(length(names(d0)))))
  print(paste(" len names d0 = ",as.character(length(names(d0)))))
  
  d00 = d0[ !names(d0) %in% num_names]
  stopifnot(ncol(d00) == sum(!names(d0) %in% num_names))
  print(paste(" len names d0 = ",as.character(length(names(d00)))))
  print(paste(" pca = ",names(pca_cols)))
  if (ncol(d00) > 0) {
    d00 <- cbind(d00, pca_cols)
    print(" PCA bin variables: add 10 PCA numerical")
  } else {
    d00 <- pca_cols
  }
  d0 = d00
  print(paste(" len names d0 = ",as.character(length(names(d0)))))
  
  
  #========= PCA binomial
  
  # don't use "classes" in PCA! 
  d = data_binary
  d = d[ !names(d) %in% c("classes") ]
  d = d[,!names(d) %in% corr_fac_vars]
  
  if (NUM_BIN_PCA_COLS >= 1 & 3<length(colnames(d))) {
    P <- pca_binary(dbin_uncorr = d, 
                    target = data_binary$classes, 
                    NumK = NUM_BIN_PCA_COLS, 
                    MajorIsTimesMinor = 20,
                    num_minor_plot = 10000,
                    file_path = file_path)
    
    pca_binary_plot(P$Psmall, P$logpca_model,  P$clogpca_model, P$logsvd_model, P$logpca_cv)
    
    print(" PCA bin variables: remove bin vars and add 3 numerical")
    print(length(names(d0)))
    
    # USe only NumK new variables
    bin_names = names(data_binary)
    bin_names = bin_names[bin_names != "classes"]
    # remove binvars, except classes
    d00 = d0[, !names(d0) %in% bin_names]
    
    print("Choose CLOGPCA")
    print(length(names(d0)))
    print(P$clogpca_model)
    print( summary(P$pca_bin_clog_df) )
    print(names(P$pca_bin_clog_df))
    
    # add new bin PCA numerical vars
    # horizontal merge: #mydata <- Merge(d0, P$pca_bin_clog_df, by="row.names")
    d00 <- cbind(d00, P$pca_bin_clog_df)
    print(" PCA bin variables: add 3 numerical")
    print(length(names(d00)))
    d0 = d00
  }
  
  return (d0)
}


# REMOVE CORRELATED VARIABLES
remove_correlated_variables <- function(data_num, data_fac, d0, corr_max_thresh = 0.95)
{
  print("#===================== Correlation numerical")

  corr_names = correlated_numerical_col_names(data_num, corr_max_thresh)
  # validate no correlation left
  corr_names2= correlated_numerical_col_names(data_num[,!names(data_num) %in% corr_names])
  stopifnot(length(corr_names2)==0)
  print(paste(" correlated_numerical_col_names num vars=", corr_names))
  if (length(corr_names)>0) {
    d0 = d0[, !names(d0) %in% corr_names]
  }
  print (paste(" total colnames length = ", as.character(length(names(d0)))))



  print("#===================== Correlation factorial")
  corr_fac_vars = c()
  stopifnot("classes" %in% names(data_fac))
  if (1 < ncol(data_fac)) {
    d = data_fac
    d = d[ !names(d) %in% c("classes") ]
    
    corr_fac_vars = correlated_categorical_col_names(d, corr_max_thresh=corr_max_thresh)
  
    stopifnot(!"classes" %in% corr_fac_vars)
    
    corr_fac_vars = corr_fac_vars[corr_fac_vars != "classes"]; #target variable to preserve!
    
    
    print(paste(" correlated fac variables = ",corr_fac_vars))
    print(paste(" correlated_categorical_col_names length numcols = ", as.character(length(corr_fac_vars))))
    # remove correlated factors
    if (length(corr_fac_vars)>0) {
      d0 = d0[, !names(d0) %in% corr_fac_vars]
    }
    print (paste(" total colnames length = ", as.character(length(names(d0)))))
  }  
  return (list(d0=d0, corr_fac_var=corr_fac_vars, corr_names=corr_names))
}

plot_train_test_distributions <- function(bc_data, train_data, test_data, file_path, jpgname)
{
  # magrittr::'%>%'
  clump_thickness = names(bc_data)[1]
  
  graphics.off(); par(mfrow=c(1,3))
  mitosis = names(bc_data)[1]
  rbind( data.frame(group = "train", train_data), data.frame(group = "test", test_data)) %>% 
    tidyr::gather(x, y, clump_thickness:mitosis)  %>%
    ggplot(aes(x = y, color = group, fill = group)) +
    geom_density(alpha = 0.3) +
    facet_wrap( ~ x, scales = "free", ncol = 3)
  
  mitosis = names(bc_data)[2]
  rbind( data.frame(group = "train", train_data), data.frame(group = "test", test_data)) %>% 
    tidyr::gather(x, y, clump_thickness:mitosis)  %>%
    ggplot(aes(x = y, color = group, fill = group)) +
    geom_density(alpha = 0.3) +
    facet_wrap( ~ x, scales = "free", ncol = 3)
  
  mitosis = names(bc_data)[3]
  rbind( data.frame(group = "train", train_data), data.frame(group = "test", test_data)) %>% 
    tidyr::gather(x, y, clump_thickness:mitosis)  %>%
    ggplot(aes(x = y, color = group, fill = group)) +
    geom_density(alpha = 0.3) +
    facet_wrap( ~ x, scales = "free", ncol = 3)
  
  mypath <- file.path(file_path, paste("data_uncertainty_train_vs_text_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
}


#bc_data$classes = 'malignant', 'benign'
data_uncertainty <- function(bc_data=NA, 
                             output_dir_name = "/datasets/bc_data_5percent", 
                             model_params, 
                             foldout_proportion=1)
{
    jpgname = "_analysis"
    file_path=paste(PATHNAME, output_dir_name, sep="")

    if (exists("bc_data") & is.na(bc_data)) {
      bc_data <- read_data()
    } 
    
    d <- remove_minority(bc_data, proportion = foldout_proportion)
    
    bc_data = d$bc_data
    bc_data_missing = d$bc_data_missing
    
    summary(bc_data)

    graphics.off()
    mypath <- file.path(file_path, paste("ggplot_data.jpg", sep = "")); print(mypath)
    myplot <- ggplot(bc_data, aes(x = classes, fill = classes)) + geom_bar()
    print(myplot)
    dev.copy(jpeg,filename=mypath); dev.off()
    
    
    
    print(file_path)
    print(summary(bc_data$classes))
    colnames(bc_data)
    
    # Train and test partition
    set.seed(42)
    index <- caret::createDataPartition(bc_data$classes, p = 0.5, list = FALSE)
    train_data <- bc_data[index, ]
    test_data  <- bc_data[-index, ]
    
    
    # Plot train vs test distributions
    plot_train_test_distributions(bc_data, train_data, test_data, file_path, jpgname)

    # MODELING
    
    models <- list()
    cm_list<- list()
    gains_list <- list()
    model_names_list <- list()
    
    print("==== gbm ==== ")
    try(
      {
        model_gbm <- make_model_rf(file_path, test_data, train_data, model_params, method="gbm", par_sampling="down")
        if (exists("model_gbm")) {
          plot_model(model_gbm$model, "gbm", file_path)
          gains <- plot_gain(model_gbm$model, test_data, train_data, file_path=file_path, jpgname='model_gbm')
          #gainsGBM_missing <- plot_gain(model_gbm$model, test_data, bc_data_missing, train_name='foldout', file_path=file_path, jpgname='mising_model_gbm')
          plot_ROC(model_gbm$model, test_data, train_data, file_path=file_path, jpgname='model_gbm')
          print( varImp(model_gbm$model))
          
          models <- append(models, list('gbm' = model_gbm$model))
          cm_list<- append(cm_list, list('gbm' = model_gbm$cm))
          gains_list <- append(gains_list, list('gbm' = gains))
          model_names_list[['gbm']] = 'gbm'
        }
      }
    )
    
        
    method = 'rf' # gbm' # C5.0' # 'gbm' # 'rf'
    
    for (i_par_sampling in c("down", "none", "up", "rose", "smote")) {
      print( paste("==== ", i_par_sampling))
      i_model_rf = NA
      try(
        {
          i_model_rf <- make_model_rf(file_path, test_data, train_data, model_params, method, par_sampling=i_par_sampling)
      
      if (exists("i_model_rf") & (!is.na(i_model_rf))) { # ) {
        plot_model(i_model_rf$model, i_par_sampling, file_path)
        gains <- plot_gain(i_model_rf$model, test_data, train_data, file_path=file_path, jpgname=i_par_sampling)
        #gainsUNDER_missing <- plot_gain(model_rf_under$model, test_data, bc_data_missing, train_name='foldout', file_path=file_path, jpgname='missing_model_under')
        plot_ROC(i_model_rf$model, test_data, train_data, file_path=file_path, jpgname=i_par_sampling)
        print( varImp(i_model_rf$model))
        
        #cm_under <- model_rf_under$cm
        #models <- append(models, list(i_par_sampling = i_model_rf$model))
        models[[i_par_sampling]] = i_model_rf$model
        #cm_list<- append(cm_list, list(i_par_sampling = i_model_rf$cm))
        cm_list[[i_par_sampling]] = i_model_rf$cm
        #gains_list <- append(gains_list, list(i_par_sampling = gains))
        gains_list[[i_par_sampling]] = gains
        model_names_list <- append(model_names_list, i_par_sampling)
      }
        }
      )
    }
    

  #============ Regression
  print("==== glm  ==== ") # SLOW
  print("==== glm  very slow use number and repeats <= 3 !!!!!! ==== ") # SLOW
  try (
    {
      ctrl <- caret::trainControl(method = "repeatedcv",#oob-Out of bag estimates are not implemented for this model
                                  number = model_params$tr_n, #min(3,tr_n),
                                  repeats = model_params$tr_r, #min(3,tr_r),
                                  summaryFunction = twoClassSummary,
                                  classProbs = TRUE,
                                  verboseIter = FALSE)
      #glmBoostGridGlm = data.frame(glmBoostGrid)
      model_glm <- caret::train(classes~., data=train_data, 
                                method="glm",  
                                preProc=c("center", "scale"),  
                                trControl=ctrl, 
                                metric="ROC")
      
      if (exists("model_glm")) {
        print(model_glm)
        gainsGLM <- plot_gain(model_glm, test_data, train_data, file_path=file_path, jpgname='model_glm')
        #gainsGLM_missing <- plot_gain(model_glm, test_data, bc_data_missing, train_name='foldout', file_path=file_path, jpgname='missing_model_glm')
        plot_ROC(model_glm, test_data, train_data, file_path=file_path, jpgname='model_glm')
        
        final <- data.frame(actual = test_data$classes, predict(model_glm, newdata = test_data, type = "prob"))
        final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
        final$correct <- ifelse(final$actual == final$predict, TRUE, FALSE)
        
        models <- append(models, list('glm' = model_glm))
        cm_glm <- confusionMatrix(final$predict, test_data$classes)
        cm_list<- append(cm_list, list('glm' = cm_glm))
        gains_list<-append(gains_list, list('glm' = gainsGLM))
        model_names_list[['glm']] = 'glm'
      }
    }
  )
  
  if (FALSE) {
  print("==== glm boosted ==== ") #VERY SLOW
  try (
    {
      set.seed(69)
      # Use the expand.grid to specify the search space   
      glmBoostGrid = expand.grid(mstop = c(50, 100, 150, 200, 250, 300),
                                 prune = c('no', 'yes')) #, 'no'))
      
      ctrl <- caret::trainControl(method = "repeatedcv",
                                  number = min(2, model_params$tr_n),
                                  repeats = min(2, model_params$tr_r),
                                  summaryFunction = twoClassSummary,
                                  classProbs = TRUE,
                                  verboseIter = FALSE)
      
      model_glm_b <- caret::train(classes~., data=train_data, 
                                  method="glmboost", # MODEL FAILS for categorical zero0varuance vars 
                                  preProc=c("center", "scale"),  
                                  trControl=ctrl, 
                                  tuneGrid = glmBoostGrid,
                                  metric="ROC")
      
      if (exists("model_glm_b")) {
        print(model_glm_b)
        gainsGLM <- plot_gain(model_glm_b, test_data, train_data, file_path=file_path, jpgname='model_glm_b')
        gainsGLM_missing <- plot_gain(model_glm_b, test_data, bc_data_missing, train_name='foldout', file_path=file_path, jpgname='missing_model_glm')
        plot_ROC(model_glm_b, test_data, train_data, file_path=file_path, jpgname='model_glm_b')
        
        final <- data.frame(actual = test_data$classes, predict(model_glm_b, newdata = test_data, type = "prob"))
        final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
        final$correct <- ifelse(final$actual == final$predict, TRUE, FALSE)
        
        models <- append(models, list(glm = model_glm_b))
        cm_glm <- confusionMatrix(final$predict, test_data$classes)
        cm_list<- append(cm_list, list(cm_glm = cm_glm))
        gains_list<-append(gains_list, list('glm_b' = gainsGLM))
        print("==== glm boosted ended ==== ")
      }
    }
  )
  }
  # 
  # Plot all test gain curves
  #
  graphics.off()
  par(fig=c(0,1.0,0, 1.0))
  plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
       ylab="True Positive Rate",
       xlab="Rate of Positive Predictions")
    nlwd = 1
  #model_names = c('GBM', 'UNDER', 'RFNone', 'OVER', 'SMOTE', 'ROSE', 'GLM') 
  cols_list = c('green', 'red', 'black', 'blue', 'green3', 'cyan', 'magenta')
  stopifnot(length(cols_list) >= length(gains_list))
  k = 1
  for (g in gains_list) {
    print(model_names_list[k])
    lines(x=g$gaintestx, y=g$gaintesty, col=cols_list[k], lwd=nlwd,lty=k)#,pch=21,bg='green')    
    k = k + 1; 
  }

  legend('topright',
         legend=model_names_list,
         lty=c(1,1,1,1,1,1,1), 
         bty='n', cex=0.75,
         col=cols_list)
  
  mypath <- file.path(file_path, paste("gain_", "all", ".jpg", sep = ""))
  dev.copy(jpeg,filename=mypath)
  dev.off()
  
  return (list(file_path = file_path, models=models, 
               cm_list = cm_list, test_data=test_data, gains_list=gains_list))
}

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

  if (FALSE) {
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
}
