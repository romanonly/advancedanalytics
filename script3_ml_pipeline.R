model_params = list(
  tr_n = 10 # 10 # 50 # 20 # 10 # FOLD number - Either the number of folds or number of resampling iterations
  , tr_r = 2 # 4 # 2 # 10 # repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
  , tr_numtree = 1500 # 100 # 5000 # 100
  , tr_numtree_speedup = 0.5 # for slow take only 0.1*numtree of "down" (fastes)
  , tr_is_repeatedcv_vs_oob = "repeatedcv"# "oob" # oob - faster, but will not do sampling as no validation-no-subsets
  , mtry = c(3,5,7) # c(1,2) #c(1,2,5,10) tr_mtry_speedup = 0.5 # mtry = 0.5*sqrt(num_columns)
  # PCA-remove-factors 10vars total 10%-decimate 6908x20: (200,10) - no overfit 
  , sampsize=10 # 50 # 10 # 10 # 50 # 50 #200
  , maxnodes=1000 # 500 # 100000 # 500# 200 # 100 # 100 # 10 
  , metric= "ROC" # Kappa" # "ROC" #"Kappa"
  
  , gbm.interaction.depth = c(1,3) # c(1,2) #c(1,2,3) # 5, 9 - overfitting , #c(1, 5, 9), 
  , gbm.n.trees = c(50) # (1:3)*25 #  50
  , gbm.shrinkage = c(0.05, 0.2) # c(0.05, 0.1) # , 0.2) #0.2
  , gbm.n.minobsinnode = 10 # c(10, 20) #
  , gbm_metric = "ROC" #"Kappa"-not supported
)

ml_read_data <- function(is_sls = FALSE)
{
  if (is_sls == TRUE) {
    d_list <- read_sls_inactive(fname="~/R/Rprojects/odbc_data_warehouse/datasets/SLS_Inactive_sibusiso.csv")
    d = d_list$d_136vars
    target = "SLS"
  } else {
    d_list <- readdata_ibm_marketing()
    d = d_list$data
    target = "Response"
  }  
  return (d)
}
set_machine_learning_pipeline <- function(ml_params, d)
{
  # MINORITY vs MAJORITY CLASS
  stopifnot(sum(d$classes=="malignant")*2 < sum(d$classes=="benign") )
  
  #=======  EXPLORATORY STAGE: CREATE SMALLER DATASET
  # exists("is_make_smaller_dataset") & 
  if (ml_params$is_make_smaller_dataset == TRUE) {
    d00 <- select_small_subset_minority_majority(d, d$classes, MajorIsTimesMinor=10, num_minor_plot=1000)
    summary(d$classes)
    summary(d00$ret_x0x1$classes)
    #write.csv(d00$ret_x0x1, file = "~/R/Rprojects/odbc_data_warehouse/datasets/SLS_Inactive_sibusiso_small_600_6000.csv",row.names=FALSE)
    d = d00$ret_x0x1
    print( summary(d$classes) )
    plot( (d$classes) )
  }
  
  #======= remove zero-variabce: faster learning, but some data loss
  ' freqCut = 95/5, uniqueCut = 10,
  freqCut
  the cutoff for the ratio of the most common value to the second most common value
  uniqueCut
  the cutoff for the percentage of distinct values out of the number of total samples
  '
  if (FALSE) {
    nzv_report = caret::nearZeroVar(d, saveMetrics = TRUE, freqCut = 95/5, uniqueCut = 10)
    print("NZV good variables")
    print( nzv_report[nzv_report[,"zeroVar"] + nzv_report[,"nzv"] == 0, ] )
    print("NZV bad variables")
    print ( length( which(nzv_report$nzv == T)))  #nzv_T finds those columns which have 
    print( nzv_report[nzv_report[,"zeroVar"] + nzv_report[,"nzv"] > 0, ] )
    #nzv_cols <- caret::nearZeroVar(d)
    #stopifnot(length(nzv_cols)==nrow(nzv_report[nzv_report[,"zeroVar"] + nzv_report[,"nzv"] > 0, ]))
    nzv_cols <- which(nzv_report$nzv == T)  #nzv_T finds those columns which have 
    nzv_cols <- setdiff( nzv_cols,  (  which(colnames(d) %in% c("classes")) ))
    if (FALSE) {
      if (length(nzv_cols) > 0) {
        d_zerovarremoved = d[, -nzv_cols]
        d = d_zerovarremoved
      }
    }
  }  
  zerovar_report = caret::nearZeroVar(d, saveMetrics = TRUE, freqCut = 99/1, uniqueCut = 1)
  print(" Zero-var VERY bad variables")
  print( zerovar_report[zerovar_report[,"zeroVar"]  == TRUE, ] )
  zerovar_cols <- which(zerovar_report$zeroVar == TRUE)  
  if (length(zerovar_cols) > 0) 
    d = d[, -zerovar_cols]
  
  stopifnot("classes" %in% colnames(d))
  
  #======= split into numerical, categorized, binary
  data <- make_num_fac_bin(d, nbreaks=3) 
  
  print(" numerical ")
  print(colnames(data$data_num))
  print(" factor ")
  print(colnames(data$data_fac))
  print(" binary ")
  print(colnames(data$data_binary))
  
  d0 = d

  # ======== remove correlated
  if (ml_params$is_apply_CORR) {
    CORR = remove_correlated_variables(data$data_num, data$data_fac, data$data, corr_max_thresh = ml_params$corr_max_thresh)
    d0 = CORR$d0
    print (paste(" total colnames length = ", as.character(length(names(d0)))))
    
    highCorr <- findCorrelation(cor(data$data_num), ml_params$corr_max_thresh) #'x' must be numeric
    print( " highCorr <- findCorrelation ")
    print( CORR$corr_names )
    print( colnames(data$data_num)[ highCorr ] )
    #d0_highCorr <- d0[, -highCorr]
    highCorr <- findCorrelation(cor(d0[, colnames(d0) %in% colnames(data$data_num) ]), ml_params$corr_max_thresh) #'x' must be numeric
    stopifnot(length(highCorr) == 0)
  }
  
  # ========== NUMERICAL PCA , BINARY PCA
  if (ml_params$is_apply_PCA) {
    print("is_apply_PCA preProcess")
    dnum = data$data_num
    xTrans <- preProcess(dnum, method="pca", thresh = ml_params$corr_max_thresh)
    print(xTrans)
    print( colnames ( xTrans$rotation ) )
    d0_xTrans = predict(xTrans, dnum)
    print( names(d0_xTrans) )
    stopifnot(nrow(d0_xTrans) == nrow(d0))
    
    dPCA = replace_numerical_binomial_with_PCA(d0, data$data_num, data$data_binary, 
                                               CORR$corr_fac_vars,
                                               NUM_PCA_COLS = min(ml_params$NUM_PCA_COLS, -1+length(colnames(data$data_num))), 
                                               NUM_BIN_PCA_COLS = min(ml_params$NUM_BIN_PCA_COLS, -1+length(colnames(data$data_binary))),
                                               plotMajorIsTimesMinor=3,  num_minority_plot=200)
    d0 = dPCA
  }
  
  #==================== KEEP ONLY NUMERICAL
  if (ml_params$is_remove_all_factors == TRUE) {
    # REMOVE ALL CATEGORICAL
    dNUMONLY = d0[, !names(d0) %in% colnames(data$data_fac)]
    dNUMONLY$classes = d0$classes
    d0 = dNUMONLY
  }
  
  return (d0)
}