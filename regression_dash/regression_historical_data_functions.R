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

plot_hist<-function(dd,file_path, jpgname)
{
  graphics.off()
  op <- par(mfrow = c(3,6))
  i = 1
  for (nm in names(dd)) {
    if ("integer" == class(dd[,nm]) | "numeric" == class(dd[,nm])) {
      #print(nm)
      i = 1 + i
      if (i <= 18) 
        hist( (dd[,nm]),main=nm) #log makes more gaussian
    }
  }
  mypath <- file.path(file_path, paste("eda_hist_all", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
}
replace_log_transformation <- function(dd)
{
  col_num_outliers=c("total_items", "subtotal", "num_distinct_items",
                     "min_item_price", "max_item_price", 
                     "estim_order_place_dt" ## estimates_order_place_duration, 
  )
  for (nm in names(dd)) {
    if ("integer" == class(dd[,nm]) | "numeric" == class(dd[,nm])) {
      #print(nm)
      hist( (dd[,nm]),main=nm) #log makes more gaussian
      if (nm %in% col_num_outliers) {
        dd[, nm] = log(1.1 + (dd[, nm] - min(dd[, nm])))
      }
    }
  }
  return (dd)
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
make_target_sort_date <- function(d)
{
  #The target value to predict here is the total seconds value between 
  # `created_at` and `actual_delivery_time`. '
  x1 <- strptime(d$created_at, "%Y-%m-%d %H:%M:%OS")
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

# factor variables: add reponses as frequencies
add_factors_responses_frequencies <- function(d0) 
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
plot_corr <- function(dd,file_path, jpgname)  
{
  graphics.off()
  op <- par(mfrow = c(1,1))
  #library(corrplot)
  
  col_num = get_num_variables_list(dd)
  M = cor(as.matrix(dd[names(dd) %in% col_num]))#c("Target", "created_hour")]))
  graphics.off()
  corrplot(M, method="number")
  mypath <- file.path(file_path, paste("fig_corrplot_number", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off()
  corrplot(M, method="circle")#"")#corrplot(M, method="number")
  mypath <- file.path(file_path, paste("fig_corrplot", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off()
  d_numerical_only0 = dd[names(dd) %in% col_num]
  target_cors = apply(d_numerical_only0,2, function(col) { cor(col, d_numerical_only0$Target)} ) 
  
  #barplot( target_cors, names.arg = names(target_cors))
  q<- target_cors
  names(q)<-names(target_cors)
  plot(barchart(q))
  
  mypath <- file.path(file_path, paste("fig_corr_target", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
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
  
  graphics.off()
  par(mfrow=c(1,2))
  plot(cumsum(pca$sdev^2/sum(pca$sdev^2)), xlab=" data numerical variables ")
  plot(pca) 
  mypath <- file.path(file_path, paste("pca_plot_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()
  
  
  graphics.off()
  par(mfrow=c(1,1))#biplot (pca , scale =0) #plot pca components using biplot in r
  #hide the wines to make it easier to view these vectors.
  #biplot(pca, xlabs = rep("", nrow(sc_pcadata)))
  biplot(pca,scale=0, cex=.8, xlabs=rep("",nrow(sc_pcadata)))
  mypath <- file.path(file_path, paste("pca_biplot_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()
  
  
  #plot(pca$rotation) #=names(pca$rotation))
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
  mypath <- file.path(file_path, paste("pca_all_x_1_2_3_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()
  
  graphics.off()
  par(mfrow=c(1,2))
  prop_varex = pca$sdev^2
  plot(prop_varex / (sum(prop_varex)), xlab="Princ Comp", ylab=" Var Explained")
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  mypath <- file.path(file_path, paste("pca_variance_explained", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  #http://blog.haunschmid.name/dimensionality-reduction-1-understanding-pca-and-ica-using-r/
  #labels = as.numeric(bin_data$classes[1:num_minority_plot])
}

