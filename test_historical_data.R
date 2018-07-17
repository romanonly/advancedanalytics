
source("test_historical_data_functions.R")
source("test_historical_data_modeling.R")

source("test_script1-data-quality-metrics.R")

#===== Setup Env
setwd("~/RstudioProjects/2018/doordash")
PATHNAME = "."
setwd(PATHNAME)
file_path = "./datasets/"

fn = paste(file_path, "historical_data.csv",sep="")
print(fn)


#===========read data
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
d_Target$Target = log(1.0 + d_Target$Target)

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

return (list(d_Target=d_Target, d_Target_responses_freq=d_Target_responses_freq))
}

#=============== MODEL ============
run_model<-function(dtrain=NULL,dtest=NULL,dir_name=NULL)
{
  
  out_dir = make_dir_sink_log(dir_name)
  print(out_dir)
  
  #EDA: 
  data = dtrain
  d_numerical_only = data[names(data) %in% get_num_variables_list(data)]
  # Outliers to log
  jpgname = "_outliers"; plot_hist(d_numerical_only)

  d_numerical_only_log = replace_log_transformation(d_numerical_only)
  
  jpgname = "_outliers_logged"; plot_hist(d_numerical_only_log)
  # Correlation to target
  jpgname = "_correl"; plot_corr(d_numerical_only)  
  jpgname = "_correl_log"; plot_corr(d_numerical_only_log)  
  
  # PCA
  # Factors only: 
  col_num_factors = c("market_id","store_primary_category","order_protocol")#,"store_id")
  d_factors = data[names(data) %in% col_num_factors]
  labels = d_factors$order_protocol
  jpgname = "_numerical"
  ret = pca_numerical(pcadata=d_numerical_only, labels=labels, num_pca_comps = 10);  pca_plot(ret)                 
  
  # Modeling
  #library(caret)
  #library(gbm)
  hist(d_numerical_only$Target)
  
  dtrain_num = dtrain[names(dtrain) %in% get_num_variables_list(dtrain)]
  dtest_num =  dtest[names(dtest) %in% get_num_variables_list(dtest)]
  
  r = modeling_ensemble(dtrain_num, dtest_num, split_ratio=0.8, is_timesorted_vs_sample=FALSE)
  
  # plot varimp
  graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r$model_gbm),main="GBM - Variable Importance")
  mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  return(list(model_gbm=r$model_gbm))
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

#==================================
d <- read.csv(fn)

ret = split_data(d, split_ratio = 0.6, is_timesorted_vs_sample = FALSE)

r=data_transformation(ret$dtrain)
d_Target_train = r$d_Target[r$d_Target$Target < 1e4,]
d_Target_responses_freq_train = r$d_Target_responses_freq[r$d_Target_responses_freq$Target < 1e4,]
hist(d_Target_train$Target)

r=data_transformation(ret$dtest)
d_Target_test = r$d_Target[r$d_Target$Target < 1e4,]
d_Target_responses_freq_test = r$d_Target_responses_freq[r$d_Target_responses_freq$Target < 1e4,]
hist(d_Target_test$Target)

#data = d_Target_responses_freq
#data = data[, !names(data) %in% c("date_create_at")]
factors <- get_factors_responses_target(d_Target_responses_freq_train) 
dtrain2<-add_factors_response_target(d_Target_responses_freq_train, factors)
dtest2<-add_factors_response_target(d_Target_responses_freq_test, factors)

#========================
# test1
dir_name="/datasets1v97"
jpgname="_target"
file_path=paste(".",dir_name,sep="")
labels = d_Target_train$market_id

r=run_model(d_Target_train, d_Target_test, dir_name)

graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r$model_gbm),main="GBM - Variable Importance")
mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
print(mypath); dev.copy(jpeg,filename=mypath); dev.off()


# === add factor response variable: ratio dashers
dir_name="/datasets3v92"
file_path=paste(".",dir_name,sep="")
jpgname="_freq"
labels = d_Target_responses_freq_train$market_id

r=run_model(d_Target_responses_freq_train, d_Target_responses_freq_test, dir_name)

graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r$model_gbm),main="GBM - Variable Importance")
mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
print(mypath); dev.copy(jpeg,filename=mypath); dev.off()

#==== add factors respionse of Target: take care of LEAKS
dir_name="/datasets4v94"
file_path=paste(".",dir_name,sep="")
jpgname="none"
labels = dtrain2$market_id


r=run_model(dtrain2,dtest2,dir_name)

graphics.off(); par(mfrow=c(1,1)); plot(varImp(object=r$model_gbm),main="GBM - Variable Importance")
mypath <- file.path(file_path, paste("modeling_varimp_", "model_gbm", ".jpg", sep = ""))
print(mypath); dev.copy(jpeg,filename=mypath); dev.off()

#======= json
read_json<-function()
{
file_pathJson = "./datasets/"
fnJson = paste(file_pathJson, "data_to_predict.json",sep="")
print(fnJson)
#json_file="./datasets/data_to_predict.json"
require(RJSONIO)    

json_file <- fromJSON(fnJson)
#Secondly we can unlist the json_file as following:
json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
#And finally we can join the unlisted columns in one data frame:
df<-as.data.frame(do.call("cbind", json_file))
df$total_items = as.integer(df$total_items)

library(DT)
datatable(head(df))

install.packages("rjson")

library(jsonlite)
winners <- fromJSON(fnJson, flatten=TRUE)
colnames(winners)

library(RJSONIO)
library(RCurl)
json_file2 = RJSONIO::fromJSON(fnJson)
head(json_file2)
}