library("bnlearn")

setwd("~/R/romaprojects/BayesianNetwork/bnlearn/")

source("readdata-ibm-marketing.R")


ret = readdata_ibm_marketing() 
d = ret$data
d_discrete = ret$d_discretize
dorig = d


#PATHNAME = "~/R/romaprojects/Webinar_ISDS"
PATHNAME = "."
setwd(PATHNAME)

source("script2-functions.R")
source("script1-data-quality-metrics.R")
#source("script2-0-read-wisconsin-data.R")
#output_dir_name = "/datasets/bc_data_5percent"

tr_n = 2 # 10 # number - Either the number of folds or number of resampling iterations
tr_r = 2 # 10 # repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
tr_numtree = 10 # 100


#d0 = d_discretize
d0 = d

d0$classes <- ifelse(d0$Response == "0", "benign", "malignant")
d0$classes = as.factor(d0$classes)

cols_remove2 = names(d0) %in% c("Response")
d0 <- d0[ !cols_remove2 ]
d0$classes = as.factor(d0$classes)

PATHNAME = "."
setwd(PATHNAME)


#output_dir_name = "/datasets/bc_data_5percent"

#d0$Ed = revalue(d0$Ed, c("High School or Below" = "HighSchool")) #rose fails for facotrs with "double words"
#d0$ES = revalue(d0$ES, c("Medical Leave" = "MedicalLeave"))
#bc_data <- read_data()
d = data_uncertainty(bc_data = d0, output_dir_name = "/datasets/emailmarketing-non_discrete", foldout_proportion = 5)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)

cols_remove = names(d0) %in% c("ROT")
d_no_ROT <- d0[ !cols_remove ]

d = data_uncertainty(bc_data = d_no_ROT, output_dir_name = "/datasets/emailmarketing-without-offertype-non_discrete", foldout_proportion = 5)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)
