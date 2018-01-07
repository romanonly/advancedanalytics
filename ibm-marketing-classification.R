packages <- c("bnlearn")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

PATHNAME = "."
setwd(PATHNAME)

source("readdata-ibm-marketing-v2.R")

ret = readdata_ibm_marketing() 
d = ret$data
d_discretize = ret$data_discrete


source("script2-functions.R")
source("script1-data-quality-metrics.R")
#source("script2-0-read-wisconsin-data.R")
#output_dir_name = "/datasets/bc_data_5percent"

tr_n = 10 # 10 # number - Either the number of folds or number of resampling iterations
tr_r = 10 # 10 # repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
tr_numtree = 100 # 100


d0 = d_discretize
d0$classes <- ifelse(d0$Response == "0", "benign", "malignant")
d0$classes = as.factor(d0$classes)
cols_remove2 = names(d0) %in% c("Response")
d0 <- d0[ !cols_remove2 ]
d0_discretize = d0

d0 = d
d0$classes <- ifelse(d0$Response == "0", "benign", "malignant")
d0$classes = as.factor(d0$classes)
cols_remove2 = names(d0) %in% c("Response")
d0 <- d0[ !cols_remove2 ]

PATHNAME = "."
setwd(PATHNAME)


#output_dir_name = "/datasets/bc_data_5percent"

#d0$Ed = revalue(d0$Ed, c("High School or Below" = "HighSchool")) #rose fails for facotrs with "double words"
#d0$ES = revalue(d0$ES, c("Medical Leave" = "MedicalLeave"))
#bc_data <- read_data()

'Two expirementa: all variables and without Bayseian variables'
cols_remove = names(d0) %in% c( "i1.EDnDL", "i1.MoPolInc", "i3.MoLaClm"
                                , "i5.NoOC"
                                ,  "PoT"
                                , "Po"
                                , "STATE")
                                #"i6.Ed", "i6.Gr", "VehcSz",
# Original, NON-DICRETIZED 
cols_remove_importfatures = names(d0) %in% c("i3.ES","i1.TCA", "Location.Code" 
                                             ,"i6.Ed","i6.Gr", "VehcSz", "i6.VC", "i6.MS" ,"i6.SC"
                                             #, "i4.ROT"
                                             , "i1.EDnDL", "i1.MoPolInc", "i3.MoLaClm"
                                             , "i5.NoOC",  "PoT", "Po", "STATE"
                                             )
d_no_ROT <- d0[ !cols_remove_importfatures ]
d = data_uncertainty(bc_data = d_no_ROT, output_dir_name = "/datasets/emailmarketing-nondiscretized-remove-important", foldout_proportion = 2)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)

d_no_ROT <- d0[ !cols_remove ]
d = data_uncertainty(bc_data = d_no_ROT, output_dir_name = "/datasets/emailmarketing-without-offertype-nondiscretized", foldout_proportion = 2)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)

d = data_uncertainty(bc_data = d0, output_dir_name = "/datasets/emailmarketing-nondiscretized", foldout_proportion = 2)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)


# DICRETIZED 
d = data_uncertainty(bc_data = d0_discretize, output_dir_name = "/datasets/emailmarketing-discretized", foldout_proportion = 2)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)

d_no_ROT <- d0_discretize[ !cols_remove ]

d = data_uncertainty(bc_data = d_no_ROT, output_dir_name = "/datasets/emailmarketing-without-offertype-discretized", foldout_proportion = 2)
print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)

