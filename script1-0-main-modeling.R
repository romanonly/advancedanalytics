'
setwd("~/R/Rprojects/odbc_data_warehouse/advancedanalytics-master")
'

PATHNAME = "."
setwd(PATHNAME)

source("script1-data-quality-metrics.R")
source("script2-functions.R")
source("read_sls_inactive.R")
source("script2-pca.R")
source("script3_ml_pipeline.R")
source("readdata-ibm-marketing.R")

#======= SET LOGGING
output_dir_name = "/datasets/temp010-500trees-10-1000"
file_path = make_dir_sink_log(output_dir_name)
print(getwd())

#======= READ DATA: IMPLEMENT IT!
d <- ml_read_data(is_sls = FALSE)

#======= TARGET classes
stopifnot (target %in% colnames(d))
d$classes <-  as.factor( ifelse(d[,target] == "0", "benign", "malignant") )
d <- d[ !names(d) %in% c(target) ]

#======= EDA: Select Parameters!
ml_params = list(
  is_remove_all_factors = FALSE # TRUE
  ,is_apply_CORR = TRUE # FALSE
  ,is_apply_PCA = TRUE
  ,NUM_PCA_COLS     = 15
  ,NUM_BIN_PCA_COLS = 4
  ,corr_max_thresh = 0.99 # 0.90
  ,is_make_smaller_dataset = FALSE
)

d0 = set_machine_learning_pipeline(ml_params, d)

# Modeling 
print( summary(d0$classes) )
plot( (d0$classes) )

Mod = data_uncertainty(d0, output_dir_name, model_params, foldout_proportion = 1)

# Plot results
print_models(file_path = Mod$file_path, models = Mod$models, cm_list = Mod$cm_list, test_data = Mod$test_data)

# Save results
mypath <- file.path(Mod$file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)
