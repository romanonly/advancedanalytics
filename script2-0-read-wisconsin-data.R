#
#script2_0_read_wisconsin_data
#
read_data<-function() #proportion = 10)
{

  bc_data <- read.table("datasets/breast-cancer-wisconsin.data.txt", 
                        header = FALSE, 
                        sep = ",")
  
  colnames(bc_data) <- c("sample_code_number", 
                         "clump_thickness", 
                         "uniformity_of_cell_size", 
                         "uniformity_of_cell_shape", 
                         "marginal_adhesion", 
                         "single_epithelial_cell_size", 
                         "bare_nuclei", 
                         "bland_chromatin", 
                         "normal_nucleoli", 
                         "mitosis", 
                         "classes")
  
  bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                            ifelse(bc_data$classes == "4", "malignant", NA))
  
  bc_data[bc_data == "?"] <- NA
  
  # how many NAs are in the data
  length(which(is.na(bc_data)))
  nrow(bc_data)
  nrow(bc_data[is.na(bc_data), ])
  ## Missing values are imputed with the mice package.
  # impute missing data
  bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
  dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
  bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))
  
  bc_data$classes <- as.factor(bc_data$classes)
  
  summary(bc_data$classes)
  return (bc_data)
  #
  # remove minority class to n=5% 10%
  #
  #return (list(bc_data = d, bc_data_missing = d_miss))
  #ret_list = remove_minority(bc_data, proportion)
  #return (ret_list) # return (list(bc_data = d, bc_data_missing = d_miss))

  '  
  d1 = bc_data[bc_data$classes == "malignant",]
  d2 = bc_data[bc_data$classes != "malignant",]
  
  n = as.integer( nrow(d2)/ proportion )
  d10 = d1[sample(nrow(d1),n),]
  d11 = d1[-sample(nrow(d1),n),]
  print (c( nrow(d1), nrow(d2)))
  stopifnot(nrow(d10) + nrow(d11) == nrow(d1))
  
  d<-rbind(d2, d10)
  d_miss <- rbind(d2, d11)
  
  return (list(bc_data = d, bc_data_missing = d_miss))
  '
}

