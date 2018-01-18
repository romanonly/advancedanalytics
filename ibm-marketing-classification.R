packages <- c("bnlearn")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

PATHNAME = "."
setwd(PATHNAME)

source("readdata-ibm-marketing.R")

ret = readdata_ibm_marketing(nbreaks = 6) 
d = ret$data
d_discretize = ret$data_discrete


source("script2-functions.R")
source("script1-data-quality-metrics.R")
#source("script2-0-read-wisconsin-data.R")
#output_dir_name = "/datasets/bc_data_5percent"

tr_n = 10 # 10 # number - Either the number of folds or number of resampling iterations
tr_r = 10 # 10 # repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
tr_numtree = 200 # 100

"
tr_n = 2 # 10 # number - Either the number of folds or number of resampling iterations
tr_r = 2 # 10 # repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
tr_numtree = 2 # 100
"

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
"
Non-discrerized
UP
  only 20 most important variables shown (out of 51)
                      Overall
i1.TCA                100.000
i1.CLfV                89.003
i1.Inc                 87.462
i1.EDnDL               81.336
i1.MoPolInc            80.185
i4.ROTOffer3           78.624
i4.ROTOffer4           76.097
i3.MpPreAu             72.713
i3.MoLaClm             70.466
i3.ESRetired           60.288
i4.ROTOffer2           30.379
i3.NoP                 28.276
Location.CodeSuburban  26.319
i6.MSSingle            23.845
i6.EdMaster            19.349
i6.SCBranch            16.944
i5.NoOC                16.818
i6.MSMarried           14.053
VehcSzSmall            10.815
i6.SCWeb                9.932


DISCRETE ML ranking
i4.ROTOffer4           100.00 - importnat both
i4.ROTOffer3            96.19 - importnat both
i3.ESRetired            60.08 - importnat both
i4.ROTOffer2            36.63 - importnat both
Location.CodeSuburban   33.21 - importnat both
i1.EDnDL(25.3,44.7]     33.02 - NotML
i6.SCBranch             32.93 - importnat both
i6.CovExtended          30.01 - Not ML
i6.GrM                  29.57 - Not ML
i1.MoPolInc(66,99.1]    28.62 - Not ML
i1.MoPolInc(33,66]      28.00 - Not ML
i6.MSSingle             27.94 - importnat both
i3.ESUnemployed         27.69 - importnat both
i6.MSMarried            27.59 - importnat both
i3.MoLaClm(11.7,23.3]   27.23 - Not ML
i6.EdCollege            26.88 - Not ML
i6.VCSUV                26.74 - importnat both
i6.SCCall Center        26.31 - importnat both
i6.EdMaster             24.76 - Not ML
i1.EDnDL(44.7,64.1]     24.61 - Not ML
"
# Discretized: importany by ML, non-important by BL - down-ML-ranking
cols_remove = names(d0) %in%  c("i1.EDnDL"
                                ,"i6.Cov"                                   
                                , "i6.Gr"
                                , "i3.MoLaClm"
                                ,"i1.MoPolInc"
                                )

# DICRETIZED,  important by BL, non-important by ML 
cols_remove_importfatures = names(d0) %in% c("i1.TCA" #none
                                             , "i1.Inc" #none 
                                             , "i6.MS" # 8th
                                             , "i6.VC" #10
                                             , "i6.SC" # 6th
                                             )

# important by ML, non-important by BL
cols_remove_ML = names(d0) %in% c("i1.EDnDL"
                                 , "i1.CLfV"
                                 , "i1.MoPolInc"
                                 , "i3.MoLaClm"
                                 , "i5.NoOC"
                                 , "i3.NoP"
                                 ,  "PoT"
                                 , "Po"
                                 , "STATE") # 9 features
                            #"i6.Ed", "i6.Gr", "VehcSz",

# Original, NON-DICRETIZED 
## important by BL, non-important by ML
cols_remove_BL = names(d0) %in% c("i3.ES", 
                                 "Location.Code" 
                                 ,"i6.Ed"
                                 ,"i6.Gr"
                                 , "VehcSz"
                                 , "i6.VC"
                                 , "i6.MS" 
                                 ,"i6.SC" # 8 features
                                 )
                                 #, "i4.ROT"
                                 #, "i1.EDnDL", "i1.MoPolInc", "i3.MoLaClm"
                                 #, "i5.NoOC",  "PoT", "Po", "STATE"

# important by ML only
cols_remove_9ML_only = names(d0) %in% c("i1.TCA" #                100.000
                                       , "i1.CLfV" #                89.003
                                       , "i1.Inc" #                 87.462
                                       
                                       , "i1.EDnDL" #               81.336
                                       , "i1.MoPolInc" #             80.185
                                       , "i4.ROT" #           78.624
                                       
                                       , "i3.MpPreAu" #             72.713
                                       , "i3.MoLaClm" #             70.466
                                       , "i3.ES" #           60.288
                                       ) # 9 features                                       

cols_remove_14ML_only = names(d0) %in% c("i1.TCA" #                100.000
                                        , "i1.CLfV" #                89.003
                                        , "i1.Inc" #                 87.462
                                        
                                        , "i1.EDnDL" #               81.336
                                        , "i1.MoPolInc" #             80.185
                                        , "i4.ROT" #           78.624
                                        
                                        , "i3.MpPreAu" #             72.713
                                        , "i3.MoLaClm" #             70.466
                                        , "i3.ES" #           60.288

                                       , "i3.NoP" #                 28.276
                                       , "Location.Code" #  26.319
                                       , "i6.MS" #            23.845
                                       , "i6.Ed" #            19.349
                                       , "i6.SC" #i6.SCBranch            16.944
                                       #i5.NoOC                16.818
                                       )

# important by BL only
cols_remove_9BL_only = names(d0) %in% c("i1.TCA"
                                       , "i1.Inc"
                                       , "i3.ES"
                                       
                                       , "i6.MS"
                                       , "i4.ROT"
                                       , "i6.SC" # 6 features - 1st order links
                                       
                                       , "Location.Code"
                                       , "i6.VC"
                                       , "i3.MpPreAu" #3 2nd order features
                                       ) # 9 features

cols_remove_14BL_only = names(d0) %in% c("i1.TCA"
                                        , "i1.Inc"
                                        , "i3.ES"
                                        
                                        , "i6.MS"
                                        , "i4.ROT"
                                        , "i6.SC" # 6 features - 1st order links
                                        
                                        , "Location.Code"
                                        , "i6.VC"
                                        , "i3.MpPreAu" #3 2nd order features
                                        
                                        , "i6.Cov"
                                        , "i1.CLfV"
                                        , "i6.Ed"
                                        , "i6.Gr"
                                        , "VehcSz"
                                        ) # 13 features


cols_remove_none = names(d0) %in% c()
                                  
cols_remove_list = list(#"remove_none" = cols_remove_none, 
                        #"cols_remove_ML" = cols_remove_ML, 
                        #"cols_remove_BL"=cols_remove_BL
                        #"cols_remove_9ML_only" = cols_remove_9ML_only
                        #, "cols_remove_9BL_only" = cols_remove_9BL_only
                        "cols_remove_14ML_only" = cols_remove_14ML_only
                        , "cols_remove_14BL_only" = cols_remove_14BL_only
                        )
print (colnames(d0[cols_remove_14ML_only != cols_remove_14BL_only]))

for (i in names(cols_remove_list)) { 
  print (i); 
  #print (cols_remove_list[[i]]) 

  cols_remove = cols_remove_list[[i]]
  #==============
  output_dir_name = paste("/datasets/discretized-",i, sep="")
  print(output_dir_name)  

  d = data_uncertainty(bc_data = d0_discretize[ !cols_remove ], output_dir_name = output_dir_name, foldout_proportion = 2)
  print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
  
  mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
  save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)
  
  #==============
  output_dir_name = paste("/datasets/nondiscretized-", i)
  print(output_dir_name)  
  
  d = data_uncertainty(bc_data = d0[ !cols_remove ], output_dir_name = output_dir_name, foldout_proportion = 2)
  print_models(file_path = d$file_path, models = d$models, cm_list = d$cm_list, test_data = d$test_data)
  
  mypath <- file.path(d$file_path, paste("saved_workspace", ".RData", sep = ""))
  save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)
}

'
## Very easy  preidciton
summary(d0_discretize$i1.Inc)
d0_i1 = d0_discretize[ d0_discretize$i1.Inc == "[-100,3.33e+04]" , ]
summary(d0_i1$i1.TCA)
d0_i1 = d0_i1[ d0_i1$i1.TCA == "[-2.79,964]", ] # very easy prediction

cols_remove_importfatures = names(d0_i1) %in% c("i1.TCA", "i1.Inc")
d_no_ROT = d0_i1[!cols_remove_importfatures]

## Very bad preidciton
summary(d0_discretize$i1.CLfV)
d0_i1 = d0_discretize[ d0_discretize$i1.CLfV == "[1.82e+03,2.9e+04]",] 
cols_remove_importfatures = names(d0_i1) %in% c("i1.CLfV")
d_no_ROT = d0_i1[!cols_remove_importfatures]

## Very bad preidciton
summary(d0_discretize$i1.EDnDL)
d0_i1 = d0_discretize[ d0_discretize$i1.EDnDL == "[5.94,25.3]",] 
cols_remove_importfatures = names(d0_i1 ) %in% c("i1.EDnDL")
d_no_ROT = d0_i1[!cols_remove_importfatures]
'
