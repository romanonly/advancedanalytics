"
NUMERICAL pCA
"
# PLOT ONLY SMALL data
#PLOT
#keep ALL MINOR and 5Xmore majors
select_small_subset_minority_majority <- function (data, target, MajorIsTimesMinor, num_minor_plot)
{
  
  x0 = data[target == "malignant",]
  
  x1 = data[target == "benign",]
  n = min(nrow(x1), MajorIsTimesMinor*nrow(x0))
  
  stopifnot(n > 0)
  #x1 = x1[1:n,]
  #x1 = x1[sample(n),]
  
  num_plot0 = min(num_minor_plot, nrow(x0))
  num_plot1 = min(num_plot0*MajorIsTimesMinor,nrow(x1))
  
  x00 = x0[sample(num_plot0),]
  x11 = x1[sample(num_plot1),]
  
  x22=rbind(x00,x11)
  
  x22colors = c(rep("minority" ,nrow(x00)), rep("majority", nrow(x11)))
  stopifnot(nrow(x22) == length(x22colors))
  
  return (list(x0=x0, x1=x1, ret_x0x1=x22, ret_colors = x22colors))
}

temp_pca <- function () {
  
if (FALSE) {
  library(psych)
  pca<-principal(data$data_num,nfactor=6,rotate="none") #forc
  pca
  
  library("bigpca")
  #bmat <- as.big.matrix(data$data_num)
  if(file.exists("testMyBig2.bck")) { unlink(c("testMyBig.bck","testMyBig.dsc")) }
  bmat <- as.big.matrix(data.matrix( data$data_num ),backingfile="testMyBig2.bck",
                        descriptorfile="testMyBig.dsc", backingpath = getwd())
  
  result <- big.PCA(bmat) #,verbose=TRUE)
  headl(result)
  # plot the eigenvalues with a linear fit line and elbow placed at 13
  Eigv <- pca.scree.plot(result$Evalues,M=bmat,elbow=6,printvar=FALSE)
}

}
#labels = data$data_binary$classes
#pcadata = data$data_num
#bin_data = data$data_binary
#data_binary = data$data_binary
#MajorIsTimesMinor=2; num_minor_plot=100
#pca = P$pca
pca_numerical_plot <- function(file_path, pca, pcadata, 
                               data_binary,
                               #labels, 
                               #small_pcadata, small_labels, 
                               #x_small_pcadata, x_small_labels, 
                               #num_plot_cluster=300,
                               MajorIsTimesMinor=10, num_minor_plot=100
                               )
{
  labels = data_binary$classes
  sc_pcadata <- scale(pcadata, center=TRUE, scale=TRUE)
  #par(mar = rep(2, 4)) #plot to show variable importance
  jpgname='analysis'
  
  Psmall = select_small_subset_minority_majority(data =pcadata, target=data_binary$classes, MajorIsTimesMinor, num_minor_plot)
  small_pcadata = Psmall$ret_x0x1
  small_labels = Psmall$ret_colors
  xPsmall = select_small_subset_minority_majority(data=pca$x, target = data_binary$classes, MajorIsTimesMinor, num_minor_plot)
  x_small_pcadata = xPsmall$ret_x0x1
  x_small_labels = xPsmall$ret_colors
  
  
  graphics.off()
  plot(colMeans(data.matrix(sc_pcadata)), xlab="Columns scaled data")
  mypath <- file.path(file_path, paste("pca_means_scales_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()
  
  
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
  plot(pca$x[,1:2], col=as.integer(as.factor(labels)))
  plot(pca$x[,2:3], col=as.integer(as.factor(labels)))
  #plot(x_small_pcadata[,1:2], col=as.integer(as.factor(x_small_labels)))
  #plot(x_small_pcadata[,2:3], col=as.integer(as.factor(x_small_labels)))
  mypath <- file.path(file_path, paste("pca_all_x_1_2_3_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()
  
  graphics.off()
  par(mfrow=c(1,2))
  plot(x_small_pcadata[,1:2], col=as.integer(as.factor(x_small_labels)))
  plot(x_small_pcadata[,2:3], col=as.integer(as.factor(x_small_labels)))
  mypath <- file.path(file_path, paste("pca_small_x_1_2_3_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()

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
  

  try(
    {
  #pc<-princomp(sc_pcadata) # pcadata - cov negative!!!
  #small_pcadata = Psmall$ret_x0x1
  #small_labels = Psmall$ret_colors
  pc_data_labels = as.factor(small_labels) #[sel_sample]

  
  #remove correlted variables
  tmp <- abs(cor(small_pcadata))
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  corr_cols = apply(tmp,2,function(x) any(x > 0.99))
  small_pcadata = small_pcadata[,which(corr_cols==FALSE)]
  sc_small_pcdata = scale(small_pcadata, center=TRUE, scale=TRUE)
  print(summary(corr_cols))
  
  #remove correlted variables
  tmp <- abs(cor(sc_small_pcdata))
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  # Above two commands can be replaced with 
  # tmp[!lower.tri(tmp)] <- 0
  corr_cols = apply(tmp,2,function(x) any(x > 0.99))
  sc_small_pcdata = sc_small_pcdata[,corr_cols==FALSE]
  print(summary(corr_cols))
  
  pc <- princomp(sc_small_pcdata)
  #sel_sample = sample(nrow(sc_small_pcdata)) #())
  pc_data = pc$scores #[sel_sample,]
  
  graphics.off(); par(mfrow=c(1,1))
  pairs(pc_data[,1:3], col=rainbow(3)[(pc_data_labels) ], asp=1)
  mypath <- file.path(file_path, paste("pca_princomp_1_2_3", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()

  graphics.off(); par(mfrow=c(1,1))
  pairs(pc_data[,4:6], col=rainbow(3)[ pc_data_labels ], asp=1)
  mypath <- file.path(file_path, paste("pca_princomp_4_5_6", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()

  graphics.off(); par(mfrow=c(1,1))
  scatterplot3d(pc_data[,c(1,2,3)], color=rainbow(3)[pc_data_labels])
  mypath <- file.path(file_path, paste("pca_princomp_3d_1_2_3", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off(); par(mfrow=c(1,1))
  scatterplot3d(pc_data[,c(4,5,6)], color=rainbow(3)[pc_data_labels])
  mypath <- file.path(file_path, paste("pca_princomp_3d_4_5_6", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
    }
  )
  
  # Manipulate data for PCA Analyis ----------------------------------------------
  # K-Means Clustering (used later) ----------------------------------------------
  set.seed(11) # For reproducibility
  
  #library(caret)
  # Assuming goal class is column 10
  #sc_pcadata <- preProcess(data.matrix(pcadata), method=c("center", "scale"))
  
  #small_num_plot=sample(MajorIsTimesMinor*num_plot_cluster)
  try(
    {
      
  small_sc_pcadata = sc_small_pcdata #sc_pcadata[small_num_plot,]
  small_labels = pc_data_labels #labels[small_num_plot ]
  
  small_pca =prcomp(small_sc_pcadata, scale = T, center = T) #pcadata[sample(10*num_plot),]
  
  print("PCA clusters num=4,5")
  print(str(small_sc_pcadata))
  km4.out <- kmeans(small_sc_pcadata, centers = 4, nstart = 50)
  km5.out <- kmeans(small_sc_pcadata, centers = 5, nstart = 50)
  #library(ggfortify) # For fortify()
  pca.fortify <- fortify(small_pca) # fortify() gets pca into usable format
  # Add group (short for color) column using k=4 and k=5 groups
  pca4.dat <- cbind(pca.fortify, group=km4.out$cluster)
  pca5.dat <- cbind(pca.fortify, group=km5.out$cluster)
  # Plotting PC1 and PC2 using ggplot and plotly ---------------------------------
  #library(ggplot2)
  #library(plotly)
  # Script for plotting k=4
  gg2 <- ggplot(pca5.dat) +
    geom_point(aes(x=PC1, y=PC2, col=factor(group), text=rownames(pca5.dat)), size=2) +
    labs(title = "Visualizing K-Means Clusters Against First Two Principal Components") +
    scale_color_brewer(name="name", palette = "Set1")
  # Use plotly for inteactivity
  #plotly2 <- ggplotly(gg2, tooltip = c("text", "x", "y")) %>% layout(legend = list(x=.9, y=.99))
  graphics.off(); par(mfrow=c(1,1))
  plot(gg2)
  mypath <- file.path(file_path, paste("pca_prcomp_clusters5_pc1_pc2", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  gg2 <- ggplot(pca5.dat) +
    geom_point(aes(x=PC2, y=PC3, col=factor(group), text=rownames(pca5.dat)), size=2) +
    labs(title = "Visualizing K-Means Clusters Against 2nd 3rd Principal Components") +
    scale_color_brewer(name="name", palette = "Set1")
  graphics.off(); par(mfrow=c(1,1))
  plot(gg2)
  mypath <- file.path(file_path, paste("pca_prcomp_clusters5_pc2_pc3", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  gg2 <- ggplot(pca5.dat) +
    geom_point(aes(x=PC1, y=PC2, col=factor(small_labels), text=rownames(pca5.dat)), size=2) +
    labs(title = "Visualizing labels Against First Two Principal Components") +
    scale_color_brewer(name="", palette = "Set1")
  graphics.off(); par(mfrow=c(1,1))
  plot(gg2) 
  mypath <- file.path(file_path, paste("pca_prcomp_clusters5_pc1_pc2_labels", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  # Plot Pc1 Pc2
  #http://amunategui.github.io/high-demensions-pca/
  #labels = as.numeric(as.factor(labels))
  dfEvaluate <- cbind(as.data.frame(pca.fortify), small_labels=small_labels)
  
  gg = ggplot(dfEvaluate, aes(x=PC1, y=PC2, colour=as.factor(small_labels))) +
    geom_point(aes(shape=as.factor(small_labels))) + scale_colour_hue()
  graphics.off(); par(mfrow=c(1,1))
  plot(gg)
  mypath <- file.path(file_path, paste("pca_pc1_pc2_small_labels", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  }
  )
  
  #=====================================
  # remove features with less than 0.01% variance:
  cut_off = 0.04
  nzv <- nearZeroVar(sc_pcadata, saveMetrics = TRUE)
  print(paste('Range:',range(nzv$percentUnique)))
  print(nzv[1:20,]) #head(nzv))
  print(paste('Column count before cutoff:',ncol(sc_pcadata)))
  print(dim(nzv[nzv$percentUnique > cut_off,]))
  #rownames = names(pcadata)
  cutoff_cols = rownames(nzv[nzv$percentUnique > cut_off,])
  sc_pcadata_cutoff = sc_pcadata[, !colnames(sc_pcadata) %in% cutoff_cols]
  print(paste(" cutoff names=", colnames(sc_pcadata_cutoff)))
  
}



pca_numerical<-function(pcadata=pcadata, labels=labels,
                        num_pca_comps = 15
                        ) 
{
  sc_pcadata <- scale(pcadata, center=TRUE, scale=TRUE)
  
  print(" ************ PCA NUMERICAL *******************")
  #head(crimtab) #show sample data
  print( dim(sc_pcadata)) #check dimensions
  
  sum(sc_pcadata) 
  #colnames(crimtab)
  #apply(sc_pcadata,2,var) #check the variance accross the variables
  mean_var = sum( apply(sc_pcadata,2,var) ) / ncol(sc_pcadata)
  print(paste(" mean_var after scaling = ", as.character(mean_var)))
  stopifnot(1e-6 > abs(1.0 - mean_var))
  
  
  pca =prcomp(pcadata, scale = T, center = T) 
  
  'below code changes the directions of the biplot, if we donot include
  the below two lines the plot will be mirror image to the below one.
  pca$rotation=-pca$rotation
  pca$x=-pca$x
  '
  
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
  return (list(pca_columns = ret_pca_columns, pca=pca , scaled_pcadata=sc_pcadata, ret_err=err))
}



"

Binary PCA


library(logisticPCA)
library(ggplot2)
library(discretization)
"

pca_binary <- function(dbin_uncorr, 
                       target,
                       NumK = 2, 
                       MajorIsTimesMinor = 10, num_minor_plot = 10000,
                       file_path)
{
  stopifnot(ncol(target) == 1)
  jpgname = ""#"pca_binary"
  stopifnot(("logisticPCA" %in% (.packages())))
  stopifnot(("discretization" %in% (.packages())))
  
  print("PCA binary vars")

  
  # PREPARE Matrix for PCA: remove NAs
  x = dbin_uncorr
  print("PCA binary")
  print(names(x))
  print("PCA binary remove NA")
  print(nrow(x))
  
  row.has.na <- apply(x, 1, function(x){any(is.na(x))})
  row.has.na.sum = sum(row.has.na)
  x.filtered <- x[!row.has.na.sum]
  print(paste(" na filtered rows number: after remove na, before = ",as.character(nrow(x.filtered)),as.character( nrow(x))))
  print(nrow(x.filtered))
  
  data_bin = x.filtered # yes i can use filetered  rows - later cpnvert all !!! as it wont fit all other vars!!! x.filtered
  
  #keep ALL MINOR and 5Xmore majors
  Psmall = select_small_subset_minority_majority(data=data_bin, target=target, MajorIsTimesMinor, num_minor_plot)
  #small_labels = Psmall$ret_colors
  
  # PCA for Minor + Major=5xminor
  data0 = data.matrix(Psmall$ret_x0x1) #data3)
  data0 = data0 - 1 
  logsvd_model = logisticSVD(data0, k = NumK)
  
  print("logsvd_model")
  print(logsvd_model) #logsvd_model$prop_deviance_expl
  
  
  ' For logistic PCA, we want to first decide which m to use with cross validation. 
  We are assuming k = 2 and trying different ms from 1 to 10. '
  print("logpca_cv ... ")
  logpca_cv = cv.lpca(data0, ks = NumK, ms = 1:10)
  print(logpca_cv)

  
  'It looks like the optimal m is 5, which we can use to fit with all the data. 
  We will also use the same m for the convex formulation.'
  print("logpca_model ... ")#
  logpca_model = logisticPCA(data0, k = NumK, m = which.min(logpca_cv))
  print(logpca_model)#logpca_model$prop_deviance_expl # 68.2%
  
  
  'Each of the formulations has a plot method to make it easier to see the results of the fit and 
  assess convergence. There are three options for the type of plot. The first is type = "trace", which plots the deviance as a function of iteration. For logistic PCA and logistic SVD, the deviance should decrease at each iteration, but not necessarily for convex logistic PCA. 
  For example, convex logistic PCA converged in 12 iterations.'
  print("clogpca_model ... ") #clogpca_model$prop_deviance_expl # 91.3%
  clogpca_model = convexLogisticPCA(data0, k = NumK, m = which.min(logpca_cv))
  print(clogpca_model) #clogpca_model$prop_deviance_expl # 91.3%
  
    
  #============
  #MAKE NnmK dim numerical PCA values for data=dbin
  # convert ALL rows
  dbin_mat = dbin_uncorr
  dims=length(colnames(dbin_uncorr))
  
  dbin_mat = -1 + data.matrix(dbin_mat)
  nfakes=nrow(dbin_mat)
  
  votes_fake = matrix(dbin_mat, nfakes, dims, dimnames = list(NULL,colnames(dbin_uncorr))) #list(NULL, colnames(d)))
  
  pca_bin_log = predict(logpca_model, votes_fake, type = "PCs")
  pca_bin_log_df = data.frame(pca_bin_log) #X1 X2
  
  pca_bin_clog = predict(clogpca_model, votes_fake, type = "PCs") #BEST
  pca_bin_clog_df = data.frame(pca_bin_clog) #X1 X2
  
  pca_bin_svd = predict(logsvd_model, votes_fake, type = "PCs")
  pca_bin_svd_df = data.frame(pca_bin_svd) #X1 X2
  
  ret = list(clogpca_model = clogpca_model, pca_bin_clog_df = pca_bin_clog_df, 
             logpca_model = logpca_model, pca_bin_log_df = pca_bin_log_df, 
             logsvd_model=logsvd_model, pca_bin_svd_df=pca_bin_svd_df,
             Psmall = Psmall,
             logpca_cv = logpca_cv
  )
  return (ret)
}

pca_binary_plot<-function(Psmall, logpca_model,  clogpca_model, logsvd_model,logpca_cv)
{
  jpgname = "pca_binary"

  graphics.off(); par(mfrow=c(1,1))
  mypath <- file.path(file_path, paste("binpca_logsvd_", jpgname, ".jpg", sep = ""))
  print(mypath); 
  plot(logsvd_model$loss_trace, main = "logsvd_model",   xlab = "iterations", ylab = "convergence")
  dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off(); par(mfrow=c(1,1))
  #plot(logpca_cv)
  plot(data.matrix(logpca_cv), main = "logsvd_model",   xlab = "iterations", ylab = "convergence")
  mypath <- file.path(file_path, paste("binpca_logpca_cv_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  graphics.off(); par(mfrow=c(1,1))
  #plot(logpca_model, type = "trace")
  temp = logpca_model$loss_trace
  plot(temp, main = "logsvd_model",   xlab = "iterations", ylab = "convergence")
  mypath <- file.path(file_path, paste("binpca_logpca_model_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  graphics.off(); par(mfrow=c(1,1))
  #plot(clogpca_model, type = "trace")
  plot(clogpca_model$loss_trace, main = "logsvd_model",   xlab = "iterations", ylab = "convergence")
  mypath <- file.path(file_path, paste("binpca_Clogpca_model_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  x22 = Psmall$ret_x0x1
  x22mat = data.matrix(x22)
  x22colors = Psmall$ret_colors
  
  nfakes=nrow(x22)
  d = ncol(x22mat)
  votes_fake = matrix(x22mat, nfakes, d,dimnames = list(NULL, colnames(x22)))
  
  pca_bin = predict(logpca_model, votes_fake, type = "PCs")
  #plot(pca_bin, col=(x2colors))

  NJIT = 16
  graphics.off(); par(mfrow=c(1,1))
  plot(jitter(pca_bin[,1],NJIT)~jitter(pca_bin[,2],NJIT), col=as.numeric(as.factor(x22colors)))
  mypath <- file.path(file_path, paste("binpca_predict_logpca_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  pca_bin2=predict(clogpca_model, votes_fake, type = "PCs")
  graphics.off(); par(mfrow=c(1,1))
  plot(jitter(pca_bin2[,1],NJIT)~jitter(pca_bin2[,2],NJIT), col=as.numeric(as.factor(x22colors)))
  mypath <- file.path(file_path, paste("binpca_predict_Clogpca_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  pca_bin3=predict(logsvd_model, votes_fake, type = "PCs")
  graphics.off(); par(mfrow=c(1,1))
  plot(jitter(pca_bin3[,1],10*NJIT)~jitter(pca_bin3[,2],10*NJIT), col=as.numeric(as.factor(x22colors)))
  mypath <- file.path(file_path, paste("binpca_predict_logsvd_", jpgname, ".jpg", sep = ""))
  print(mypath); dev.copy(jpeg,filename=mypath); dev.off()
  
  'All three formulations do a good job of separating the political parties based on voting record alone.'
  '
  classes = data$data_binary$classes
  party = rownames(x)
  plot(logsvd_model, type = "scores") + geom_point(aes(colour = as.factor(x0x1_colors))) + 
  ggtitle("Exponential Family PCA") + scale_colour_manual(values = c("blue", "red"))
  
  plot(logpca_model, type = "scores") + geom_point(aes(colour = x0x1_colors)) + 
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("blue", "red"))
  
  plot(clogpca_model, type = "scores") + geom_point(aes(colour = x0x1_colors)) + 
  ggtitle("Convex Logistic PCA") + scale_colour_manual(values = c("blue", "red"))
  '
  
  
  'One can also examine the latent space of the variables by using type = "loadings".'
  'The fitted function provides fitted values of either probabilities or natural parameters. For example,'
  #head(fitted(logpca_model, type = "response"), 2)
  
  

}

