'
source("roman-functions.R")

pathnames <- list.files(pattern="[.]R$", path="R/", full.names=TRUE);
sapply(pathnames, FUN=source);



  FYI, in the R.utils package there is a utility function
sourceDirectory() that makes this even easier- this function can keep
track of what files have been changed since last time you sourced a
directory.  Just do

sourceDirectory("R/", modifiedOnly=TRUE);

to load all your *.R scripts.  If you then update one of the files, just do

sourceDirectory("R/", modifiedOnly=TRUE);

'


plot_ROC <- function(model_rf, test_data, train_data, npar=TRUE,print=TRUE) {
  result <- list()
  rfFit <- model_rf
  selectedIndices <- rfFit$pred$mtry == 2
  if(FALSE) {
    # Select a parameter setting
    plot.roc(rfFit$pred$obs[selectedIndices],
             rfFit$pred$malignant[selectedIndices])
  }
  if (FALSE) {
    obs <- rfFit$pred$obs[selectedIndices]
    
    g <- ggplot(rfFit$pred[selectedIndices, ], 
                aes(m='malignant', 
                    d=factor(obs, levels = rev(levels(train_data$classes)) ))) + #c("benign", "malignant")))) + 
      geom_roc(n.cuts=0) + 
      coord_equal() +
      style_roc()
    
    g + annotate("text", x=0.75, y=0.25, label=paste("AUC1 =", round((calc_auc(g))$AUC, 3), "AUC2=", round(gbm.ROC$auc, 3)))
  }
  if (TRUE) {
    #Draw the ROC curve 
    
    if (FALSE) {
      gbm.probs <- predict(model_rf,newdata = test_data,type="prob")
      #head(gbm.probs)
      gbm.ROC <- roc(predictor=gbm.probs$benign,
                     response=test_data$classes,
                     levels=rev(levels(test_data$classes)))
      gbm.ROC$auc
      plot(gbm.ROC,main=as.character(gbm.ROC$auc))
    }    
    gbm2.probs <- predict(model_rf,newdata = test_data,type="prob")
    
    gbm2.ROC <- roc(predictor=gbm2.probs$malignant,
                    response=test_data$classes,
                    levels=rev(levels(test_data$classes)))
    gbm2.ROC$auc
    
    par(new=FALSE)
    plot(gbm2.ROC,col='red')#,main=as.character(gbm2.ROC$auc))
    
    gbm3.probs <- predict(model_rf,newdata = train_data,type="prob")
    
    gbm3.ROC <- roc(predictor=gbm3.probs$malignant,
                    response=train_data$classes,
                    levels=rev(levels(train_data$classes)))
    gbm3.ROC$auc
    par(new=TRUE)
    
    roc_title = paste("AUC_test =", round(gbm2.ROC$auc, 3), "AUC_train=", round(gbm3.ROC$auc, 3))
    roc_title
    plot(gbm3.ROC,main=roc_title,col='blue')
    #histogram(~gbm2.probs$malignant|test_data$classes,xlab="Histogram misbalance")  
    result <- list(auc_test = gbm2.ROC$auc, auc_train = gbm3.ROC$auc)
  }
  
  return(result)
}


make_model_rf <- function(test_data, train_data, tr_n, tr_r, tr_numtree, par_sampling="none", isprint=TRUE) 
  {
    print (par_sampling) #'none sampling')
    if (par_sampling == "none")
      ctrl <- trainControl(method = "repeatedcv", 
                           number = tr_n, 
                           repeats = tr_r, 
                           
                           #summaryFunction = twoClassSummary,
                           #classProbs = TRUE,
                           
                           verboseIter = FALSE
                           )
    else
      ctrl <- trainControl(method = "repeatedcv", 
                           number = tr_n, 
                           repeats = tr_r, 
                           verboseIter = FALSE,
                           
                           #summaryFunction = twoClassSummary,
                           #classProbs = TRUE,
                           
                           sampling = par_sampling)#"down")
    
    #metric="ROC",
    #trControl = trainControl(#summaryFunction=twoClassSummary, classProbs=T,savePredictions = T,
    
    set.seed(42)
    model_rf <- caret::train(classes ~ .,
                             data = train_data,
                             method = "rf",
                             ntree = tr_numtree,
                            
                             #metric = "ROC",
                             #metric = "Kappa",
                            
                             preProcess = c("scale", "center"),
                             trControl = ctrl)
    
    final <- data.frame(actual = test_data$classes,
                        predict(model_rf, newdata = test_data, type = "prob"))
    
    final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
    
    cm_original <- confusionMatrix(final$predict, test_data$classes)#, mode = "prec_recall"
    
    final$correct <- ifelse(final$actual == final$predict, TRUE, FALSE)
    
    if (isprint) {
      print ("plot final")
      
      ggplot(final, aes(x = predict, fill = correct)) +
        geom_bar(position = "dodge")
      
      ggplot(final, aes(x = predict, y = benign, color = correct, shape = correct)) +
        geom_jitter(size = 3, alpha = 0.6)
      
  }
  
  result <- list(model = model_rf, cm=cm_original, final=final)
  return(result)
}

plot_gain <- function(model_rf, test_data, train_data, test_predictions, train_predictions) { #, npar=TRUE,print=TRUE) {
  # y <- mysummary(x)
  # y$center is the median (4) 
  # y$spread is the median absolute deviation (1.4826)
  t_data <- test_data
  
  if (missing(test_predictions)) {
    final <- data.frame(actual = t_data$classes,
                        predict(model_rf, newdata = t_data, type = "prob"))
  } else {
    final <- data.frame(actual = t_data$classes, test_predictions)
  }
  score = final$malignant #benign
  cls = as.array(t_data$classes)
  
  pred = prediction(score, cls)
  gain = performance(pred, "tpr", "rpp")
  
  gain.x = unlist(slot(gain, 'x.values'))
  gain.y = unlist(slot(gain, 'y.values'))
  
  t2_data <- train_data
  if (missing(train_predictions)) {
    final2 <- data.frame(actual = t2_data$classes,
                        predict(model_rf, newdata = t2_data, type = "prob"))
  } else {
    final2 <- data.frame(actual = t2_data$classes,train_predictions)
  }
  
  score2 = final2$malignant #benign
  cls2 = as.array(t2_data$classes)
  
  pred2 = prediction(score2, cls2)
  gain2 = performance(pred2, "tpr", "rpp")
  gain2.x = unlist(slot(gain2, 'x.values'))
  gain2.y = unlist(slot(gain2, 'y.values'))
  
  plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
       ylab="True Positive Rate", 
       xlab="Rate of Positive Predictions")
  
  #lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)
  #plot(gain, col="orange", lwd=2)
  lines(x=gain.x, y=gain.y, col="green", lwd=2)
  lines(x=gain2.x, y=gain2.y, col="blue", lwd=2)
  
  legend('topright', 
         legend=c('random', 'test', 'train') , 
         lty=c(1,2,3), 
         bty='n', cex=2.75, 
         col=c('red', 'green', 'blue'))
  
  # Plot the performance of the model applied to the evaluation set as
  # an ROC curve.
  #require(ROCR)
  '
  pred <- prediction(x.evaluate$probabilities, x.evaluate$Kyphosis)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, main="ROC curve", colorize=T)
  
  # And then a lift chart
  perf <- performance(pred,"lift","rpp")
  plot(perf, main="lift curve", colorize=T)
  '
  result <- list(gaintest=gain,gaintrain=gain2)
  return(result)
}

read_data<-function(proportion = 10)
{
  ##summary(cars)
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
  
  #
  # remove minority class to n=5% 10%
  #
  d1 = bc_data[bc_data$classes == "malignant",]
  d2 = bc_data[bc_data$classes != "malignant",]
  
  n = as.integer( nrow(d2)/ proportion )
  d10 = d1[sample(nrow(d1),n),]
  
  d<-rbind(d2, d10)
  summary(d)
  ggplot(d, aes(x = classes, fill = classes)) +
    geom_bar()
  
  bc_data = d
  
  return(bc_data)
}

make_tomek <- function(train_data)
{
  dT <- train_data
  dT$Target = as.factor(dT$classes == "malignant")
  dT <- subset(dT, select = -c(classes ))
  colnames(dT)[colnames(dT) == 'Target'] <- 'classes'
  
  
  target <- subset(dT, select = c(classes))
  target = as.integer(target == TRUE) #TRUE)# as.factor(target$classes==TRUE))# == "malignant") )
  unique(target)
  assert_that(mean(target)<0.2)
  input <- subset(bc_data, select = -c(classes))
  
  target = 1 - target
  data<-ubTomek(X=input, Y= target)#, positive="malignant")
  
  d<-cbind(data$X, data$Y, verbose = TRUE)
  
  #d$classes = as.factor(d$classes)
  
  colnames(d)[colnames(d) == 'data$Y'] <- 'classes'
  
  d$predict <- as.factor( ifelse(d$classes == 1, "benign", "malignant") )
  
  d <- subset(d, select = -c(classes,verbose))
  colnames(d)[colnames(d) == 'predict'] <- 'classes'

  return(d)
}



plot_results <- function(models)
{
  
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         PosPredValue  = rep(NA, length(models)),      
                         NegPredValue = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))

for (name in names(models)) {
  model <- get(paste0("cm_", name))
  
  #  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
  #    mutate(Sensitivity = model$byClass["Sensitivity"],
  #           Specificity = model$byClass["Specificity"],
  #           Precision = model$byClass["Precision"],
  #           Recall = model$byClass["Recall"],
  #           F1 = model$byClass["F1"])
  comparison[comparison$model == name, ] $ F1 = model$byClass["F1"]
  comparison[comparison$model == name, ] $ Specificity = model$byClass["Specificity"]
  comparison[comparison$model == name, ] $ Sensitivity = model$byClass["Sensitivity"]
  comparison[comparison$model == name, ] $ Precision = model$byClass["Precision"]
  comparison[comparison$model == name, ] $ Recall = model$byClass["Recall"]
  comparison[comparison$model == name, ] $ PosPredValue = model$byClass["Pos Pred Value"]
  comparison[comparison$model == name, ] $ NegPredValue = model$byClass["Neg Pred Value"]
}

#library(tidyr)
p1 =
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.3, alpha = 0.7, size = 4)

ggsave('p1.jpg', p1)
assign('p1', p1, envir=.GlobalEnv)

  return(comparison)
##==##``` 
}
