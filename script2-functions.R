#
#script2_funcions
#
remove_minority<-function(bc_data, proportion)
{
  #
  # remove minority class to n=5% 10%
  #
  d1 = bc_data[bc_data$classes == "malignant",]
  d2 = bc_data[bc_data$classes != "malignant",]
  
  n = as.integer( nrow(d2)/ proportion )
  
  n = min(n, nrow(d1) - 2)
  
  d10 = d1[sample(nrow(d1),n),]
  d11 = d1[-sample(nrow(d1),n),]
  print (c( nrow(d1), nrow(d2)))
  stopifnot(nrow(d10) + nrow(d11) == nrow(d1))
  
  d<-rbind(d2, d10)
  d_miss <- rbind(d2, d11)
  
  return (list(bc_data = d, bc_data_missing = d_miss))
  
}


make_gain_curves <- function(score, cls) 
{
  library(ROCR)
  pred1 = ROCR::prediction(abs(score), cls)
  gain = ROCR::performance(pred1, "tpr", "rpp")
  
  gain.x = unlist(slot(gain, 'x.values'))
  gain.y = unlist(slot(gain, 'y.values'))
  
  gain2 = data.frame(x = gain.x, y = gain.y)
  
  return (gain2)
}
plot_gain <- function(model_rf, test_data, train_data, test_predictions, train_predictions,
                      train_name = 'train',
                      file_path="~//", 
                      jpgname='gain_model_rf',
                      classes = "classes",
                      malignant="malignant") 
{ 
  if (missing(test_predictions)) {
    final <- data.frame(actual = test_data$classes,
                        predict(model_rf, newdata = test_data, type = "prob"))
  } else {
    final <- data.frame(actual = test_data[,classes], test_predictions)
  }
  
  gain <- make_gain_curves(score =  final[,malignant], cls = as.array(test_data[,classes])) 
  
  if (missing(train_predictions)) {
    final2 <- data.frame(actual = train_data$classes,
                         predict(model_rf, newdata = train_data, type = "prob"))
  } else {
    final2 <- data.frame(actual = train_data[,classes],train_predictions)
  }
  
  score2 = final2[,malignant] #benign
  cls2 = as.array(train_data[,classes])
  
  gain2 <- make_gain_curves(score2, cls2) 
  
  graphics.off()
  
  plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
       ylab="True Positive Rate", 
       xlab="Rate of Positive Predictions")
  
  lines(x=gain$x, y=gain$y, col="green", lwd=2)
  lines(x=gain2$x, y=gain2$y, col="blue", lwd=2)
  
  legend('topright', 
         legend=c('random', 'test', train_name) , 
         lty=c(1,2,3), 
         bty='n', cex=2.75, 
         col=c('red', 'green', 'blue'))
  
  mypath <- file.path(file_path, paste("gain_", jpgname, ".jpg", sep = ""))
  print(mypath)
  dev.copy(jpeg,filename=mypath) 
  dev.off()
  
  result <- list(gaintestx=gain$x, gaintesty=gain$y, gaintrain=gain2)
  
  return(result)
}


make_model_rf <- function(test_data, train_data, tr_n, tr_r, tr_numtree=10, method_rf = "rf", par_sampling="none", isprint="TRUE") 
{
  print (par_sampling) #'none sampling')
  if (par_sampling == "none")
    ctrl <- trainControl(method = "repeatedcv", 
                         number = tr_n, 
                         repeats = tr_r, 
                         
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         
                         allowParallel = TRUE,
                         verboseIter = FALSE
    )
  else
    ctrl <- trainControl(method = "repeatedcv", 
                         number = tr_n, 
                         repeats = tr_r, 
                         verboseIter = FALSE,
                         
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         
                         allowParallel = TRUE,
                         sampling = par_sampling)#"down")
  
  #metric="ROC",
  #trControl = trainControl(#summaryFunction=twoClassSummary, classProbs=T,savePredictions = T,
  
  set.seed(42)
  if (method_rf == "gbm")
    model_rf <- caret::train(classes ~ .,
                             data = train_data,
                             method = method_rf,
                             #ntree = tr_numtree,
                             
                             metric = "ROC",
                             #metric = "Kappa",
                             
                             preProcess = c("scale", "center"),
                             trControl = ctrl)
  if (method_rf == "rf")
    model_rf <- caret::train(classes ~ .,
                             data = train_data,
                             method = method_rf,
                             ntree = tr_numtree,
                             
                             metric = "ROC",
                             #metric = "Kappa",
                             
                             preProcess = c("scale", "center"),
                             trControl = ctrl)
  
  model_rf
  
  
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

plot_ROC <- function(model_rf, test_data, train_data, 
                     file_path="~//", 
                     jpgname='model_rf') 
{
  result <- list()
  rfFit <- model_rf
  selectedIndices <- rfFit$pred$mtry == 2

  if (TRUE) {
    #Draw the ROC curve 
    
    gbm2.probs <- predict(model_rf,newdata = test_data,type="prob")
    
    gbm2.ROC <- roc(predictor=gbm2.probs$malignant,
                    response=test_data$classes,
                    levels=rev(levels(test_data$classes)))
    gbm2.ROC$auc
    
    gbm3.probs <- predict(model_rf,newdata = train_data,type="prob")
    
    gbm3.ROC <- roc(predictor=gbm3.probs$malignant,
                    response=train_data$classes,
                    levels=rev(levels(train_data$classes)))
    gbm3.ROC$auc
    
    
    roc_title = paste("AUC_test =", round(gbm2.ROC$auc, 3), "AUC_train=", round(gbm3.ROC$auc, 3))
    roc_title
    
    mypath <- file.path(file_path, paste("plotROC_", jpgname, ".jpg", sep = ""))

    print(mypath)
    
    "
    x11()
    dev.off() only works in an interactive session. If you're interested in implementing such behavior in a script, you should use
    "
    graphics.off()
    
    par(fig=c(0,0.8,0,0.8))
    plot(gbm3.ROC,main=roc_title,col='blue')
    par(fig=c(0,0.8,0,0.8), new=TRUE)
    plot(gbm2.ROC,col='red')#,main=as.character(gbm2.ROC$auc))
    
    
    dev.copy(jpeg,filename=mypath) #paste("plotROC_", jpgname, ".jpg", sep = ""));
    dev.off()
    
    
    #histogram(~gbm2.probs$malignant|test_data$classes,xlab="Histogram misbalance")  
    result <- list(auc_test = gbm2.ROC$auc, auc_train = gbm3.ROC$auc)
  }
  
  return(result)
}

plot_results <- function(models, cm_list)
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
    #model <- get(paste0("cm_", name))
    model <- cm_list[paste0("cm_", name)]
    
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
  
  p1 =
    comparison %>%
    gather(x, y, Sensitivity:F1) %>%
    ggplot(aes(x = x, y = y, color = model)) +
    geom_jitter(width = 0.3, alpha = 0.7, size = 4)
  
  ggsave('p1.jpg', p1)
  assign('p1', p1, envir=.GlobalEnv)
  
  return(comparison)
}

