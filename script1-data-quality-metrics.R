library(caret)

library(mice)
library(ggplot2)

library(pcaGoPromoter)
library(ellipse)

library(doParallel)

# READ and PARTITION

library(lattice)


PATHNAME = "~/R/romaprojects/Webinar_ISDS"
PATHNAME = "C://Users//rkazinnik//Documents//R//romaprojects//Webinar_ISDS"

setwd(PATHNAME)
source("script2-functions.R")


file_path=paste(PATHNAME, "//datasets//bc_data_1percent", sep="")
dir.create(file_path) # file.path(mainDir, subDir))
data_file_name="bcdata_model"


# output re-directed in cwd. output is appended
# to existing file. output also send to terminal. 
mypath <- file.path(file_path, paste("report_script1_", data_file_name, ".txt", sep = ""))
sink(mypath, append=FALSE, split=TRUE)

bc_data <- read_data(proportion = 100)

print(file_name)
print(summary(bc_data$classes))
colnames(bc_data)

# Train and test partition
set.seed(42)
index <- caret::createDataPartition(bc_data$classes, p = 0.5, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]


# Plot train vs test distributions
rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>%
  gather(x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)



# MODELING
# caret currently fails for multi-core: make sure to use sequential processing
library(iterators)
library(foreach)
library(parallel)
library(doParallel)
no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores, outfile=paste0('./info_parallel.log'))
registerDoParallel(cl)
stopCluster(cl)
registerDoSEQ()


library(mlbench)
library(plotROC)
library(pROC)

library('ROCR')
library(jpeg)
library(tidyr)

tr_n = 10 # number - Either the number of folds or number of resampling iterations
tr_r = 10 # repeat - For repeated k-fold cross-validation only: the number of complete sets of folds to compute
tr_numtree = 100

library(plyr); library(dplyr)



models <- list()

method = 'rf' # gbm' # C5.0' # 'gbm' # 'rf'

model_rf_under <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, method, par_sampling="down")
if (exists("model_rf_under")) {
  gainsUNDER <- plot_gain(model_rf_under$model, test_data, train_data, file_path=file_path, jpgname='model_under')
  plot_ROC(model_rf_under$model, test_data, train_data, file_path=file_path, jpgname='model_rf_under')
  print( varImp(model_rf_under$model))
  cm_under <- model_rf_under$cm
  models <- append(models, list(under = model_rf_under$model))
}


model_rf <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, method) 
if (exists("model_rf")) {
  gains <- plot_gain(model_rf$model, test_data, train_data, file_path=file_path, jpgname='gain_model_rf')
  plot_ROC(model_rf$model, test_data, train_data, file_path=file_path, jpgname='model_rf')
  print(  varImp(model_rf$model) )
  cm_original <- model_rf$cm
  models <- append(models, list(original = model_rf$model))
}

model_rf_over <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, method, par_sampling="up")
if (exists("model_rf_over")) {
  gainsOVER <- plot_gain(model_rf_over$model, test_data, train_data, file_path=file_path, jpgname='model_over')
  plot_ROC(model_rf_over$model, test_data, train_data, file_path=file_path, jpgname='model_rf_over')
  print( varImp(model_rf_over$model))
  cm_over <- model_rf_over$cm
  models <- append(models, list(over = model_rf_over$model))
}

model_rf_rose <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, method, par_sampling="rose")
if (exists("model_rf_rose")) {
  gainsROSE <- plot_gain(model_rf_rose$model, test_data, train_data, file_path=file_path, jpgname='model_rose')
  plot_ROC(model_rf_rose$model, test_data, train_data, file_path=file_path, jpgname='model_rf_rose')
  print( varImp(model_rf_rose$model))
  cm_rose <- model_rf_rose$cm
  models <- append(models, list(rose = model_rf_rose$model))
}

model_rf_smote <- make_model_rf(test_data, train_data, tr_n, tr_r, tr_numtree, method, par_sampling="smote")
if (exists("model_rf_smote")) {
  gainsSMOTE <- plot_gain(model_rf_smote$model, test_data, train_data, file_path=file_path, jpgname='model_smote')
  plot_ROC(model_rf_smote$model, test_data, train_data, file_path=file_path, jpgname='model_rf_smote')
  print( varImp(model_rf_smote$model))
  cm_smote <- model_rf_smote$cm
  models <- append(models, list(smote = model_rf_smote$model))
}



ctrl <- caret::trainControl(method = "repeatedcv",
                            number = tr_n,
                            repeats = tr_r,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            verboseIter = FALSE)
model_glm <- caret::train(classes~., data=train_data, method="glm",  preProc=c("center", "scale"),  trControl=ctrl, metric="ROC")

if (exists("model_glm")) {
  gainsGLM <- plot_gain(model_glm, test_data, train_data, file_path=file_path, jpgname='model_glm')
  plot_ROC(model_glm, test_data, train_data, file_path=file_path, jpgname='model_glm')
  
  final <- data.frame(actual = test_data$classes, predict(model_glm, newdata = test_data, type = "prob"))
  final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
  final$correct <- ifelse(final$actual == final$predict, TRUE, FALSE)

  cm_glm <- confusionMatrix(final$predict, test_data$classes)
  models <- append(models, list(glm = model_glm))
}

# 
# Plot all test gain curves
#
graphics.off()
par(fig=c(0,0.9,0, 0.9))
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate",
     xlab="Rate of Positive Predictions")

lines(x=gainsGLM$gaintestx, y=gainsGLM$gaintesty, col="green", lwd=2)
lines(x=gainsOVER$gaintestx, y=gainsOVER$gaintesty, col="blue", lwd=2)
lines(x=gainsUNDER$gaintestx, y=gainsUNDER$gaintesty, col="red", lwd=2)
lines(x=gainsSMOTE$gaintestx, y=gainsSMOTE$gaintesty, col="yellow", lwd=2)

legend('topright',
       legend=c('GLM', 'OVER', 'UNDER', 'SMOTE') ,
       lty=c(1,1,1,1), 
       bty='n', cex=0.75,
       col=c('green', 'blue', 'red', 'yellow'))

mypath <- file.path(file_path, paste("gain_", "all", ".jpg", sep = ""))
dev.copy(jpeg,filename=mypath)
dev.off()



#
# Plot Confidence intervals as resampling
#
resampling <- resamples(models)
graphics.off()
bwplot(resampling)
print( summary(resampling, metric = "ROC") )
dev.copy(jpeg,filename=file.path(file_path, paste("resample_", "all", ".jpg", sep = "")))
dev.off()

#
# Plot confusion matrix metrics for 0.5-threshold
#
comparison <- plot_results(models)
pic1=
  comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.3, alpha = 0.7, size = 3)

filename=file.path(file_path, paste("comparison_confusion_matrix_", "all", ".jpg", sep = ""))
ggsave(filename, pic1)
assign(filename, pic1, envir=.GlobalEnv)

#
# Plot COnficence Intervals (CI) of ROC as lower-ROC-upper bounds
#
test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$classes,
                 predict(model, data, type = "prob")[, "malignant"],
                 levels = c("benign", "malignant"))
  ci(roc_obj)
}

outside_test <- lapply(models, test_roc, data = test_data)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
cc = c("original", "under", "over", "smote", "glm", "rose")
outside_test = cbind(outside_test, model = row.names(outside_test))
outside_test <- as.data.frame(outside_test)

print(outside_test)

p1 =
  outside_test %>%
  gather(x, y, lower:upper) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.3, alpha = 0.7, size = 3)

filename=file.path(file_path, paste("ROC_test_CI_", "all", ".jpg", sep = ""))
ggsave(filename, p1)
assign(filename, p1, envir=.GlobalEnv)



# PLOT RESULTS

'
Found out how to do this! caret objects do in fact store the original model,
beneath a huge pile of metadata. You can access this model with
my_model_name$finalModel.

Thus, to find the confidence interval, you would call
predict(my_model_name$finalModel, my_data, interval = "confidence")
predict(train_model$finalModel, newdata=dframe, interval = "confidence",type=raw)
'

## Plot Lift and calibration
lift_results <- data.frame(Class = test_data$classes)
class1 = "benign"
lift_results$GEN <- predict(model_rf$model, newdata = test_data, type = "prob")[,class1]
lift_results$ROSE <- predict(model_rf_rose$model, newdata = test_data, type = "prob")[,class1]
lift_results$SMOTE <- predict(model_rf_smote$model, newdata = test_data, type = "prob")[,class1]
lift_results$UNDER <- predict(model_rf_under$model, newdata = test_data, type = "prob")[,class1]
lift_results$OVER <- predict(model_rf_over$model, newdata = test_data, type = "prob")[,class1]
lift_results$GLM <- predict(model_glm, newdata = test_data, type = "prob")[,class1]
print( head(lift_results,5) )

trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ GEN + SMOTE + GLM + UNDER + OVER, data = test_data) 
pic000 <- ggplot(lift_obj, values = 60)
# plot(lift_obj, values = 60, auto.key = list(columns = 3,lines = TRUE,points = FALSE))
filename=file.path(file_path, paste("lift_results_", "all", ".jpg", sep = ""))
ggsave(filename, pic000)



cal_obj <- calibration(Class ~ GEN + SMOTE + GLM + UNDER + OVER + ROSE,
                       data = lift_results,
                       cuts = 5) 
pic001 <- ggplot(cal_obj)
filename=file.path(file_path, paste("calibration_results_", "all", ".jpg", sep = ""))
ggsave(filename, pic001)

filename=file.path(file_path, paste("calibration_results_", "all_curves", ".jpg", sep = ""))
jpeg(file = filename)
plot(cal_obj, type = "l", auto.key = list(columns = 3,lines = TRUE, points = FALSE))
dev.off()


#
# save allthe models in my current workspace
#
mypath <- file.path(file_path, paste("saved_workspace", ".RData", sep = ""))
save(list = ls(all.names = TRUE), file = mypath, envir = .GlobalEnv)