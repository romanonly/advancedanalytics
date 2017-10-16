# Random Forest Confidence Intervals

#install.packages("devtools")
library(devtools) 
install_github("swager/randomForestCI")
library(randomForestCI)
library(dplyr) # For data manipulation
library(randomForest) # For random forest ensemble models
library(ggplot2)


library(randomForestCI)
# Make some data...
n = 250
p = 100
X = matrix(rnorm(n * p), n, p)
Y = rnorm(n)

#  Run the method
rf = randomForest(X, Y, keep.inbag = TRUE)
ij = randomForestInfJack(rf, X, calibrate = TRUE)

plot(ij)


# Fetch data from the UCI MAchine Learning Repository
url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/
auto-mpg/auto-mpg.data"
mpg <- read.table(url,stringsAsFactors = FALSE,na.strings="?")
# https://archive.ics.uci.edu/ml/machine-learning-databases/
auto-mpg/auto-mpg.names
names(mpg) <- c("mpg","cyl","disp","hp","weight","accel","year","origin","name")
head(mpg)

# Look at the data and reset some of the data types
dim(mpg); Summary(mpg)
sapply(mpg,class)
mpg <- mutate(mpg, hp = as.numeric(hp),
              year = as.factor(year),
              origin = as.factor(origin))
head(mpg,2)
#
# Function to divide data into training, and test sets 
index <- function(data=data,pctTrain=0.7)
{
  # fcn to create indices to divide data into random 
  # training, validation and testing data sets
  N <- nrow(data) 
  train <- sample(N, pctTrain*N) 
  test <- setdiff(seq_len(N),train) 
  Ind <- list(train=train,test=test)
  return(Ind)
} 
#
set.seed(123)
ind <- index(mpg,0.8)
length(ind$train); length(ind$test)

form <- formula("mpg ~ cyl + disp + hp + weight + 
                accel + year + origin")

rf_fit <- randomForest(formula=form,data=na.omit(mpg[ind$train,]),
                       keep.inbag=TRUE) # Build the model

# Plot the error as the number of trees increases
plot(rf_fit)

# Plot the important variables
varImpPlot(rf_fit,col="blue",pch= 2)

# Calculate the Variance
X <- na.omit(mpg[ind$test,-1])
var_hat <- randomForestInfJack(rf_fit, X, calibrate = TRUE)

#Have a look at the variance
head(var_hat); dim(var_hat); plot(var_hat)

# Plot the fit
df <- data.frame(y = mpg[ind$test,]$mpg, var_hat)
df <- mutate(df, se = sqrt(var.hat))
head(df)

p1 <- ggplot(df, aes(x = y, y = y.hat))
p1 + geom_errorbar(aes(ymin=y.hat-se, ymax=y.hat+se), width=.1) +
  geom_point() + 
  geom_abline(intercept=0, slope=1, linetype=2) +
  xlab("Reported MPG") +
  ylab("Predicted MPG") +
  ggtitle("Error Bars for Random Forests")



RF_CI