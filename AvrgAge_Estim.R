######################################## TESTING #############################################################
### AVERAGE AGE TESTING AND TUNING
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(caTools)
library(caret)
library(MLmetrics)
library(smooth)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("Input_v1.csv", stringsAsFactors = FALSE)
file1 <- read.csv("Input_v1_EST.csv", stringsAsFactors = FALSE)

##exclude rows with "NA" age --> no population
data <- subset(file[c(3:13)])
data <- na.omit(data)
data <- subset(data,data$BLK_Count>2)
data<-data[c(-9,-10)]

##exclude rows with "NA" age --> no population
est <- subset(file1[c(3:13)])
est <- na.omit(est)
est <- subset(est,est$BLK_Count>2)
est<-est[c(-9,-10)]

##add highly correlate variable pairs to the DFs
{
  data$corr1 <- sqrt(as.numeric(apply(data[c(1,2)], 1, prod)))
  data$corr2 <- sqrt(as.numeric(apply(data[c(3,6)], 1, prod)))
}

{
  est$corr1 <- sqrt(as.numeric(apply(est[c(1,2)], 1, prod)))
  est$corr2 <- sqrt(as.numeric(apply(est[c(3,6)], 1, prod)))
}

##summary
summary(data)

##check data type
sapply(data, class)


# set train control
control <- trainControl(method="repeatedcv", number=10, repeats=3)

######################################################################

##set seed
set.seed(676767)

##train the RF model
rf_fit <- train(avrg ~ .,
                data = data, 
                method = "rf",
                ntree = 1000,
                preProcess = c("center","scale"),
                trControl=control)
                

##check the results
rf_fit

##set variable Importance and plot
rf_imp<-varImp(rf_fit, scale = FALSE)
plot(rf_imp, main="RF Variable Importance (with FC")

##validate results of the test set
rf_pred_test <- predict(rf_fit, est)
results_rf<-as.data.frame(rf_pred_test)
est<-cbind(est,results_rf)

##calculate the measures for validation
postResample(pred = rf_pred_test, obs = est$avrg)
MAPE(est$rf_pred_test,est$avrg)
cor(est$avrg,est$rf_pred_test, method=c("pearson"))

######################################################################

##set seed
set.seed(676767)

##train the SVM model
svm_fit <- train(avrg ~ .,
                 data = data, 
                 method = "svmLinear3",
                 preProcess = c("center","scale"),
                 trControl=control)

##check the results
svm_fit

##set variable Importance and plot
svm_imp<-varImp(svm_fit, scale = FALSE)
plot(svm_imp, main="SVM Variable Importance (with FC)")

##validate results of the test set
svm_pred_test <- predict(svm_fit, est)
results_SVM<-as.data.frame(svm_pred_test)
est<-cbind(est,results_SVM)

##calculate the measures for validation
postResample(pred = svm_pred_test, obs = est$avrg)
MAPE(est$svm_pred_test,est$avrg)
cor(est$avrg,est$svm_pred_test, method=c("pearson"))
######################################################################

##set seed
set.seed(676767)

##train the LM model
lm_fit <- train(avrg ~ .,
                data = data, 
                method = "lm",
                preProcess = c("center","scale"),
                trControl=control)

##check the results
lm_fit$results

##set variable Importance and plot
lm_imp<-varImp(lm_fit, scale = FALSE)
plot(lm_imp, main="LM Variable Importance (with FC)")

##validate results of the test set
lm_pred_test <- predict(lm_fit, est)
results_LM<-as.data.frame(lm_pred_test)
est<-cbind(est,results_LM)

##calculate the measures for validation
postResample(pred = lm_pred_test, obs = est$avrg)
MAPE(est$lm_pred_test,est$avrg)
cor(est$avrg,est$lm_pred_test, method=c("pearson"))

#############################################################

##check results with different trees
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000, 2500)) {
  set.seed(676767)
  fit <- train(avrg ~ .,
               data = data, 
               method = "rf",
               preProcess = c("center","scale"),
               trControl=control,
               ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results, main="RF with different amount of trees (Avrg Age)")
