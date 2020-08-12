######################################## TESTING #############################################################
### POPULATION COUNT TESTING AND TUNING
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(caret)
library(MLmetrics)
library(UBL)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("Input_Amenities_v1_wRE.csv", stringsAsFactors = FALSE)
file1 <- read.csv("Estim_Amenities_v1_wRE.csv", stringsAsFactors = FALSE)

##select the rows and remove NAs and block counts smaller than 2
data <- file[c(3:13,15)]
data <- na.omit(data)
data <- subset(data,data$BLK_Count>2)

##select the rows and remove NAs
est <- file1[c(3:13,15)]
est <- na.omit(est)
est <- subset(est,est$BLK_Count>2)

##create SMOTE dataset with Manhattan distance.
{
  data<-SmoteRegress(data$HDB_pop~.,data,dist = "Manhattan")
}

##add highly correlate variable pairs to the DFs
{
data$corr1 <- sqrt(as.numeric(apply(data[c(1,2)], 1, prod)))
data$corr2 <- sqrt(as.numeric(apply(data[c(8,11)], 1, prod)))
}

{
  est$corr1 <- sqrt(as.numeric(apply(est[c(1,2)], 1, prod)))
  est$corr2 <- sqrt(as.numeric(apply(est[c(8,11)], 1, prod)))
}


# set train control
control <- trainControl(method="repeatedcv", number=10, repeats=3)

######################################################################

##set seed
set.seed(676767)

##train the RF model
rf_fit <- train(HDB_pop ~ .,
                data = data, 
                method = "rf",
                ntree = 1000,
                preProcess = c("center","scale"),
                trControl=control)

##check the results
rf_fit

##set variable Importance and plot
rf_imp<-varImp(rf_fit, scale = FALSE)
plot(rf_imp, main="RF Variable Importance (with SMOTE)")

##validate results of the test set, and add the values to the DF
rf_pred_test <- predict(rf_fit, est)
results_rf<-as.data.frame(rf_pred_test)
est<-cbind(est,results_rf)

##calculate the measures for model estimation
postResample(pred = rf_pred_test, obs = est$HDB_pop)
cor(est$HDB_pop,rf_pred_test,method = c("pearson"))
MAPE(est$rf_pred_test,est$HDB_pop)

##aggregate the data to the total population, to meet the criteria of disaggregation
est$rf_pred_test_norm<-rf_pred_test/sum(rf_pred_test)*sum(est$HDB_pop)

##calculate the measures for the aggregated values
MAE(est$HDB_pop,est$rf_pred_test_norm)
MAPE(est$rf_pred_test_norm,est$HDB_pop)
cor(est$HDB_pop,est$rf_pred_test_norm, method = c("pearson"))

######################################################################

##set seed
set.seed(676767)

##train the SVM model
svm_fit <- train(HDB_pop ~ .,
                  data = data, 
                  method = "svmLinear3",
                  preProcess = c("center","scale"),
                  trControl=control)

##check the results
svm_fit

##set variable Importance and plot
svm_imp<-varImp(svm_fit, scale = FALSE)
plot(svm_imp, main="SVM Variable Importance (with SMOTE)")

##validate results of the test set, and add the values to the DF
svm_pred_test <- predict(svm_fit, est)
results_svm<-as.data.frame(svm_pred_test)
est<-cbind(est,results_svm)

##calculate the measures for validation
postResample(pred = svm_pred_test, obs = est$HDB_pop)
cor(est$HDB_pop,svm_pred_test,method = c("pearson"))
MAPE(est$svm_pred_test,est$HDB_pop)

##aggregate the data to the total population, to meet the criteria of disaggregation
est$svm_pred_test_norm<-svm_pred_test/sum(svm_pred_test)*sum(est$HDB_pop)

##calculate the measures for the aggregated values
MAE(est$HDB_pop,est$svm_pred_test_norm)
MAPE(est$svm_pred_test_norm,est$HDB_pop)
cor(est$HDB_pop,est$svm_pred_test_norm, method = c("pearson"))

######################################################################

##set seed
set.seed(676767)

##train the LM model
lm_fit <- train(HDB_pop ~ .,
                  data = data, 
                  method = "lm",
                  preProcess = c("center","scale"),
                  trControl=control)

##check the results
lm_fit$results

##set variable Importance and plot
lm_imp<-varImp(lm_fit, scale = FALSE)
plot(lm_imp, main="LM Variable Importance (with SMOTE)")

##validate results of the test set, and add the values to the DF
lm_pred_test <- predict(lm_fit, est)
results_LM<-as.data.frame(lm_pred_test)
est<-cbind(est,results_LM)

##aggregate the data to the total population, to meet the criteria of disaggregation
est$lm_pred_test_norm<-lm_pred_test/sum(lm_pred_test)*sum(est$HDB_pop)

##calculate the measures for validation
postResample(pred = lm_pred_test, obs = est$HDB_pop)
cor(est$HDB_pop,lm_pred_test,method = c("pearson"))
MAPE(est$lm_pred_test,est$HDB_pop)

##calculate the measures for the aggregated values
MAE(est$HDB_pop,est$lm_pred_test_norm)
MAPE(est$lm_pred_test_norm,est$HDB_pop)
cor(est$HDB_pop,est$lm_pred_test_norm,method = c("pearson"))


#############################################################

##check results with different trees
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000, 2500)) {
  set.seed(676767)
  fit <- train(HDB_pop ~ .,
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
dotplot(results, main="RF with different amount of trees (Population Counts)")
