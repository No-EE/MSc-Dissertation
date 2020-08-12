######################################## FINAL RUNS ##########################################################
### AVERAGE AGE
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(caTools)
library(caret)
library(MLmetrics)

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

# set train control
control <- trainControl(method="repeatedcv", number=10, repeats=3)

##create empty datalists
{
datalist = list()
datalist1 = list()
datalist2 = list()
}

######################################################################


for (i in 1:100) {
  print(i)
  ##train the SVM model (with feature combination)
  svm_fit <- train(avrg ~ .,
                  data = data, 
                  method = "svmLinear3",
                  preProcess = c("center","scale"),
                  trControl=control)
  
  ##validate results of the test set
  svm_pred_test <- predict(svm_fit, est)
  
  varImp(svm_fit)
  
  ##calculate the measures for validation
  ERRS<-postResample(pred = svm_pred_test, obs = est$avrg)
  MAPE<-MAPE(svm_pred_test,est$avrg)
  cor<-cor(est$avrg,svm_pred_test, method = c("pearson"))
  
  ##combine the measures
  measures<-cbind(MAPE,cor)
  
  ##add the rows/columns to the datalist
  datalist[[i]] <- measures
  datalist1[[i]] <- svm_pred_test
  datalist2[[i]] <- ERRS
  
}

##combine the count values and add them to the estimation DF and calculate means
{
  counts_svm <- do.call(cbind, datalist1)
  counts_svm <-as.data.frame(counts_svm)
  est$estimations_svm<-(rowMeans(counts_svm))
}

##combine the MAPE and correlation coefficients and calculate means
measure_svm <- do.call(rbind, datalist)
measure_svm <-as.data.frame(measure_svm)
mean(measure_svm$MAPE)
mean(measure_svm$cor)

##combine the error measures and calculate means
errs_svm <- do.call(rbind, datalist2)
errs_svm <-as.data.frame(errs_svm)
mean(errs_svm$RMSE)
mean(errs_svm$MAE)
mean(errs_svm$Rsquared)


######################################################################

##train the LM model (without feature combination)
lm_fit <- train(avrg ~ .,
                   data = data, 
                   method = "lm",
                   preProcess = c("center","scale"),
                   trControl=control)

##validate results of the test set
lm_pred_test <- predict(lm_fit, est)
  
##calculate the measures for validation
postResample(pred = lm_pred_test, obs = est$avrg)
MAPE<-MAPE(lm_pred_test,est$avrg)
cor<-cor(est$avrg,lm_pred_test, method = c("pearson"))

##add the estimations to the DF
est$estimations_lm <- lm_pred_test

######################################################################

## merge to retrive the SZ and write the final estimations
total <- merge(est,file1,by="avrg")
write.csv(total,"final_avrg_estimationLM.csv")
