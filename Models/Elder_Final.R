######################################## FINAL RUNS ##########################################################
### ELDERLY 
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

##create an empty datalist
datalist = list()
datalist1 = list()
datalist2 = list()

######################################################################


for (i in 1:100) {
  print(i)
  ##train the SVM model (with feature combination)
  rf_fit <- train(elder ~ .,
                   data = data, 
                   method = "rf",
                   trees = 1000,
                   preProcess = c("center","scale"),
                   trControl=control)
  
  ##validate results of the test set
  rf_pred_test <- predict(rf_fit, est)
  
  varImp(rf_fit)
  
  ##calculate the measures for validation
  ERRS<-postResample(pred = rf_pred_test, obs = est$elder)
  MAPE<-MAPE(rf_pred_test,est$elder)
  cor<-cor(est$elder,rf_pred_test, method = c("pearson"))
  
  ##combine the measures
  measures<-cbind(MAPE,cor)
  
  ##add the rows/columns to the datalist
  datalist[[i]] <- measures
  datalist1[[i]] <- rf_pred_test
  datalist2[[i]] <- ERRS
  
}

##combine the count values and add them to the estimation DF
{
  counts_rf <- do.call(cbind, datalist1)
  counts_rf <-as.data.frame(counts_rf)
  est$estimations_rf<-(rowMeans(counts_rf))
}

##combine the rows and remove MAPE outliers
measure_rf <- do.call(rbind, datalist)
measure_rf <-as.data.frame(measure_rf)
measure_rf <- subset(measure_rf,measure_rf$MAPE<0.5)
mean(measure_rf$MAPE)
mean(measure_rf$cor)

##
errs_rf <- do.call(rbind, datalist2)
errs_rf <-as.data.frame(errs_rf)
mean(errs_rf$RMSE)
mean(errs_rf$MAE)
mean(errs_rf$Rsquared)


######################################################################

##train the LM model (without feature combination)
lm_fit <- train(elder ~ .,
                data = data, 
                method = "lm",
                preProcess = c("center","scale"),
                trControl=control)

##validate results of the test set
lm_pred_test <- predict(lm_fit, est)

varImp(lm_fit)

##calculate the measures for validation
postResample(pred = lm_pred_test, obs = est$elder)
MAPE<-MAPE(lm_pred_test,est$elder)
cor<-cor(est$elder,lm_pred_test, method = c("pearson"))

##add the estimations to the DF
est$estimations_lm <- lm_pred_test

######################################################################

## merge to retrive the SZ and write the final estimations
total <- merge(est,file1,by="elder")
write.csv(total,"final_elder_estimationRF.csv")

