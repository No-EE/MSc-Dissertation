######################################## FINAL RUNS ##########################################################
### POPULATION COUNTS
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(caret)
library(MLmetrics)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("Input_Amenities_v1_wRE.csv", stringsAsFactors = FALSE)
file1 <- read.csv("Estim_Amenities_v1_wRE.csv", stringsAsFactors = FALSE)

##select the rows and remove NAs
data <- file[c(3:13,15)]
data <- na.omit(data)
data <- subset(data,data$BLK_Count>2)

##select the rows and remove NAs
est <- file1[c(3:13,15)]
est <- na.omit(est)
est <- subset(est,est$BLK_Count>2)

##add highly correlated variable pairs to the DFs
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

##create empty datalists
datalist = list()
datalist1 = list()

######################################################################


for (i in 1:100) {
  print(i)
##train the SVM model (with feature combination)
svm_fit <- train(HDB_pop ~ .,
                  data = data, 
                  method = "svmLinear3",
                  preProcess = c("center","scale"),
                  trControl=control)

##validate results of the test set
svm_pred_test <- predict(svm_fit, est)

## print the var importance
varImp(svm_fit)

##normalise to aggregated values
pop_count_svm<-svm_pred_test/sum(svm_pred_test)*sum(est$HDB_pop)

##calculate the error measures
RMSE<-RMSE(est$HDB_pop,pop_count_svm)
MAE<-MAE(est$HDB_pop,pop_count_svm)
MAPE<-MAPE(pop_count_svm,est$HDB_pop)
cor<-cor(est$HDB_pop,pop_count_svm, method = c("pearson"))

##combine the measures
measures<-cbind(RMSE,MAE,MAPE,cor)

##add the rows/columns to the datalist
datalist[[i]] <- measures
datalist1[[i]] <- pop_count_svm

}

##combine the count values and add them to the estimation DF
{
counts_svm <- do.call(cbind, datalist1)
counts_svm <-as.data.frame(counts_svm)
est$estimations_svm<-(rowMeans(counts_svm))
}

##combine the rows and remove MAPE outliers
measure_svm <- do.call(rbind, datalist)
measure_svm <-as.data.frame(measure_svm)
measure_svm <- subset(measure_svm,measure_svm$MAPE<0.5)

##calculate the measures
mean(measure_svm$RMSE)
mean(measure_svm$MAE)
mean(measure_svm$MAPE)
mean(measure_svm$cor)


######################################################################

##train the SVM model (with feature combination)
lm_fit <- train(HDB_pop ~ .,
                   data = data, 
                   method = "lm",
                   preProcess = c("center","scale"),
                   trControl=control)
  
##validate results of the test set
lm_pred_test <- predict(lm_fit, est)

varImp(lm_fit)
  
##normalise to aggregated values
pop_count_lm<-lm_pred_test/sum(lm_pred_test)*sum(est$HDB_pop)
  
##calculate the error measures
RMSE(est$HDB_pop,pop_count_lm)
MAE(est$HDB_pop,pop_count_lm)
MAPE(pop_count_lm,est$HDB_pop)
cor(est$HDB_pop,pop_count_lm, method = c("pearson"))
  
##add the estimations to the DF
est$estimations_lm <- pop_count_lm

######################################################################

## merge to retrive the SZ and write the final estimations
total <- merge(est,file1,by="HDB_pop")
write.csv(total,"final_pop_estimationSVM.csv")




