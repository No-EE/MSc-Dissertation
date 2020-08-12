######################################## Correlation Matrix ###################################################
### Correlation Matrix of the features
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

library(corrplot)

##set the working directory and check the files within
setwd("//Users/Noee/Documents/UoE/MSC_RESEARCH - FINAL_FINAL/7_Models/4_AvrgAge_Final")
list.files()

##read the file and create a subset
file <- read.csv("Input_v1.csv", stringsAsFactors = FALSE)

##exclude rows with "NA" age --> no population
data <- subset(file[c(3:13)])
data <- na.omit(data)
data <- subset(data,data$BLK_Count>2)
data<-data[c(-9,-10,-11)]

##define the correlation coefficients
cor <- cor(data, method = "pearson")

##visualise the results
corrplot(cor, method = "square", type = "upper" , order = "hclust", addCoef.col = "dark red",
         tl.col = "black", tl.srt = 45)

