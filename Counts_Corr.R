######################################## Correlation Matrix ###################################################
### Correlation Matrix of the features
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

library(corrplot)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("Input_Amenities_v1_wRE.csv", stringsAsFactors = FALSE)

##select the rows and remove NAs
data <- file[c(3:13)]
data <- na.omit(data)

##define the correlation coefficients
cor <- cor(data, method = "pearson")

##visualise the results
corrplot(cor, method = "square", type = "upper" , order = "hclust", addCoef.col = "dark red",
         tl.col = "black", tl.srt = 45)

