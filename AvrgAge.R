######################################## AVERAGE AGE ################################
### Retrieve the average ages
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(simPop)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("average_age.csv", stringsAsFactors = FALSE)

##check the data type
sapply(file,class)

##create an empty list
datalist = list()

##define the input
input = paste0(file$SZ)

##create a function to loop over the rows
for (i in 1:nrow(file)) {

  ## call the "sprague" function, which estaimes counts for each age
  sprague <- sprague(subset(file[3:19],file$SZ==input[i]))

  ## create a DF and add a column title, remove the last age group which is 80+
  df<-do.call(rbind.data.frame, sprague[-81])
  names(df)[1] <- "counts"
  
  ## add the age values from the last three age groups (80-84,85-89,90+)
  df[nrow(df) + 1,] = list(file$ageGr18[file$SZ==input[i]])
  df[nrow(df) + 1,] = list(file$ageGr19[file$SZ==input[i]])
  df[nrow(df) + 1,] = list(file$ageGr20[file$SZ==input[i]])

  ## add a second column with consecutive numbers =age starting from 0
  df$Number<-(0:82)
  
  ##replace the values for the last 2 groups
  df$Number[df$Number == 81] <- 85
  df$Number[df$Number == 82] <- 90
  
  ## calculate the weighted mean
  wm <- weighted.mean(df$Number,df$counts)

  ##append to the list
  datalist[[i]] <- wm

}

## convert the list to a full dataframe
avrgAge = do.call(rbind, datalist)

## export CSV
write.csv(avrgAge,"average_age_RA.csv")

