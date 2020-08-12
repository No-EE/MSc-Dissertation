######################################## CALCULATE HDB AGE ################################
##### HDB AGES
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())


##set the working directory and check the files within
setwd("//../")
list.files()

##read the data 
data <- read.csv("HDB_Intersected_RA.csv", stringsAsFactors = FALSE)
dummy <- read.csv("Dummy_RA.csv", stringsAsFactors = FALSE)

##create empty columns to be filled with fetched data
mean = vector("numeric", length = nrow(dummy))
median = vector("numeric", length = nrow(dummy))
mode = vector("numeric", length = nrow(dummy))

##create a function to calculate the mode, since there is no built in function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##define the input
input = paste0(dummy$SZ)

##calculate all values for each SZ
for (i in 1:nrow(dummy)) {

mean[i]<-mean(subset(2020-data$year_completed, data$SUBZONE_N==input[i]))
median[i]<-median(subset(2020-data$year_completed, data$SUBZONE_N==input[i]))
mode[i]<-getmode(subset(2020-data$year_completed, data$SUBZONE_N==input[i]))
}

##add the fetched values to the DF
dummy$mean = mean
dummy$median = median
dummy$mode = mode

##write CSV
write.csv(dummy,"HDB_ages.csv")





