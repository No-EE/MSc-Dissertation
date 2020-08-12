######################################## HDB TRANSACTIONS ################################
##### CALCULATE HDB TRANSACTIONS
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##import the libraries
library(tidyverse)


##set the working directory and check the files within
setwd("//../")
list.files()

##read the data 
data <- read.csv("HDB_Intersected_EST.csv", stringsAsFactors = FALSE)
hdb <- read.csv("resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv", stringsAsFactors = FALSE)
dummy <- read.csv("Dummy_EST.csv", stringsAsFactors = FALSE)

##create same addresses for both datasets
data <- data %>% unite("address", blk_no:street, sep = " ", remove = FALSE)
hdb <- hdb %>% unite("address", block:street_name, sep = " ", remove = FALSE)

##merge them in terms of the address
new<-merge(hdb, data, by.x='address', by.y='address')

##write CSV
#write.csv(new,"HDB_Subzone.csv")

####################################################

##create empty columns to be filled with fetched data
rowCount = vector("numeric", length = nrow(dummy))
avrgPrice = vector("numeric", length = nrow(dummy))

##define the input
input = paste0(dummy$SZ)

##count the rows
for (i in 1:nrow(dummy)) {
  
  rowCount[i]<-nrow(subset(new,new$SUBZONE_N == input[i]))
  
}

##add the fetched values to the DF
dummy$rowCount= rowCount

##define the input
input = paste0(dummy$SZ)

##calculate the average transcation prices
for (i in 1:nrow(dummy)) {
  
  avrgPrice[i]<-mean(subset(new$resale_price,new$SUBZONE_N == input[i]))
  
}

##add the fetched values to the DF
dummy$avrgPrice= avrgPrice

write.csv(dummy,"HDB_Transactions_EST.csv")





