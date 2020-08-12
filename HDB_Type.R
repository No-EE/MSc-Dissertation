######################################## CALCULATE THE ROOM TYPES ################################
### HDB room proportions
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())


##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("HDB_Intersected_EST.csv", stringsAsFactors = FALSE)
dummy <- read.csv("Dummy_EST.csv", stringsAsFactors = FALSE)

##create empty columns to be filled with fetched data
  {
oneRatio = vector("numeric", length = nrow(dummy))
twoRatio = vector("numeric", length = nrow(dummy))
threeRatio = vector("numeric", length = nrow(dummy))
fourRatio = vector("numeric", length = nrow(dummy))
fiveRatio = vector("numeric", length = nrow(dummy))
execRatio = vector("numeric", length = nrow(dummy))
multiRatio = vector("numeric", length = nrow(dummy))
totalRatio = vector("numeric", length = nrow(dummy))
}
##transformt the data to make it useful
  {
file$X1room_sold<-as.numeric(file$X1room_sold)
file$X2room_sold<-as.numeric(file$X2room_sold)
file$X3room_sold<-as.numeric(file$X3room_sold)
file$X4room_sold<-as.numeric(file$X4room_sold)
file$X5room_sold<-as.numeric(file$X1room_sold)
file$exec_sold<-as.numeric(file$exec_sold)
file$multigen_sold<-as.numeric(file$multigen_sold)
file$studio_apartment_sold<-as.numeric(file$studio_apartment_sold)
file$X1room_rental<-as.numeric(file$X1room_rental)
file$X2room_rental<-as.numeric(file$X2room_rental)
file$X3room_rental<-as.numeric(file$X3room_rental)
}
##check if we are good to go
sapply(file,class)

##define the input
input = paste0(dummy$SZ)


for (i in 1:nrow(dummy)) {

##calculate the sums
oneRoom <- sum(file$X1room_sold[file$SUBZONE_N==input[i]]+file$X1room_rent[file$SUBZONE_N==input[i]]
               +file$studio_apartment_sold[file$SUBZONE_N==input[i]])
twoRoom <- sum(file$X2room_sold[file$SUBZONE_N==input[i]]+file$X2room_rental[file$SUBZONE_N==input[i]])
threeRoom <- sum(file$X3room_sold[file$SUBZONE_N==input[i]]+file$X3room_rental[file$SUBZONE_N==input[i]])
fourRoom <- sum(file$X4room_sold[file$SUBZONE_N==input[i]])
fiveRoom <- sum(file$X5room_sold[file$SUBZONE_N==input[i]])
execRoom <- sum(file$exec_sold[file$SUBZONE_N==input[i]])
multiRoom <- sum(file$multigen_sold[file$SUBZONE_N==input[i]])

##get the total and calculate ratios
Total <- oneRoom+twoRoom+threeRoom+fourRoom+fiveRoom+execRoom+multiRoom

oneRatio[i] <- oneRoom/Total
twoRatio[i] <- twoRoom/Total
threeRatio[i] <- threeRoom/Total
fourRatio[i] <- fourRoom/Total
fiveRatio[i] <- fiveRoom/Total
execRatio[i] <- execRoom/Total
multiRatio[i] <- multiRoom/Total

##check if ratio is good (==1)
totalRatio[i] <- oneRatio+twoRatio+threeRatio+fourRatio+fiveRatio+execRatio+multiRatio

}

##add the fetched values to the DF
{
dummy$oneRatio = oneRatio
dummy$twoRatio = twoRatio
dummy$threeRatio = threeRatio
dummy$fourRatio = fourRatio
dummy$fiveRatio = fiveRatio
dummy$execRatio = execRatio
dummy$multiRatio = multiRatio
dummy$totalRatio = totalRatio
}

write.csv(dummy,"flat_type_EST.csv")

