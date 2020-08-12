######################################## GOOGLE GEOCODE API ################################
##### FETCH HDB COORDINATES
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##load the necessary libraries
library(googleway)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the data 
data <- read.csv("hdb-property-information.csv", stringsAsFactors = FALSE)
nrow(data)

###############################################################################

##keep only the buildings which are residential
data <- subset(data, data$residential=="Y")

##reduce data to planning area of interest
data <- subset(data, data$bldg_contract_town=="YS")

##input address
address = paste0(data$street, " ", data$blk_no, ", SINGAPORE")

##create empty columns to be filled with fetched data
lat = vector("numeric", length = nrow(data))
lng = vector("numeric", length = nrow(data))

#### CAREFUL!! LOOP AHEAD, BE AWARE OF YOUR QUOTA
for (i in 1:nrow(data)) {
  print(i)
  coord = google_geocode(address[i], key = "AIzaSyD9Fsn2Mt62OPMVxrMkhM2fJsaJKCqfmuo")
  
  
  if (coord$status == "OK") {
    coord = geocode_coordinates(coord)
    lat[i] = coord$lat[1] ##only return one coordinate if multiple
    lng[i] = coord$lng[1] ##only return one coordinate if multiple
    ##fill columns with "NA" is not result
  } else {
    lat[i] = NA
    lng[i] = NA
  }
}

##add the values to the DF
data$lat = lat
data$lng = lng


##write CSV
write.csv(data,"Yishan_HDB.csv")

###############################################################################

