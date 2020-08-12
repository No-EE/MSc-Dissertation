######################################## GOOGLE GEOCODE API ################################
##### FETCH SCHOOL COORDINATES
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##load the necessary libraries
library(googleway)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the data 
data <- read.csv("general-information-of-schools.csv", stringsAsFactors = FALSE)
nrow(data)

###############################################################################

##input address
address = paste0(data$address, ", SINGAPORE")

##create empty columns to be filled with fetched data
lat = vector("numeric", length = nrow(data))
lng = vector("numeric", length = nrow(data))

#### CAREFUL!! LOOP AHEAD, BE AWARE OF YOUR QUOTA
for (i in 1:nrow(data)) {
  print(i)
  coord = google_geocode(address[i], key = "YourApiKey")
  
  
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
write.csv(data,"Schools.csv")

