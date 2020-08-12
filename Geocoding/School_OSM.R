######################################## OSM API ################################
##### FETCH SCHOOL COORDINATES
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##load the necessary libraries
library(tidyverse)   # data manipulation
library(jsonlite)    # JSON

##set the working directory and check the files within
setwd("//../")
list.files()

##read the data 
data <- read.csv("general-information-of-schools.csv", stringsAsFactors = FALSE) %>%
  mutate(lon = 0,
         lat = 0)
nrow(data)

### FUNCTIONS ###
#################

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'https://nominatim.openstreetmap.org/search/@addr@?format=json&amp;addressdetails=0&amp;limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

### Add Geocoding ###
for (i in 1:nrow(data)) {
  print(i)
  long_lat <- nominatim_osm(paste0(data$address[i],", Singapore"))
  Sys.sleep(1)  # ensure 1 second between API call as per OSM guidelines
  if (dim(long_lat)[1] != 0) {
    data$lon[i] = long_lat$lon
    data$lat[i] = long_lat$lat
  }
}

write.csv(data,"schools_fetched.csv")

