######################################## OSM API ################################
##### FETCH HDB COORDINATES
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##load the necessary libraries
library(tidyverse)  
library(jsonlite)   

##set the working directory and check the files within
setwd("//../")
list.files()

##read the data 
data <- read.csv("hdb-property-information.csv", stringsAsFactors = FALSE) %>%
  mutate(lon = 0,
         lat = 0)
nrow(data)

##keep only the buildings which are residential
data <- subset(data, data$residential=="Y")

nrow(data)

##define the function
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

###run the loop to fetch the coordinates
for (i in 1:nrow(data)) { 
  print(i)
  long_lat <- nominatim_osm(paste0(data$blk_no[i], " ", data$street[i], ", SINGAPORE"))
  Sys.sleep(1)  # ensure 1.5 second between API call as per OSM guidelines
  if (dim(long_lat)[1] != 0) {
    data$lon[i] = long_lat$lon
    data$lat[i] = long_lat$lat
  }
}

write.csv(data,"HDB_fetched.csv")

