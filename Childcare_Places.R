######################################## GOOGLE PLACES API ################################
##### FETCH CHILDCARE FACILITIES
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

library(googleway)
library(tidyverse)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the data containing the administrative areas
data <- read.csv("Amenities_Input.csv", stringsAsFactors = FALSE)
nrow(data)
                 
###############################################################################

##set the api key and the mutable inputs
api_key <- "YourApiKey"
input = paste0("childcare ", data$SZ)
location = c(1.352100,103.819800)#Singapore
radius = 35000#35 km
filter = "Preschool|preschool|Children|children|Kindergarden|kindergarden|
                                 |MOE|Pre-School|pre-school|PCF|Pcf|Kidz|kidz|Playgroup|playgroup|Cambridge|
                                 |MY World|Childcare|childcare"
##create an empty list
datalist = list()

##LOOP TIME, be careful of your API quota!!
for (i in 1:nrow(data)) {
  ##pause for 1.5 secs to avoid API key protection issue
  Sys.sleep(1.5)
  
  ##get the first 20 results of the API data, which contains multiple DF within each other
  res <- google_places(location = location,
                       search_string = input[i],
                       radius = radius,
                     key = api_key)
  
  ##if there are no results, go to next iteration
  if(is_empty(res$results)) {
    next
  }
  
  ##assign the coordinate and result DF to variables
  coord1 <- do.call("data.frame", res$results$geometry$location)
  info1 <- data_frame(res$results[ , c("id", "formatted_address", "name")])
  ##bind the DFs and run the filter for better results
  df1 <- cbind(info1,coord1)
  df1 <- df1 %>% filter(str_detect(name, filter))
  
  if(is_empty(df1$name)) {
    next
  }

  ##pause for 1.5 secs to avoid API key protection issue
  Sys.sleep(1.5)

  ##use the 'next_page_token' from the previous search to get the next 20 results
  res_next <- google_places(location = location,
                            search_string = input[i],
                            radius = radius,
                          page_token = res$next_page_token,
                          key = api_key)
  
  ##if there are no results, take the results from page 1 only
  if(is_empty(res_next$results)) {
    df1$subzone<-data$SZ[i]
    datalist[[i]] <- df1
    next
   }

  coord2 <- do.call("data.frame", res_next$results$geometry$location)
  info2 <- data_frame(res_next$results[ , c("id", "formatted_address", "name")])
  
  df2 <- cbind(info2,coord2)
  df2 <- df2 %>% filter(str_detect(name, filter))
  
  Sys.sleep(1.5)

  ##use the 'next_page_token' from the previous search to get the next 20 results
  res_next1 <- google_places(location = location,
                             search_string = input[i],
                             radius = radius,
                          page_token = res_next$next_page_token,
                          key = api_key)
  
  ##if there are no results, take the results from page 1 and 2 only
  if(is_empty(res_next1$results)) {
    file=rbind(df1,df2)
    file$subzone<-data$SZ[i]
    datalist[[i]] <- file
    next
  }
  

  coord3 <- do.call("data.frame", res_next1$results$geometry$location)
  info3 <- data_frame(res_next1$results[ , c("id", "formatted_address", "name")])

  df3 <- cbind(info3,coord3)
  df3 <- df3 %>% filter(str_detect(name, filter))

  file <- rbind(df1,df2,df3)
  file$subzone<-data$SZ[i]
  
  datalist[[i]] <- file

}

##combine all results to a DF and remove duplicates
big_data = do.call(rbind, datalist)
big_data <- big_data[ -c(1,6) ]
big_data <- big_data[!duplicated(big_data$name), ]


##add another line if second search with different input
big_data1 <- do.call(rbind, datalist)
big_data1 <- big_data1[ -c(1,6) ]
big_data1 <- big_data1[!duplicated(big_data$name), ]

##add another line if second search with different input
big_data2 <- do.call(rbind, datalist)
big_data2 <- big_data2[ -c(1,6) ]
big_data2 <- big_data2[!duplicated(big_data$name), ]

##add another line if second search with different input
big_data3 <- do.call(rbind, datalist)
big_data3 <- big_data3[ -c(1,6) ]
big_data3 <- big_data3[!duplicated(big_data$name), ]



combine <- rbind(big_data,big_data1,big_data2,big_data3)
total <- combine[!duplicated(combine$name), ]




###############################################################################

write.csv(total,"childcare.csv")


