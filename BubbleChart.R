######################################## Plotting Population ################################
### Bubble Plot
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

# Libraries
library(ggplot2)
library(tidyverse)


##set the working directory and check the files within
setwd("//../")
list.files()

##read the file and create a subset
file <- read.csv("Input_Bubble.csv", stringsAsFactors = FALSE)
data <- subset(file,file$HDB_pop>0)

# create the bubble plot
data %>%
  arrange(desc(HDB_pop)) %>%
  ggplot(aes(x=PA, y=HDB_pop, size = HDB_pop, colour= PA)) +
  geom_point(alpha=0.4, stroke = 1) +
  scale_size(range = c(2, 30), name="Population") +
  theme_minimal()+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
  labs(title="Subzone Population Counts",
       x ="Planning Area", y = "HDB Population Counts")
