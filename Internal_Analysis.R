######################################## INTERNAL REGRESSION ANALYSIS ################################
### Regression Analysis
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(ggplot2)
library(GGally)


##set the working directory and check the files within
setwd("//../")
list.files()

##load the datasets and transform data

################################################################################

PASZ <- read.csv("v1_Internal_Analysis.csv", stringsAsFactors = FALSE)
PASZ <- subset(PASZ,PASZ$HDB>0.5)

################################################################################

##proper scatterplot

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

##define the plot and add title and caption

g = ggpairs(subset(PASZ[3:16]), lower = list(continuous = my_fn))
g +
  labs(title="\nScatterplot of Internal Factors",
       caption="Noée Szarka | UoE, School of GeoSciences | NUS, Urban Analytics Lab (ual.sg)") 

################################################################################

#print out the correlations
mosthighlycorrelated <- function(PASZ,numtoreport)
{
  # find the correlations
  cormatrix <- cor(PASZ)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

mosthighlycorrelated(PASZ[3:16], 15)


