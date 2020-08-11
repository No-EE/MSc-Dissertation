# MSc-Dissertation

This repository contains the major codes that have been developed for the MSc Dissertation "Enhanced Population Estimation beyond Counts: Modelling Age Patterns", The University of Edinburgh, 2020. 

The following packages have been used in R:

- caret
- caTools
- ggplot2
- ggpubr
- googleway
- jsonlite
- MLmetrics
- simPop
- tidyverse
- UBL

## Census Data Processing

_AvrgAge.R_

A code to calculate the average ages for administrative areas by implementing the sprague interpolation method.

_CensusVisualisation.R_

Creates several barplots to visualise the census dataset in terms of age, gender, property type and flat type.

_BubbleChart.R_

A code to visualise the population counts of the areas by a bubble plot.

_Internal_Analysis.R_

Draws a scatterplot matrix of the demographic factors within the census dataset.


## Geocoding
### Google Geocode API

_HDB_Geocode.R_

A loop to fetch the coordinates for all HDB properties in Singapore by Google Geocode API.

_School_Geocode.R_

A loop to fetch the coordinates for schools in Singapore by Google Geocode API.

### Google Places API

_Childcare_Places.R_

A loop to fetch childcare amenities without input dataset by Google Places API. This code can be used for other amenities by adjusting the script.

### OSM API

_HDB_OSM.R_

A loop to fetch the coordinates for all HDB properties in Singapore by OSM API.

_School_OSM.R_

A loop to fetch the coordinates for schools in Singapore by OSM API.

### HDB Processing

_HDB_Age.R_

A code to calculate the average age (mean, median and mode) for HDB properties in Subzones.

_HDB_Transactions.R_

Calculates the transaction counts and mean transaction values for Subzones.

_HDB_Type.R_

Calculates the flat type proportions for each Subzone.

## Models
### Testing

_Counts_Estim.R_

Trains RF, SVM and LM models to estimate population counts including feature combination (optional) and sample balancing (optional).

_Counts_Corr.R_
