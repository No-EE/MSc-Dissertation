######################################## VISUALISING THE RESULTS #############################################
### TREEMAPS
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(treemap)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the files
pop <- read.csv("final_pop.csv", stringsAsFactors = FALSE)
avrg <- read.csv("final_avrg.csv", stringsAsFactors = FALSE)
elder <- read.csv("final_elder.csv", stringsAsFactors = FALSE)

##define the means for the colour scales
mean(pop$HDB_pop)
mean(avrg$avrg)
mean(elder$elder)

########################################################################

### population count
##treemap for observed
treemap(pop,
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        sortID = "HDB_pop",
        vSize = "area", 
        vColor = "HDB_pop",
        type="value", 
        palette = "Blues", 
        mapping = c(min(0), mean(range(21113)), max(130000)),
        title="Population Counts (Observed)", 
        title.legend="Population Count",
        fontsize.title = 14 
)

##treemap for LM
treemap(pop, 
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        sortID = "HDB_pop",
        vSize = "area",  
        vColor = "estimations_lm",
        type="value", 
        palette = "Blues",  
        mapping = c(min(0), mean(range(21113)), max(130000)),
        title="Population Counts (LM Predictions)", 
        title.legend="Population Count",
        fontsize.title = 14 
)

##treemap for SVM
treemap(pop, 
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        sortID = "HDB_pop",
        vSize = "area",  
        vColor = "estimations_svm",
        type="value", 
        palette = "Blues",  
        mapping = c(min(0), mean(range(21113)), max(130000)),
        title="Population Counts (SVM Predictions)", 
        title.legend="Population Count",
        fontsize.title = 14 
)

########################################################################

### average age
##treemap for observed
treemap(avrg,
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        #sortID = "HDB_pop",
        vSize = "area", 
        vColor = "avrg",
        type="value", 
        palette = "Blues", 
        mapping = c(min(28), mean(range(39.4)), max(52)),
        range = c(28,52),
        n = 10,
        title="Average Age (Observed)", 
        title.legend="Average Age",
        fontsize.title = 14 
)

##treemap for LM
treemap(avrg, 
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        #sortID = "HDB_pop",
        vSize = "area",  
        vColor = "estimations_lm",
        type="value", 
        palette = "Blues",  
        mapping = c(min(28), mean(range(39.4)), max(52)),
        range = c(28,52),
        n = 10,
        title="Average Age (LM)", 
        title.legend="Average Age",
        fontsize.title = 14 
)

##treemap for SVM
treemap(avrg, 
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        #sortID = "PA",
        vSize = "area",  
        vColor = "estimations_svm",
        type="value", 
        palette = "Blues",  
        mapping = c(min(28), mean(range(39.1)), max(52)),
        range = c(28,52),
        n = 10,
        title="Average Age (SVM)", 
        title.legend="Average Age",
        fontsize.title = 14 
)

########################################################################

### average age
##treemap for observed
treemap(elder,
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        #sortID = "HDB_pop",
        vSize = "area", 
        vColor = "elder",
        type="value", 
        palette = "Blues", 
        mapping = c(min(0.03), mean(range(0.17)), max(0.35)),
        range=c(0.03,0.35),
        n = 6,
        title="Elderly Proportion (Observed)", 
        title.legend="Elderly Proportion",
        fontsize.title = 14 
)

##treemap for LM
treemap(elder, 
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        #sortID = "HDB_pop",
        vSize = "area",  
        vColor = "estimations_lm",
        type="value", 
        palette = "Blues",  
        mapping = c(min(0.03), mean(range(0.17)), max(0.35)),
        range=c(0.03,0.35),
        n = 6,
        title="Elderly Proportion (LM)", 
        title.legend="Elderly Proportion",
        fontsize.title = 14 
)

##treemap for SVM
treemap(elder, 
        index=c("PA","SZ"),  
        algorithm = "pivotSize",
        #sortID = "PA",
        vSize = "area",  
        vColor = "estimations_rf",
        type="value", 
        palette = "Blues",  
        mapping = c(min(0.03), mean(range(0.17)), max(0.35)),
        range=c(0.03,0.35),
        n = 6,
        title="Elderly Proportion (RF)", 
        title.legend="Elderly Proportion",
        fontsize.title = 14 
)


