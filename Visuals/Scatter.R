######################################## VISUALISING THE RESULTS #############################################
### SCATTERPLOTS
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(ggplot2)
library(ggpubr)

##set the working directory and check the files within
setwd("//../")
list.files()

##read the files
pop <- read.csv("final_pop.csv", stringsAsFactors = FALSE)
avrg <- read.csv("final_avrg.csv", stringsAsFactors = FALSE)
elder <- read.csv("final_elder.csv", stringsAsFactors = FALSE)

########################################################################

### population count
##scatterplot for LM
pop1 <- ggplot(pop, aes(x=estimations_lm, y=HDB_pop)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted LM", breaks = seq(from = 0, to = 125000, by = 25000),
                   limits=c(0,135000)) +
  scale_y_continuous(name ="Observed", breaks = seq(from = 0, to = 125000, by = 25000),
                   limits=c(0,135000)) +
  labs(title = "Population Estimation",
       subtitle = "LM without model tuning")+
  stat_cor(label.x = 15000, label.y = 125000, size = 3)

##scatterplot for SVM
pop2 <- ggplot(pop, aes(x=estimations_svm, y=HDB_pop)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted SVM", breaks = seq(from = 0, to = 125000, by = 25000),
                     limits=c(0,135000)) +
  scale_y_continuous(name ="Observed", breaks = seq(from = 0, to = 125000, by = 25000),
                     limits=c(0,135000)) +
  labs(title = "",
    subtitle = "SVM with feature combination")+
  stat_cor(label.x = 15000, label.y = 125000, size = 3)

##scatterplot of SVM vs. LM
pop3 <- ggplot(pop, aes(x=estimations_svm, y=estimations_lm)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted SVM", breaks = seq(from = 0, to = 125000, by = 25000),
                     limits=c(0,135000)) +
  scale_y_continuous(name ="Predicted LM", breaks = seq(from = 0, to = 125000, by = 25000),
                     limits=c(0,135000)) +
  labs(title = "",
    subtitle = "SVM vs. LM")+
  stat_cor(label.x = 15000, label.y = 125000, size = 3)

########################################################################

### average age
##scatterplot for LM
avrg1 <- ggplot(avrg, aes(x=estimations_lm, y=avrg)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted LM", breaks = seq(from = 30, to = 50, by = 5),
                    limits=c(28,52)) +
  scale_y_continuous(name ="Observed", breaks = seq(from = 30, to = 50, by = 5),
                     limits=c(28,52)) +
  labs(title = "Average Age Estimation", 
       subtitle = "LM without model tuning") +
  stat_cor(label.x = 30, label.y = 50, size = 3)

##scatterplot for SVM
avrg2 <- ggplot(avrg, aes(x=estimations_svm, y=avrg)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted SVM", breaks = seq(from = 30, to = 50, by = 5),
                     limits=c(28,52)) +
  scale_y_continuous(name ="Observed", breaks = seq(from = 30, to = 50, by = 5),
                     limits=c(28,52)) +
  labs(title ="",
       subtitle = "SVM with feature combination")+
  stat_cor(label.x = 30, label.y = 50, size = 3)

##scatterplot SVM vs. LM
avrg3 <- ggplot(avrg, aes(x=estimations_svm, y=estimations_lm)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted SVM", breaks = seq(from = 30, to = 50, by = 5),
                     limits=c(28,52)) +
  scale_y_continuous(name ="Predicted LM", breaks = seq(from = 30, to = 50, by = 5),
                     limits=c(28,52)) +
  labs(title ="",
       subtitle = "SVM vs. LM") +
  stat_cor(label.x = 30, label.y = 50, size = 3)

########################################################################

### elderly
##scatterplot for LM
elder1 <- ggplot(elder, aes(x=estimations_lm, y=elder)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted LM", breaks = seq(from = 0, to = 0.35, by = 0.05),
                    limits=c(0,0.35)) +
  scale_y_continuous(name ="Observed", breaks = seq(from = 0, to = 0.35, by = 0.05),
                     limits=c(0,0.35)) +
  labs(title = "Elderly Proportion Estimation",
       subtitle = "LM without model tuning") +
  stat_cor(label.x = 0.025, label.y = 0.32, size = 3)

##scatterplot for RF
elder2 <- ggplot(elder, aes(x=estimations_rf, y=elder)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted RF", breaks = seq(from = 0, to = 0.35, by = 0.05),
                     limits=c(0,0.35)) +
  scale_y_continuous(name ="Observed", breaks = seq(from = 0, to = 0.35, by = 0.05),
                     limits=c(0,0.35)) +
  labs(title ="",
       subtitle = "RF with feature combination") +
  stat_cor(label.x = 0.025, label.y = 0.32, size = 3)

##scatterplot RF vs. LM
elder3 <- ggplot(elder, aes(x=estimations_rf, y=estimations_lm)) + 
  geom_point()+
  geom_abline(slope = 1) +
  scale_x_continuous(name ="Predicted RF", breaks = seq(from = 0, to = 0.35, by = 0.05),
                     limits=c(0,0.35)) +
  scale_y_continuous(name ="Predicted LM", breaks = seq(from = 0, to = 0.35, by = 0.05),
                     limits=c(0,0.35)) +
  labs(title ="",
       subtitle = "RF vs. LM")+
  stat_cor(label.x = 0.025, label.y = 0.32, size = 3)

########################################################################

##combine the plots
ggarrange(pop1, pop2, pop3, avrg1, avrg2, avrg3, elder1, elder2, elder3, nrow = 3, ncol = 3)
