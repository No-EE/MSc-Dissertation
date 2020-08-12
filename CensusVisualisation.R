######################################## ANALYSIS OF DEMOGRAPHIC DISTRIBUTION ################################
### Analysis of demographic distribution of age groups
##############################################################################################################

##CLEAR R MEMORY
rm(list = ls())

##call packages
library(ggplot2)


##set the working directory and check the files within
setwd("//../")
list.files()

PASZ <- read.csv("Prepared_2019_Bedok.csv", stringsAsFactors = FALSE)

#overall relative distribution within the PA
barplot(PASZ$RelToTot[PASZ$Subzone=="Total" & PASZ$Type.of.Dwelling=="Total"])

################################################################################

##create barchartz in ggplot
#barchart of age distribution within PA, just a standard overview
PA<-ggplot(data=subset(PASZ, PASZ$Subzone=="Total" & PASZ$Type.of.Dwelling=="Total" & PASZ$Age.Group!="Total"), 
           aes(x=Age.Group, y=RelToTot, fill=Age.Group)) +
  geom_bar(stat="identity", width=0.9) +
  geom_text(aes(label=round(RelToTot,2)), vjust=1.6, size=4) +
  #ylim(0,0.32) +
  xlab("\nAge Group") + 
  ylab("Relative Proportion\n") +
  theme_minimal()

PA +
  scale_fill_manual(values=c("#ccebc5", "#7bccc4", "#43a2ca")) +
  theme(legend.position="none",
        axis.line = element_line(colour = "darkgrey", size = 0.5, linetype = "solid")) +
  labs(title="\nAge Group Distribution in 2019",
       subtitle="For SG Planning Area \"Bedok\"",
       caption="Noée Szarka | UoE, School of GeoSciences | NUS, Urban Analytics Lab (ual.sg)") +
  scale_x_discrete(limits=c("0-14", "15-64", "65+"))

################################################################################

##AGE DISTRIBUTION
##stacked barchart of distribution of age groups in PA
PA<-ggplot(data=subset(PASZ, PASZ$Type.of.Dwelling=="Total" & PASZ$Age.Group!="Total"), 
           aes(x=Subzone, y=RelToTot, fill=Age.Group)) +
  geom_bar(stat="identity", width=0.9, position = position_fill(reverse = TRUE)) +
  ylim(0,1.05) +
  xlab("\nSubzone") + 
  ylab("Relative Proportion\n") +
  theme_minimal()

PA +
  scale_fill_manual(values=c("#ccebc5", "#a8ddb5", "#43a2ca")) +
  theme(legend.position="top",
        axis.line = element_line(colour = "darkgrey", size = 0.5, linetype = "solid"),
        axis.text.y = element_text(face="bold")) +
  labs(title="\nAge Group Distribution in 2019",
       fill = "Age Group",
       subtitle="For SG Planning Area \"Bedok\"",
       caption="Noée Szarka | UoE, School of GeoSciences | NUS, Urban Analytics Lab (ual.sg)") +
  scale_x_discrete(limits=c("Siglap", "Kembangan", "Kaki Bukit", "Frankel", "Bedok South", "Bedok Reservoir", 
                            "Bedok North", "Bayshore", "Total")) +
  coord_flip() 

################################################################################

##GENDER DISTRIBUTION

##stacked barchart of gender
##new Dataframe which allows to compare between the two
Gen <- rbind(
  data.frame(PASZ$Subzone, PASZ$Age.Group, PASZ$Type.of.Dwelling, "count" = PASZ$RelMale, "type"="male"),
  data.frame(PASZ$Subzone, PASZ$Age.Group, PASZ$Type.of.Dwelling, "count" = PASZ$RelFemale, "type"="female")
)

PA<-ggplot(data=subset(Gen, Gen$PASZ.Type.of.Dwelling=="Total" & Gen$PASZ.Age.Group=="Total"), 
           aes(x=PASZ.Subzone, y=count, fill=type)) +
  geom_bar(stat="identity", width=0.9, position = position_fill(reverse = TRUE)) +
  xlab("\nSubzone") + 
  ylab("Relative Proportion\n") +
  theme_minimal()

PA +
  scale_fill_manual(values=c("#43a2ca", "#a8ddb5")) +
  theme(legend.position="top",
        axis.line = element_line(colour = "darkgrey", size = 0.5, linetype = "solid"),
        axis.text.y = element_text(face="bold")) +
  labs(title="\nGender Distribution in 2019",
       fill = "Gender",
       subtitle="For SG Planning Area \"Bedok\"",
       caption="Noée Szarka | UoE, School of GeoSciences | NUS, Urban Analytics Lab (ual.sg)") +
  geom_hline(yintercept=0.50, size=0.5) +
  scale_x_discrete(limits=c("Siglap", "Kembangan", "Kaki Bukit", "Frankel", "Bedok South", "Bedok Reservoir", 
                            "Bedok North", "Bayshore", "Total")) +
  coord_flip() 


################################################################################

##PROPERTY TYPE DISTRIBUTION

##reassign the values for logical visualisation of data
#reassign the NA column to new classes
PASZ$FlatClass<-NA

PASZ$FlatClass[PASZ$Type.of.Dwelling=="Total HDB"]<-"A" 
PASZ$FlatClass[PASZ$Type.of.Dwelling=="Landed Properties"]<-"B"
PASZ$FlatClass[PASZ$Type.of.Dwelling=="Condominiums and Other Apartments"]<-"C" 
PASZ$FlatClass[PASZ$Type.of.Dwelling=="Others"]<-"D"


##stacked barchart of distribution of property types
PA<-ggplot(data=subset(PASZ, PASZ$Type.of.Dwelling!="Total" &
                         PASZ$Type.of.Dwelling!="1- and 2-Room Flats" &
                         PASZ$Type.of.Dwelling!="3-Room Flats" &
                         PASZ$Type.of.Dwelling!="4-Room Flats" &
                         PASZ$Type.of.Dwelling!="5-Room and Executive Flats" &
                         PASZ$Age.Group=="Total"), 
           aes(x=Subzone, y=RelCount, fill=FlatClass)) +
  geom_bar(stat="identity", width=0.9, position = position_fill(reverse = TRUE)) +
  xlab("\nSubzone") + 
  ylab("Relative Proportion\n") +
  theme_minimal()

PA +
  scale_fill_manual(values=c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4"),
                    labels=c("HDB", "Landed", "Condo|Other Apartments", "Others")) +
  theme(legend.position="top",
        axis.line = element_line(colour = "darkgrey", size = 0.5, linetype = "solid"),
        axis.text.y = element_text(face="bold")) +
  labs(title="\nProperty Type Distribution in 2019",
       fill = "Property Type",
       subtitle="For SG Planning Area \"Bedok\"",
       caption="Noée Szarka | UoE, School of GeoSciences | NUS, Urban Analytics Lab (ual.sg)") +
  scale_x_discrete(limits=c("Siglap", "Kembangan", "Kaki Bukit", "Frankel", "Bedok South", "Bedok Reservoir", 
                            "Bedok North", "Bayshore", "Total")) +
  coord_flip() 

################################################################################

##FLAT TYPE DISTRIBUTION

##reassign the values for logical visualisation of data
#reassign the NA column to new classes
PASZ$RoomClass<-NA

PASZ$RoomClass[PASZ$Type.of.Dwelling=="1- and 2-Room Flats"]<-"A" 
PASZ$RoomClass[PASZ$Type.of.Dwelling=="3-Room Flats"]<-"B"
PASZ$RoomClass[PASZ$Type.of.Dwelling=="4-Room Flats"]<-"C" 
PASZ$RoomClass[PASZ$Type.of.Dwelling=="5-Room and Executive Flats"]<-"D"

##stacked barchart of distribution of flat types
PA<-ggplot(data=subset(PASZ, PASZ$Type.of.Dwelling!="Total" &
                         PASZ$Type.of.Dwelling!="Total HDB" &
                         PASZ$Type.of.Dwelling!="Condominiums and Other Apartments" &
                         PASZ$Type.of.Dwelling!="Landed Properties" &
                         PASZ$Type.of.Dwelling!="Others" &
                         PASZ$Age.Group=="Total"), 
           aes(x=Subzone, y=RelCount, fill=RoomClass)) +
  geom_bar(stat="identity", width=0.9, position = position_fill(reverse = TRUE)) +
  xlab("\nSubzone") + 
  ylab("Relative Proportion\n") +
  theme_minimal()

PA +
  scale_fill_manual(values=c("#ccebc5", "#a8ddb5", "#7bccc4", "#43a2ca"),
                    labels=c("1- and 2-room", "3-room", "4-room", "5-room and Executive")) +
  theme(legend.position="top",
        axis.line = element_line(colour = "darkgrey", size = 0.5, linetype = "solid"),
        axis.text.y = element_text(face="bold")) +
  labs(title="\nFlat Type Distribution in 2019 (HDB only)",
       fill = "Flat Type",
       subtitle="For SG Planning Area \"Bedok\"",
       caption="Noée Szarka | UoE, School of GeoSciences | NUS, Urban Analytics Lab (ual.sg)") +
  scale_x_discrete(limits=c("Kembangan", "Kaki Bukit", "Bedok South", "Bedok Reservoir", 
                            "Bedok North", "Total")) +
  coord_flip() 

################################################################################

