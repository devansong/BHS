##################################################################
##################################################################
######Survival analyses for BHS Laliberte et al###################
####Code by Anne Devan-Song. Bend, OR. 2022#######################
####PLot XGBoost results for adults and lambs#####################
##################################################################

graphics.off()
rm(list = ls()) # clear working directory
library(tidyverse)
library(ggpubr)

setwd("~/Dropbox/CurrentProjects/Connor_Sheep_Survival/data") 

summary <- read.csv("summary_xgboost_1000_adult.csv")
str(summary) #everything besides "Feature" should be numeric. Change if not 
#reorder the data in decreasing order by Average importance
summary <- summary[order(summary$Average.Importance, decreasing = TRUE), ]
summary$dem <- "Adult"
summarylamb <- read.csv("summary_xgboost_1000_lamb.csv")
summarylamb <- summarylamb[order(summarylamb$Average.Importance, decreasing = TRUE), ]
summarylamb$dem <- "Lamb"


adult <- ggplot(summary, aes(reorder(Feature, Average.Importance), 
                            Average.Importance), fill=Feature) + 
  geom_bar(stat="identity", color="white", fill="#003f5c",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Average.Importance-SE, ymax=Average.Importance+SE), width=0.7,
                position=position_dodge(.9))+
  geom_hline(yintercept = 0.05, linetype="dashed",
             color = "grey", size=0.9) +
  coord_flip()+
  theme_classic()+
  labs(y = "Average Importance for Survival", x ="Feature")+
  ggtitle("Adults")

adult

lamb <- ggplot(summarylamb, aes(reorder(Feature, Average.Importance), 
                            Average.Importance), fill=Feature) + 
  geom_bar(stat="identity", color="white", fill="#bc5090",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Average.Importance-SE, ymax=Average.Importance+SE), width=0.7,
                position=position_dodge(.9))+
  coord_flip()+
  theme_classic()+
  geom_hline(yintercept = 0.05, linetype="dashed",
             color = "grey", size=0.9) +
  labs(y = "Average Importance for Survival", x ="Feature")+
  ggtitle("Lambs")

lamb

ggarrange(adult, lamb)

png("XGBOOST_survival.png", units="in", width=9, height=5, res=300) #this sets dimensions of image
ggarrange(adult, lamb)
dev.off()








