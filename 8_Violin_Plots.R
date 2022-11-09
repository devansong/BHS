##################################################################
##################################################################
######Survival analyses for BHS Laliberte et al###################
####Code by Anne Devan-Song. Bend, OR. 2022#######################
#############Violin plots#########################################
##################################################################

rm(list=ls())
graphics.off()
library(tidyverse)
library("stringr")  
library(ggbeeswarm)
library(ggpubr)

df <-read.csv("fulldataset.csv")
lam <- subset(df, LambTime>0)

lambBHB <- ggplot(data = lam, aes(x = LambDelta, y = X.HBA.adj, fill = LambDelta))+
  scale_fill_manual(values = c("#bc5090", "#492539")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.7
                               ,show.legend = F)+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.8, alpha = 0,show.legend = F)+
  ylab(expression(paste("Ewe ", beta, "-HBA (mg/dL)")))+
  xlab("Lamb Survival") + 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ylim(0, 7)+
  ggtitle("B")
lambBHB


df <-read.csv("fulldataset.csv")
adu <- subset(df, AdultTime>0)
adu <- subset(adu, Notes != "capture related mort - don't include")

aduBHB <- ggplot(data = adu, aes(x = AdultDelta, y = X.HBA.adj, fill = AdultDelta))+
  scale_fill_manual(values = c("#003f5c", "#0f1d27")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.9
                               ,show.legend = F)+
  geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=0.7, alpha = 0,show.legend = F)+
  ylab(expression(paste(beta, "-HBA (mg/dL)")))+
  xlab("Adult Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ylim(0, 7)+
  ggtitle("A")
aduBHB

png("sig_covariates.png", units="in", width=6, height=4, res=300)
ggarrange(aduBHB, lambBHB,ncol = 2)
dev.off()


P <- ggplot(data = adu, aes(x = AdultDelta, y = P, fill = AdultDelta))+
  scale_fill_manual(values = c("#003f5c", "#0f1d27")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.9
                               ,show.legend = F)+
  geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=0.7, alpha = 0,show.legend = F)+
  ylab("P (mg/dL)")+ 
  xlab("Adult Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ggtitle("")
P

png("Adult_P_violin.png", units="in", width=3, height=3.5, res=300)
P
dev.off()


lambMO <- ggplot(data = lam, aes(x = LambDelta, y = Movi., fill = LambDelta))+
  scale_fill_manual(values = c("#bc5090", "#492539")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.7, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.8
                               ,show.legend = F, groupOnX = TRUE)+
  scale_y_continuous(breaks = round(seq(min(0), max(1), by = 1),1), limits=c(0,1))+
  ylab("Lamb Mycoplasma")+ 
  xlab("Lamb Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ggtitle("B")

lambMO

lambK <- ggplot(data = lam, aes(x = LambDelta, y = K, fill = LambDelta))+
  scale_fill_manual(values = c("#bc5090", "#492539")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.7
                               ,show.legend = F)+
  geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=0.5, alpha = 0,show.legend = F)+
  ylab("Ewe K (mEq/L)")+ 
  xlab("Lamb Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ggtitle("A")

lambK


aduBHB <- ggplot(data = adu, aes(x = AdultDelta, y = X.HBA.adj, fill = AdultDelta))+
  scale_fill_manual(values = c("#003f5c", "#0f1d27")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.9
                               ,show.legend = F)+
  geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=0.7, alpha = 0,show.legend = F)+
  ylab(expression(paste(beta, "-HBA (mg/dL)")))+
  xlab("Adult Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
  ylim(0, 6.5)+
  ggtitle("C")
aduBHB


aduTCO <- ggplot(data = adu, aes(x = AdultDelta, y = TCO, fill = AdultDelta))+
  scale_fill_manual(values = c("#003f5c", "#0f1d27")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.9
                               ,show.legend = F)+
  geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=0.7, alpha = 0,show.legend = F)+
  ylab("TCO (mEq/L)")+ 
  xlab("Adult Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
 ggtitle("F")
aduTCO

aducreat <- ggplot(data = adu, aes(x = AdultDelta, y = creatinine, fill = AdultDelta))+
  scale_fill_manual(values = c("#003f5c", "#0f1d27")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.9
                               ,show.legend = F)+
  geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=0.7, alpha = 0,show.legend = F)+
  ylab("Creatinine (mg/dL)")+ 
  xlab("Adult Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
 ggtitle("D")
aducreat


aduCl <- ggplot(data = adu, aes(x = AdultDelta, y = Cl, fill = AdultDelta))+
  scale_fill_manual(values = c("#003f5c", "#0f1d27")) +
  scale_x_discrete(labels=c("Yes", "No"))+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA, show.legend=F) +
  ggbeeswarm::geom_quasirandom(shape = 21,size=1.4, dodge.width = .75, color="black",alpha=0.9
                               ,show.legend = F)+
  geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=0.7, alpha = 0,show.legend = F)+
  ylab("Cl (mEq/L)")+ 
  xlab("Adult Survival")+ 
  theme(strip.background =  element_rect(fill = NA, colour = NA), 
        panel.background =  element_rect(fill = "white"), 
        panel.border =      element_rect(fill = NA, colour="black"), 
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank())+
 ggtitle("E")
aduCl

png("NON_sig_covariates.png", units="in", width=9, height=8, res=300)
ggarrange(lambK, aduBHB, aduCl, lambMO, aducreat, aduTCO, ncols=3)
dev.off()




