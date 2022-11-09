##################################################################
##################################################################
######Survival analyses for BHS Laliberte et al###################
####Code by Anne Devan-Song. Bend, OR. 2022#######################
#######Code to plot Survival Curve################################
##################################################################

rm(list=ls())
graphics.off()
library(ggplot2)
library(devtools)
library(survival)

df <-read.csv("fulldataset.csv")

adult <- df[, c("Capture.ID",
                "pop",
                "AdultDelta", 
                "AdultTime")]
adult <- subset(adult, AdultTime >0)
colnames(adult) <- c("ID", "pop", "delta", "time")
adult$group <- "Adult"

lamb <- df[, c("Capture.ID", 
               "LambPop", 
               "LambDelta",
               "LambTime")]
lamb <- subset(lamb, LambTime>0)
colnames(lamb) <- c("ID", "pop", "delta", "time")
lamb$group <- "Lamb"

comb <- rbind(adult, lamb)

my.surv <- Surv(comb$time, comb$delta)
my.fit <- survfit(my.surv~ group, data=comb)

png("Lamb_Adult_KM_Curve.png", units="in", width=5.5, height=4.5, res=300)
ggsurvplot(my.fit, data = comb, 
           palette = c("#003f5c", "#bc5090"), 
           conf.int = TRUE,          # Add confidence interval
           risk.table = TRUE,
           legend.labs =
             c("Adult", "Lamb"),
           legend.title="", 
           xlab = "Observation Period",   # customize X axis label.
           ylab = "Survival Probability",
           risk.table.y.text.col = T,# colour risk table text annotations.
           risk.table.height = 0.3, # the height of the risk table
           risk.table.y.text = FALSE, 
           risk.table.xlab = "")
dev.off()

