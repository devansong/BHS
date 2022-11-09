##################################################################
##################################################################
######Survival analyses for bighorn sheep for Connor##############
####Code by Anne Devan-Song. Bend, OR. 2022#######################
#############Coxph for adults#####################################
##################################################################


rm(list=ls())
graphics.off()
setwd("~/Dropbox/CurrentProjects/Connor_Sheep_Survival/data")

library(ggplot2)
library(devtools)
library(survival)
library(rankhazard)
library(splines)
library(smoothHR)

df <-read.csv("fulldataset.csv")
df <- subset(df, AdultTime >0)
nrow(subset(df, AdultDelta =="TRUE"))
df <- subset(df, Notes != "capture related mort - don't include")

df <- rbind(nolamb, deadlamb, livelamb, unklamb)
survival.vector <- Surv(df$AdultTime, df$AdultDelta)
df$age <- as.numeric(as.character(df$age))
## Create vectors for outcome and predictors
outcome    <- c("survival.vector")
predictors <- c("age", "pop", "sex",
                "TCO","Cl","creatinine", "X.HBA.adj")
dataset    <- df

## Create list of models
list.of.models <- lapply(seq_along((predictors)), function(n) {
  
  left.hand.side  <- outcome
  right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
  
  paste(left.hand.side, right.hand.side, sep = "  ~  ")
})
## Convert to a vector
vector.of.models <- unlist(list.of.models)
## Fit coxph to all models
list.of.fits <- lapply(vector.of.models, function(x) {
  formula    <- as.formula(x)
  fit        <- coxph(formula, data = dataset)
  result.AIC <- extractAIC(fit)
  
  data.frame(num.predictors = result.AIC[1],
             AIC            = result.AIC[2],
             model          = x)
})
## Collapse to a data frame
result <- do.call(rbind, list.of.fits)
## Sort and print
library(doBy)
summ <- orderBy(~ AIC, result)

write.csv(summ, file = "model.selection.adults_NO_P.csv", row.names=FALSE)

###Now do CoxPh with selected variables
df <-read.csv("fulldataset.csv")
df <- subset(df, AdultTime>0)
df <- subset(df, Notes != "capture related mort - don't include")

my.surv <- Surv(df$AdultTime, df$AdultDelta)
survfit(my.surv ~ 1)
my.fit <- survfit(my.surv~ 1)


df$age <- as.numeric(df$age)
coxph.fit <- coxph(my.surv ~ 
                     df$age + 
                     df$pop + #+ 
                     df$sex+
                     df$P+
                     df$creatinine+
                     df$X.HBA.adj,
                   method="efron")
coxph.fit
summary(coxph.fit)
summary <- summary(coxph.fit)
summary$waldtest
log <- as.data.frame(summary$logtest)
wald <- as.data.frame(summary$waldtest)
logrank <- as.data.frame(summary$sctest)
overall <- cbind(log, wald, logrank)
write.csv(overall, file="adult.overall.results_NOP.csv", row.names=TRUE)
coef <- as.data.frame(summary$coefficients)
write.csv(coef, file="adult.coxph.results_NOP.csv", row.names=TRUE)

coxph.fit <- coxph(my.surv ~  P+X.HBA.adj,df,x=TRUE)
rankhazardplot(coxph.fit, data=df)
cox.zph(coxph.fit)
hr.plot <- smoothHR(data=df, coxfit=coxph.fit)
colpal <- c("#003f5c", "#869eb3", "#d7e4f1")
png("PropHazPADULT.png", units="in", width=4, height=4.5, res=300)
plot(hr.plot, predictor="P", prob=0, conf.level=0.95, col= colpal, xlab="P (mg/dL)", ylab="Ln Hazard Ratio (Z, Zref)", ref.label = "Min", main="", lwd=1)
dev.off()
png("PropHazPADULT_BHB.png", units="in", width=4, height=4, res=300)
plot(hr.plot, predictor="X.HBA.adj", prob=0, conf.level=0.95, col= colpal, xlab=expression(paste(beta, "-HBA (mg/dL)")), ylab="Ln Hazard Ratio (Z, Zref)", ref.label = "Min", main="", lwd=1)
dev.off()


