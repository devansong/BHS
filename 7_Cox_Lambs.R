##################################################################
##################################################################
######Survival analyses for bighorn sheep for Connor##############
####Code by Anne Devan-Song. Bend, OR. 2022#######################
#############Coxph for lambs#####################################
##################################################################
rm(list=ls())
graphics.off()
library(ggplot2)
library(devtools)
library(survival)
library(rankhazard)
library(splines)
library(smoothHR)

df <-read.csv("fulldataset.csv")
df <- subset(df, LambTime >0)

survival.vector <- Surv(df$LambTime, df$LambDelta)

df$age <- as.numeric(as.character(df$age))

## Create vectors for outcome and predictors
outcome    <- c("survival.vector")
predictors <- c("LambPop", "age", "Movi.","X.HBA.adj", "K")

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

write.csv(summ, file = "model.selection.lambs.csv", row.names=FALSE)

df <-read.csv("fulldataset.csv")
df$age <- as.numeric(df$age)
dflamb <- subset(df, LambTime >0)

my.surv.lamb <- Surv(dflamb$LambTime, dflamb$LambDelta)
survfit(my.surv.lamb  ~ 1)
my.fit.lamb <- survfit(my.surv.lamb~ 1)
coxph.fit <- coxph(my.surv.lamb ~ 
                     dflamb$X.HBA.adj+
                     dflamb$Movi.+
                     dflamb$K+
                     dflamb$age+
                     dflamb$totneu+
                     dflamb$pop,
                     method="efron")
coxph.fit
summary(coxph.fit)
coxph.fit
summary <- summary(coxph.fit)
summary$waldtest
log <- as.data.frame(summary$logtest)
wald <- as.data.frame(summary$waldtest)
logrank <- as.data.frame(summary$sctest)
overall <- cbind(log, wald, logrank)
write.csv(overall, file="lamb.overall.results.csv", row.names=TRUE)
coef <- as.data.frame(summary$coefficients)
write.csv(coef, file="lamb.coxph.results.csv", row.names=TRUE)

### repeat same ting but remove HBA outlier 
setwd("~/Desktop/Connor_Sheep_Survival/data")
df <-read.csv("fulldataset.csv")
df$age <- as.numeric(df$age)
dflamb <- subset(df, LambTime >0)
dflamb <- subset(dflamb, Capture.ID !="Y7-0217")

my.surv.lamb <- Surv(dflamb$LambTime, dflamb$LambDelta)
survfit(my.surv.lamb  ~ 1)
my.fit.lamb <- survfit(my.surv.lamb~ 1)
coxph.fit <- coxph(my.surv.lamb ~ 
                     dflamb$age + 
                     dflamb$X.HBA.adj+
                     dflamb$Movi.+
                     dflamb$K+
                     dflamb$pop,method="efron")
coxph.fit
summary(coxph.fit)
coxph.fit
summary <- summary(coxph.fit)
summary$waldtest
log <- as.data.frame(summary$logtest)
wald <- as.data.frame(summary$waldtest)
logrank <- as.data.frame(summary$sctest)
overall <- cbind(log, wald, logrank)
write.csv(overall, file="lamb.overall.results_WITHOUTBHBOUTLIER.csv", row.names=TRUE)
coef <- as.data.frame(summary$coefficients)
write.csv(coef, file="lamb.coxph.results_WITHOUTBHBOUTLIER.csv", row.names=TRUE)


coxph.fit <- coxph(my.surv.lamb ~ 
                     age + 
                     pop +
                   X.HBA.adj+
                     Movi.,
                   dflamb,
                   x=TRUE)

rankhazardplot(coxph.fit, data=dflamb)

cox.zph(coxph.fit)

colpal <- c("#bc5090", "#e5a1c8", "#f1dde7")

hr.plot <- smoothHR(data=df, coxfit=coxph.fit)

setwd("~/Dropbox/CurrentProjects/Connor_Sheep_Survival/figures")
png("PropHazLamb.png", units="in", width=4, height=4, res=300)
plot(hr.plot, predictor="X.HBA.adj", prob=0, conf.level=0.95, col= colpal, xlab=expression(paste("Ewe ", beta, "-HBA (mg/dL)")), ylab="Ln Hazard Ratio (Z, Zref)", ref.label = "Min", main="", lwd=1)
dev.off()



