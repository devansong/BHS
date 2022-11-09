##################################################################
##################################################################
######Survival analyses for BHS Laliberte et al###################
####Code by Anne Devan-Song. Bend, OR. 2022#######################
####PLot corelation matrix of selected  c###################
##################################################################
graphics.off()
rm(list = ls()) # clear working directory
library(ggplot2)
library("ggcorrplot")
data <- read.csv("fulldataset.csv")

# Compute a correlation matrix
data$age <- as.numeric(data$age)
subdf <- data[, c("age", 
                  "X.HBA.adj",
                  "P",
                  "TCO",
                  "creatinine",
                  "Cl",
                  "K", 
                  "cholesterol",
                  "totneu",
                  "totmon")]
                
colnames(subdf) <- c("Age", 
                     "β-HBA",                     
                     "P (Phosphorus)",
                     "TCO (Total CO2)",
                     "Creatinine",
                     "Cl (Chloride)",
                     "K (Potassium)", 
                     "Cholesterol",
                     "Neutrophils",
                     "Monocytes")
            

for(i in c(1:ncol(subdf))){
  subdf[,i] <- as.numeric(subdf[,i]) #make all data points numeric
}

df <-na.omit(subdf)
corr <- round(cor(df), 1)


png("Covariate_correlation.png", units="in", width=5, height=5, res=300) #this sets dimensions of image
ggcorrplot(corr, p.mat = cor_pmat(df),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)
dev.off()

