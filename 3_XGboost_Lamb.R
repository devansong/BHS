##################################################################
##################################################################
######Survival analyses for BHS Laliberte et al###################
####Code by Anne Devan-Song. Bend, OR. 2022#######################
#######Code for lamb xgboost######################################
##################################################################

graphics.off()
rm(list = ls()) # clear working directory
library(xgboost)
library(caret)
library(fastDummies)
library(DiagrammeR)
library(Ckmeans.1d.dp) 

data <- read.csv("fulldataset.csv") #load csv of chem-survival tab 
str(data) #examine data; make sure things that are numeric are numeric 


data$age <- as.numeric(data$age) # run this line if age is wonky 
survival <- subset(data, LambDelta == "FALSE" | LambDelta == "TRUE" ) #subset data which has #survival info. 134 observations.
subdf <- survival[, c("LambDelta", 
                      "LambPop", 
                      "age", 
                      "Movi.",
                      "NEFAs.adj",
                      "X.HBA.adj",
                      "creatine.kinase",
                      "glucose",
                      "cholesterol", 
                      "triglycerides",
                      "albumin",
                      "GGT", 
                      "Na",
                      "K",
                      "Cl",
                      "P",
                      "Mg", 
                      "total.WBC",
                      "totneu",
                      "totlym",
                      "totmon",
                      "toteos",
                      "totbas",
                      "prop.E.coli.killed..1.128.",
                      "LPS.prop.activation"
)]

subdf <- dummy_cols(subdf, select_columns = 'LambPop') #create dummy variables for population 
subdf <-subdf[, !(colnames(subdf) %in% c("LambPop"))] #remove pop.y column 
df <- subdf

for(i in c(1:ncol(df))){
  df[,i] <- as.numeric(df[,i]) #make all data points numeric
}
m <- as.matrix(df) #convert df into a matrix for use in XGBoost

indices <- sample(1:nrow(df), size = 0.75 * nrow(df)) #randomly divide df into two groups
train <- m[indices,] #this is the subset of data you will train the model on  
test <- m[-indices,] #this is the subset of data you will try to predict with your model
m1_xgb <-
  xgboost(
    data = train[, 2:ncol(df)],
    label = train[, 1],
    nrounds = 10,
    objective = "binary:logistic",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )
importance_matrix <- xgb.importance(model = m1_xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
###################################################################

newdf <- data.frame("Feature"  = numeric(0),
                   "Gain"  = numeric(0),
                   "Cover" = numeric(0),
                   "Frequency" = numeric(0),
                   "Importance"  = numeric(0),
                   "Iteration" = numeric(0),
                   stringsAsFactors=FALSE) 


for (i in c(1:1000)) {
  print(i)
  indices <- sample(1:nrow(df), size = 0.75 * nrow(df))
  train <- m[indices,] #this is the subset of data you will train the model on  
  test <- m[-indices,] #this is the subset of data you will try to predict with your model
  
  m1_xgb <-
    xgboost(
      data = train[, 2:20],
      label = train[, 1],
      nrounds = 10,
      objective = "binary:logistic",
      early_stopping_rounds = 3,
      max_depth = 6,
      eta = .25
    )
  
  importance_matrix <- xgb.importance(model = m1_xgb)
  immpy <-xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
  importance_matrix$Iteration <- i
  newdf <- rbind(newdf, importance_matrix) #this line combines your 1000 results together
}

#save your newdf as a csv in your folder 
write.csv(newdf, file="xgboost_results1000_lamb.csv")
data <- read.csv("xgboost_results1000_lamb.csv") #load csv of xgboost results that you saved earlier
str(data)

listofvariables <-unique(data$Feature)
listofvariables

newdf <- data.frame("Feature" = numeric(0),
                    "Average" = numeric(0), 
                    "SD"= numeric(0), 
                    "SE" = numeric (0))


for (i in listofvariables){
  df <- subset(data, Feature== i) #subset the data by feature, e.g. "totneu" 
  samplesize <- nrow(df) #calculate the number of rows, should be between 0-1000
  startrow <- samplesize+1 
  zerorow <- c("extra", i, 0, 0, 0, 0, 0) #create a template row
  zerodf <- as.data.frame(t(zerorow)) 
  zerodf <- zerodf[rep(seq_len(nrow(zerodf)), each = (1000-nrow(df))), ] #supplement the dataframe 
  #so it has 1000 rows, with 0 in the rows where the variable did not turn up in the model 
  colnames(zerodf) <- colnames(df) #rename the df
  tempdf <- rbind(df, zerodf) #bind the subsetted df and the remainder dfs with the zeros
  tempdf$Importance <- as.numeric(tempdf$Importance)
  average <- mean(tempdf$Importance) #calculate average importance
  sd <- sd(tempdf$Importance) 
  se <- sd/(sqrt(nrow(tempdf))) #this measures standard error, which is SD/(squareroot(samplesize))
  combine <-c(i, average, sd, se) 
  row <- as.data.frame(t(combine))
  newdf <-rbind(newdf, row) # this adds everything to the new dataframe 
}

colnames(newdf) <- c("Feature", "Average.Importance", "SD", "SE")
#Examine newdf and see if it makes sense 

write.csv(newdf, file="summary_xgboost_1000_lamb.csv", row.names=FALSE) #save your new df as a csv
